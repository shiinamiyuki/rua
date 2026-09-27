//! io library: file I/O operations.

use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Seek, SeekFrom, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

use crate::closure::{Closure, NativeFn};
use crate::error::LuaError;
use crate::gc::{Gc, GcRef};
use crate::value::Value;

// ── LuaFile ────────────────────────────────────────────────────────

/// Internal file handle stored as userdata.
pub struct LuaFile {
    kind: FileKind,
    closable: bool,
    /// Bytes pushed back by `read("n")` scanning (used as a stack).
    pending: Vec<u8>,
    /// Buffered output for regular files (flushed by `flush`/`close`).
    write_buf: Vec<u8>,
    /// Whether the file was opened for writing.
    writable: bool,
    /// Output buffering mode (regular files only).
    bufmode: BufMode,
    /// Requested buffer size for `BufMode::Full` (0 = default).
    bufsize: usize,
}

#[derive(Clone, Copy, PartialEq)]
pub enum BufMode {
    Full,
    None,
    Line,
}

enum FileKind {
    Regular(BufReader<File>),
    Stdin,
    Stdout,
    Stderr,
    Closed,
    /// `io.popen(cmd, "r")`: read the child's stdout.
    ReadPipe(Child, BufReader<ChildStdout>),
    /// `io.popen(cmd, "w")`: write to the child's stdin.
    WritePipe(Child, ChildStdin),
}

impl LuaFile {
    pub fn stdin() -> Self {
        LuaFile { kind: FileKind::Stdin, closable: false, pending: Vec::new(), write_buf: Vec::new(), writable: false, bufmode: BufMode::Full, bufsize: 0 }
    }
    pub fn stdout() -> Self {
        LuaFile { kind: FileKind::Stdout, closable: false, pending: Vec::new(), write_buf: Vec::new(), writable: true, bufmode: BufMode::Full, bufsize: 0 }
    }
    pub fn stderr() -> Self {
        LuaFile { kind: FileKind::Stderr, closable: false, pending: Vec::new(), write_buf: Vec::new(), writable: true, bufmode: BufMode::Full, bufsize: 0 }
    }
    fn from_file(file: File) -> Self {
        LuaFile {
            kind: FileKind::Regular(BufReader::new(file)),
            closable: true,
            pending: Vec::new(),
            write_buf: Vec::new(),
            writable: true,
            bufmode: BufMode::Full,
            bufsize: 0,
        }
    }
    fn from_file_readonly(file: File) -> Self {
        let mut f = Self::from_file(file);
        f.writable = false;
        f
    }
    pub fn is_closed(&self) -> bool {
        matches!(self.kind, FileKind::Closed)
    }

    // ── Read operations ────────────────────────────────────────

    fn read_line(&mut self, keep_nl: bool) -> io::Result<Option<Vec<u8>>> {
        let mut buf = String::new();
        if !self.pending.is_empty() {
            let pending = std::mem::take(&mut self.pending);
            for b in pending {
                buf.push(b as char);
                if b == b'\n' {
                    return Ok(Some(buf.into_bytes()));
                }
            }
        }
        let n = match &mut self.kind {
            FileKind::Regular(r) => r.read_line(&mut buf)?,
            FileKind::Stdin => io::stdin().lock().read_line(&mut buf)?,
            FileKind::ReadPipe(_, r) => r.read_line(&mut buf)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "attempt to use a closed file")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        };
        if n == 0 && buf.is_empty() {
            return Ok(None); // EOF
        }
        if !keep_nl {
            if buf.ends_with('\n') { buf.pop(); }
            if buf.ends_with('\r') { buf.pop(); }
        }
        Ok(Some(buf.into_bytes()))
    }

    fn read_all(&mut self) -> io::Result<Vec<u8>> {
        let mut buf = Vec::new();
        buf.extend(self.pending.drain(..));
        match &mut self.kind {
            FileKind::Regular(r) => { r.read_to_end(&mut buf)?; }
            FileKind::Stdin => { io::stdin().lock().read_to_end(&mut buf)?; }
            FileKind::ReadPipe(_, r) => { r.read_to_end(&mut buf)?; }
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "attempt to use a closed file")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        }
        Ok(buf)
    }

    fn read_bytes(&mut self, n: usize) -> io::Result<Option<Vec<u8>>> {
        let mut buf = vec![0u8; n];
        let mut taken_from_pending = 0usize;
        while taken_from_pending < n {
            match self.pending.pop() {
                Some(b) => {
                    buf[taken_from_pending] = b;
                    taken_from_pending += 1;
                }
                None => break,
            }
        }
        if n == 0 {
            return Ok(None);
        }
        let mut bytes_read = 0usize;
        while taken_from_pending + bytes_read < n {
            let start = taken_from_pending + bytes_read;
            let got = match &mut self.kind {
                FileKind::Regular(r) => r.read(&mut buf[start..])?,
                FileKind::Stdin => io::stdin().lock().read(&mut buf[start..])?,
                FileKind::ReadPipe(_, r) => r.read(&mut buf[start..])?,
                FileKind::Closed => {
                    return Err(io::Error::new(io::ErrorKind::Other, "attempt to use a closed file"))
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "not open for reading",
                    ))
                }
            };
            if got == 0 {
                break;
            }
            bytes_read += got;
        }
        let total = bytes_read + taken_from_pending;
        if total == 0 {
            return Ok(None); // EOF
        }
        buf.truncate(total);
        Ok(Some(buf))
    }

    fn read_number(&mut self) -> io::Result<Option<Value>> {
        // Port of reference Lua's numeral scanner: read the longest prefix
        // that matches a numeral automaton, then validate it.
        const MAXRN: usize = 200;
        struct Scanner<'a> {
            file: &'a mut LuaFile,
            buff: Vec<u8>,
            c: Option<u8>,
            overflow: bool,
        }
        impl<'a> Scanner<'a> {
            fn nextc(&mut self) -> io::Result<bool> {
                if self.buff.len() >= MAXRN {
                    self.overflow = true;
                    return Ok(false);
                }
                if let Some(c) = self.c {
                    self.buff.push(c);
                }
                self.c = self.file.read_byte()?;
                Ok(true)
            }
            fn test2(&mut self, set: &[u8; 2]) -> io::Result<bool> {
                if self.c.map_or(false, |c| c == set[0] || c == set[1]) {
                    self.nextc()
                } else {
                    Ok(false)
                }
            }
            fn testc(&mut self, set: &[u8; 2]) -> io::Result<bool> {
                if self.c.map_or(false, |c| c == set[0] || c == set[1]) {
                    self.nextc()
                } else {
                    Ok(false)
                }
            }
            fn readdigits(&mut self, hex: bool) -> io::Result<usize> {
                let mut count = 0usize;
                while self
                    .c
                    .map_or(false, |c| if hex { c.is_ascii_hexdigit() } else { c.is_ascii_digit() })
                {
                    if !self.nextc()? {
                        break;
                    }
                    count += 1;
                }
                Ok(count)
            }
        }

        let first = loop {
            match self.read_byte()? {
                Some(b) if b.is_ascii_whitespace() => continue,
                other => break other,
            }
        };
        let mut sc = Scanner {
            file: self,
            buff: Vec::new(),
            c: first,
            overflow: false,
        };

        sc.test2(b"-+")?;
        let mut hex = false;
        let mut count = 0usize;
        if sc.test2(b"00")? {
            if sc.test2(b"xX")? {
                hex = true;
            } else {
                count = 1;
            }
        }
        count += sc.readdigits(hex)?;
        if sc.testc(b"..")? {
            count += sc.readdigits(hex)?;
        }
        if count > 0 && sc.testc(if hex { b"pP" } else { b"eE" })? {
            sc.test2(b"-+")?;
            sc.readdigits(false)?;
        }
        // Unread the look-ahead character.
        let pending_c = sc.c;
        let overflow = sc.overflow;
        let buff = std::mem::take(&mut sc.buff);
        drop(sc);
        if let Some(c) = pending_c {
            self.pending.push(c);
        }
        if overflow {
            return Ok(None);
        }
        Ok(parse_lua_number(&buff))
    }

    fn read_byte(&mut self) -> io::Result<Option<u8>> {
        if let Some(b) = self.pending.pop() {
            return Ok(Some(b));
        }
        let mut buf = [0u8; 1];
        let n = match &mut self.kind {
            FileKind::Regular(r) => r.read(&mut buf)?,
            FileKind::Stdin => io::stdin().lock().read(&mut buf)?,
            FileKind::ReadPipe(_, r) => r.read(&mut buf)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "attempt to use a closed file")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        };
        if n == 0 { Ok(None) } else { Ok(Some(buf[0])) }
    }

    // ── Write operations ───────────────────────────────────────

    fn write_bytes(&mut self, data: &[u8]) -> io::Result<()> {
        match &mut self.kind {
            FileKind::Regular(_) => {
                if !self.writable {
                    return Err(io::Error::from_raw_os_error(9));
                }
                match self.bufmode {
                    BufMode::None => {
                        let r = match &mut self.kind {
                            FileKind::Regular(r) => r,
                            _ => unreachable!(),
                        };
                        r.get_mut().write_all(data)?;
                    }
                    BufMode::Line => {
                        self.write_buf.extend_from_slice(data);
                        if data.contains(&b'\n') {
                            self.flush()?;
                        }
                    }
                    BufMode::Full => {
                        self.write_buf.extend_from_slice(data);
                        if self.bufsize > 0 && self.write_buf.len() >= self.bufsize {
                            self.flush()?;
                        }
                    }
                }
            }
            FileKind::Stdout => io::stdout().write_all(data)?,
            FileKind::Stderr => io::stderr().write_all(data)?,
            FileKind::WritePipe(_, w) => w.write_all(data)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "attempt to use a closed file")),
            FileKind::Stdin => return Err(io::Error::new(io::ErrorKind::Other, "not open for writing")),
            FileKind::ReadPipe(..) => {
                return Err(io::Error::new(io::ErrorKind::Other, "not open for writing"))
            }
        }
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        match &mut self.kind {
            FileKind::Regular(r) => {
                if !self.write_buf.is_empty() {
                    r.get_mut().write_all(&self.write_buf)?;
                    self.write_buf.clear();
                }
                r.get_mut().flush()
            }
            FileKind::Stdout => io::stdout().flush(),
            FileKind::Stderr => io::stderr().flush(),
            FileKind::WritePipe(_, w) => w.flush(),
            _ => Ok(()),
        }
    }

    fn seek(&mut self, whence: &str, offset: i64) -> io::Result<u64> {
        let pos = match whence {
            "set" => SeekFrom::Start(offset as u64),
            "cur" => SeekFrom::Current(offset),
            "end" => SeekFrom::End(offset),
            _ => return Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid whence")),
        };
        match &mut self.kind {
            FileKind::Regular(r) => {
                if !self.write_buf.is_empty() {
                    let buf = std::mem::take(&mut self.write_buf);
                    r.get_mut().write_all(&buf)?;
                }
                r.seek(pos)
            }
            _ => Err(io::Error::new(io::ErrorKind::Other, "cannot seek on this file")),
        }
    }

    /// Close the file. For pipes, waits for the child and returns its
    /// termination info as `(what, code)` where `what` is "exit" or "signal".
    fn close(&mut self) -> Option<(String, i64)> {
        // Flush buffered output; ignore errors (reference Lua's close still
        // succeeds after a failed flush).
        if !self.write_buf.is_empty() {
            let _ = self.flush();
            self.write_buf.clear();
        }
        let old = std::mem::replace(&mut self.kind, FileKind::Closed);
        match old {
            FileKind::ReadPipe(mut child, reader) => {
                drop(reader); // close the read end first so the child sees EOF
                child.wait().ok().map(pclose_result)
            }
            FileKind::WritePipe(mut child, writer) => {
                drop(writer); // close the write end first
                child.wait().ok().map(pclose_result)
            }
            _ => None,
        }
    }
}

// ── Helpers ────────────────────────────────────────────────────────

/// Extract a `&mut LuaFile` from args[idx], which must be a userdata.
///
/// # Safety
/// GcRef is a raw pointer wrapper. The referenced GcObject lives on the heap
/// and is valid for the duration of the native function call (kept alive on the VM stack).
fn get_file_mut<'a>(args: &[Value], idx: usize, fname: &str) -> Result<&'a mut LuaFile, LuaError> {
    match args.get(idx).copied() {
        Some(Value::Object(r)) => {
            // SAFETY: GcRef wraps NonNull<GcObject>. We use ptr_value() to obtain
            // the raw pointer and cast it to break the artificial lifetime tie to local `r`.
            let obj: &mut crate::gc::GcObject = unsafe { &mut *(r.ptr_value() as *mut crate::gc::GcObject) };
            let ud = obj.as_userdata_mut()
                .ok_or_else(|| LuaError::new(format!(
                    "bad argument #{} to '{}' (FILE* expected)", idx + 1, fname)))?;
            ud.data.downcast_mut::<LuaFile>()
                .ok_or_else(|| LuaError::new(format!(
                    "bad argument #{} to '{}' (FILE* expected)", idx + 1, fname)))
        }
        Some(v) => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (FILE* expected, got {})",
            idx + 1,
            fname,
            v.type_name()
        ))),
        None => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (FILE* expected, got no value)",
            idx + 1,
            fname
        ))),
    }
}

/// Borrow a `LuaFile` from a `GcRef`, breaking the artificial lifetime tie
/// (same raw-pointer approach as `get_file_mut`).
fn file_mut_from_ref<'a>(r: GcRef, fname: &str) -> Result<&'a mut LuaFile, LuaError> {
    let obj: &mut crate::gc::GcObject =
        unsafe { &mut *(r.ptr_value() as *mut crate::gc::GcObject) };
    let ud = obj
        .as_userdata_mut()
        .ok_or_else(|| LuaError::new(format!("bad argument to '{fname}' (FILE* expected)")))?;
    ud.data
        .downcast_mut::<LuaFile>()
        .ok_or_else(|| LuaError::new(format!("bad argument to '{fname}' (FILE* expected)")))
}

/// Current default input handle, creating the stdin handle lazily.
fn ensure_default_input(gc: &mut Gc) -> GcRef {
    if let Some(r) = gc.io_input {
        return r;
    }
    let r = new_file_handle(gc, LuaFile::stdin());
    gc.io_input = Some(r);
    r
}

/// Current default output handle, creating the stdout handle lazily.
fn ensure_default_output(gc: &mut Gc) -> GcRef {
    if let Some(r) = gc.io_output {
        return r;
    }
    let r = new_file_handle(gc, LuaFile::stdout());
    gc.io_output = Some(r);
    r
}

/// A parsed read format for `file:read`/`io.read`/`io.lines`.
#[derive(Clone, Copy)]
pub enum ReadFormat {
    Line,
    LineKeep,
    All,
    Number,
    Bytes(usize),
}

fn parse_read_format(v: Value) -> Result<ReadFormat, LuaError> {
    match v {
        Value::Nil => Ok(ReadFormat::Line),
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = s.as_bytes();
            let text = text.strip_prefix(b"*").unwrap_or(text);
            match text.first() {
                Some(b'n') => Ok(ReadFormat::Number),
                Some(b'a') => Ok(ReadFormat::All),
                Some(b'l') => Ok(ReadFormat::Line),
                Some(b'L') => Ok(ReadFormat::LineKeep),
                _ => Err(LuaError::new("invalid format")),
            }
        }
        Value::Integer(n) if n >= 0 => Ok(ReadFormat::Bytes(n as usize)),
        Value::Float(f) if f >= 0.0 && f.fract() == 0.0 => Ok(ReadFormat::Bytes(f as usize)),
        _ => Err(LuaError::new("invalid format")),
    }
}

fn do_read_format(file: &mut LuaFile, fmt: ReadFormat, gc: &mut Gc) -> Result<Value, LuaError> {
    match fmt {
        ReadFormat::Number => match file.read_number().map_err(|e| LuaError::new(e.to_string()))? {
            Some(v) => Ok(v),
            None => Ok(Value::Nil),
        },
        ReadFormat::All => {
            let bytes = file.read_all().map_err(|e| LuaError::new(e.to_string()))?;
            Ok(Value::Object(gc.new_string(&bytes)))
        }
        ReadFormat::Line | ReadFormat::LineKeep => {
            let keep = matches!(fmt, ReadFormat::LineKeep);
            match file.read_line(keep).map_err(|e| LuaError::new(e.to_string()))? {
                Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
                None => Ok(Value::Nil),
            }
        }
        ReadFormat::Bytes(0) => match file.read_byte().map_err(|e| LuaError::new(e.to_string()))? {
            Some(b) => {
                file.pending.push(b);
                Ok(Value::Object(gc.new_string(b"")))
            }
            None => Ok(Value::Nil),
        },
        ReadFormat::Bytes(n) => match file
            .read_bytes(n)
            .map_err(|e| LuaError::new(e.to_string()))?
        {
            Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
            None => Ok(Value::Nil),
        },
    }
}

/// Parse a Lua numeral (decimal/hex integer, decimal/hex float) the way
/// the lexer does, returning `None` when the token is not a valid numeral.
pub fn parse_lua_number(token: &[u8]) -> Option<Value> {
    let txt = std::str::from_utf8(token).ok()?;
    let (neg, body) = if let Some(rest) = txt.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = txt.strip_prefix('+') {
        (false, rest)
    } else {
        (false, txt)
    };
    if body.is_empty() {
        return None;
    }

    let (is_hex, digits) = match body.strip_prefix("0x").or_else(|| body.strip_prefix("0X")) {
        Some(rest) => (true, rest),
        None => (false, body),
    };

    if is_hex {
        if digits.is_empty() {
            return None;
        }
        if digits.contains('.') || digits.contains('p') || digits.contains('P') {
            let f = parse_hex_float(digits)?;
            return Some(Value::Float(if neg { -f } else { f }));
        }
        let mut v: u64 = 0;
        for &c in digits.as_bytes() {
            let d = match c {
                b'0'..=b'9' => c - b'0',
                b'a'..=b'f' => c - b'a' + 10,
                b'A'..=b'F' => c - b'A' + 10,
                _ => return None,
            };
            v = v.wrapping_mul(16).wrapping_add(d as u64);
        }
        let signed = v as i64;
        return Some(Value::Integer(if neg { signed.wrapping_neg() } else { signed }));
    }

    // Decimal: try an integer first (float on overflow), then a float.
    if !digits
        .as_bytes()
        .first()
        .map_or(false, |c| c.is_ascii_digit() || *c == b'.')
    {
        return None;
    }
    if !digits.contains('.') && !digits.contains('e') && !digits.contains('E') {
        if let Ok(i) = digits.parse::<i64>() {
            return Some(Value::Integer(if neg { -i } else { i }));
        }
    }
    digits
        .parse::<f64>()
        .ok()
        .map(|f| Value::Float(if neg { -f } else { f }))
}

/// Parse the body of a hexadecimal float (after the `0x` prefix), e.g.
/// `ABCp-3` or `1.8p1`. Returns `None` if malformed.
fn parse_hex_float(digits: &str) -> Option<f64> {
    let (mantissa, exp) = match digits.split_once(['p', 'P']) {
        Some((m, e)) => (m, e.parse::<i32>().ok()?),
        None => (digits, 0),
    };
    let (int_part, frac_part) = match mantissa.split_once('.') {
        Some((i, f)) => (i, f),
        None => (mantissa, ""),
    };
    if int_part.is_empty() && frac_part.is_empty() {
        return None;
    }
    let mut value: f64 = 0.0;
    let mut any = false;
    for c in int_part.bytes() {
        let d = hex_digit(c)?;
        value = value * 16.0 + d as f64;
        any = true;
    }
    let mut scale: f64 = 1.0 / 16.0;
    for c in frac_part.bytes() {
        let d = hex_digit(c)?;
        value += d as f64 * scale;
        scale /= 16.0;
        any = true;
    }
    if !any {
        return None;
    }
    Some(value * 2f64.powi(exp))
}

fn is_numeral_char(c: u8) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_hexdigit()
        || matches!(c, b'.' | b'x' | b'X' | b'p' | b'P' | b'e' | b'E' | b'+' | b'-')
}

fn hex_digit(c: u8) -> Option<u32> {
    match c {
        b'0'..=b'9' => Some((c - b'0') as u32),
        b'a'..=b'f' => Some((c - b'a' + 10) as u32),
        b'A'..=b'F' => Some((c - b'A' + 10) as u32),
        _ => None,
    }
}

/// Perform a read operation on a LuaFile for one format argument.
fn do_read_one(file: &mut LuaFile, fmt: Value, gc: &mut Gc) -> Result<Value, LuaError> {
    let format = parse_read_format(fmt)?;
    do_read_format(file, format, gc)
}

/// Create a new file handle userdata with the file metatable.
fn new_file_handle(gc: &mut Gc, file: LuaFile) -> GcRef {
    let mt = gc.file_metatable;
    let r = gc.new_userdata(Box::new(file), mt);
    // File handles are finalized so buffered output is flushed even when
    // the handle is collected without an explicit close.
    gc.register_finalizer(r);
    r
}

// ── File methods (first arg is the file handle) ────────────────────

pub fn file_read(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file = get_file_mut(args, 0, "read")?;
    if args.len() <= 1 {
        // Default: read a line
        match do_read_one(file, Value::Nil, gc) {
            Ok(v) => return Ok(vec![v]),
            Err(e) if e.message.contains("invalid format") => return Err(e),
            Err(e) => {
                let msg = gc.new_string(e.message.as_bytes());
                return Ok(vec![
                    Value::Nil,
                    Value::Object(msg),
                    Value::Integer(0),
                ]);
            }
        }
    }
    let mut results = Vec::new();
    for i in 1..args.len() {
        match do_read_one(file, args[i], gc) {
            Ok(v) => {
                let failed = v.is_nil();
                results.push(v);
                if failed {
                    break;
                }
            }
            Err(e) if e.message.contains("invalid format") => return Err(e),
            Err(e) => {
                let msg = gc.new_string(e.message.as_bytes());
                return Ok(vec![
                    Value::Nil,
                    Value::Object(msg),
                    Value::Integer(0),
                ]);
            }
        }
    }
    Ok(results)
}

pub fn file_write(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file_val = args.first().copied().unwrap_or(Value::Nil);
    let file = get_file_mut(args, 0, "write")?;
    for arg in &args[1..] {
        let data = match arg {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                r.as_object().as_string().unwrap().as_bytes().to_vec()
            }
            Value::Integer(n) => format!("{n}").into_bytes(),
            Value::Float(n) => format!("{n}").into_bytes(),
            _ => return Err(LuaError::new("bad argument to 'write' (string or number expected)")),
        };
        if let Err(e) = file.write_bytes(&data) {
            let errno = e.raw_os_error().unwrap_or(0) as i64;
            let msg = gc.new_string(e.to_string().as_bytes());
            return Ok(vec![
                Value::Nil,
                Value::Object(msg),
                Value::Integer(errno),
            ]);
        }
    }
    Ok(vec![file_val])
}

pub fn file_close(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file = get_file_mut(args, 0, "close")?;
    if file.is_closed() {
        return Err(LuaError::new("attempt to use a closed file"));
    }
    if !file.closable {
        let msg = gc.new_string(b"cannot close standard file");
        return Ok(vec![Value::Nil, Value::Object(msg)]);
    }
    match file.close() {
        Some((what, code)) => {
            let ok = what == "exit" && code == 0;
            Ok(vec![
                if ok { Value::Boolean(true) } else { Value::Nil },
                Value::Object(gc.new_string(what.as_bytes())),
                Value::Integer(code),
            ])
        }
        None => Ok(vec![Value::Boolean(true)]),
    }
}

pub fn file_seek(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let whence = args.get(1)
        .and_then(|v| v.as_str_bytes())
        .map(|b| std::str::from_utf8(b).unwrap_or("cur"))
        .unwrap_or("cur");
    let offset = args.get(2)
        .and_then(|v| v.as_integer())
        .unwrap_or(0);
    let file = get_file_mut(args, 0, "seek")?;
    match file.seek(whence, offset) {
        Ok(pos) => Ok(vec![Value::Integer(pos as i64)]),
        Err(e) => {
            let errno = e.raw_os_error().unwrap_or(0) as i64;
            let msg = e.to_string();
            Ok(vec![
                Value::Nil,
                Value::Object(gc.new_string(msg.as_bytes())),
                Value::Integer(errno),
            ])
        }
    }
}

pub fn file_flush(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file_val = args.first().copied().unwrap_or(Value::Nil);
    let file = get_file_mut(args, 0, "flush")?;
    if let Err(e) = file.flush() {
        let errno = e.raw_os_error().unwrap_or(0) as i64;
        let msg = _gc.new_string(e.to_string().as_bytes());
        return Ok(vec![
            Value::Nil,
            Value::Object(msg),
            Value::Integer(errno),
        ]);
    }
    Ok(vec![file_val])
}

pub fn file_setvbuf(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mode = match args.get(1) {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            String::from_utf8_lossy(r.as_object().as_string().unwrap().as_bytes()).to_string()
        }
        _ => {
            return Err(LuaError::new(
                "bad argument #2 to 'setvbuf' (string expected)",
            ))
        }
    };
    let size = match args.get(2).and_then(|v| v.as_integer()) {
        Some(n) if n >= 0 => n as usize,
        Some(_) => {
            return Err(LuaError::new(
                "bad argument #3 to 'setvbuf' (invalid buffer size)",
            ))
        }
        None => 0,
    };
    let bufmode = match mode.as_str() {
        "no" => BufMode::None,
        "full" => BufMode::Full,
        "line" => BufMode::Line,
        _ => return Err(LuaError::new(format!("invalid option '{mode}'"))),
    };
    let file_val = args.first().copied().unwrap_or(Value::Nil);
    let file = get_file_mut(args, 0, "setvbuf")?;
    file.bufmode = bufmode;
    file.bufsize = size;
    Ok(vec![file_val])
}

pub fn file_lines(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file_ref = match args.first() {
        Some(Value::Object(r)) => *r,
        _ => return Err(LuaError::new("bad argument #1 to 'lines' (FILE* expected)")),
    };
    if args.len() - 1 > 250 {
        return Err(LuaError::new("too many arguments"));
    }
    let mut formats: Vec<ReadFormat> = Vec::new();
    for v in &args[1..] {
        formats.push(parse_read_format(*v)?);
    }
    if formats.is_empty() {
        formats.push(ReadFormat::Line);
    }

    let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let ud = file_ref
            .as_object_mut()
            .as_userdata_mut()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        let file = ud
            .data
            .downcast_mut::<LuaFile>()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        let mut results = Vec::with_capacity(formats.len());
        for fmt in &formats {
            let v = do_read_format(file, *fmt, gc)?;
            let failed = v.is_nil();
            results.push(v);
            if failed {
                break;
            }
        }
        Ok(results)
    };
    let closure = Closure::new_native_dyn("file_lines_iterator".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    Ok(vec![Value::Object(closure_ref)])
}

/// file:__tostring metamethod (matches reference Lua's "file (0x...)").
pub fn file_tostring(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if let Some(Value::Object(r)) = args.first().copied() {
        if let Some(ud) = r.as_object().as_userdata() {
            if let Some(file) = ud.data.downcast_ref::<LuaFile>() {
                let s = if file.is_closed() {
                    "file (closed)".to_string()
                } else {
                    format!("file (0x{:x})", r.ptr_value() as usize)
                };
                return Ok(vec![Value::Object(gc.new_string(s.as_bytes()))]);
            }
        }
    }
    Err(LuaError::new("bad argument #1 to '__tostring' (FILE* expected)"))
}

/// file:__close metamethod
pub fn file_gc_close(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if let Some(Value::Object(r)) = args.first() {
        if let Some(ud) = r.as_object_mut().as_userdata_mut() {
            if let Some(file) = ud.data.downcast_mut::<LuaFile>() {
                if file.closable && !file.is_closed() {
                    file.close();
                }
            }
        }
    }
    Ok(vec![])
}

// ── io library functions ───────────────────────────────────────────

pub fn io_open(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let filename = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid filename"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'open' (string expected)")),
    };
    let mode = args
        .get(1)
        .and_then(|v| v.as_str_bytes())
        .map(|b| std::str::from_utf8(b).unwrap_or("r"))
        .unwrap_or("r");

    // Allowed modes follow C's fopen: [rwa] '+'? 'b'? (the 'b' must be last).
    let valid = match mode.as_bytes() {
        [c] if matches!(c, b'r' | b'w' | b'a') => true,
        [c, b'b'] if matches!(c, b'r' | b'w' | b'a') => true,
        [c, b'+'] if matches!(c, b'r' | b'w' | b'a') => true,
        [c, b'+', b'b'] if matches!(c, b'r' | b'w' | b'a') => true,
        _ => false,
    };
    if !valid {
        return Err(LuaError::new(format!("invalid mode '{}'", mode)));
    }

    let opened: Result<File, std::io::Error> = match mode {
        "r" | "rb" => File::open(&filename),
        "w" | "wb" => File::create(&filename),
        "a" | "ab" => OpenOptions::new().append(true).create(true).open(&filename),
        "r+" | "r+b" => OpenOptions::new().read(true).write(true).open(&filename),
        "w+" | "w+b" => OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&filename),
        "a+" | "a+b" => OpenOptions::new()
            .read(true)
            .append(true)
            .create(true)
            .open(&filename),
        _ => unreachable!(),
    };

    match opened {
        Ok(file) => {
            let writable = mode.contains('+') || mode.starts_with('w') || mode.starts_with('a');
            let lf = if writable {
                LuaFile::from_file(file)
            } else {
                LuaFile::from_file_readonly(file)
            };
            let handle = new_file_handle(gc, lf);
            Ok(vec![Value::Object(handle)])
        }
        Err(e) => {
            let errno = e.raw_os_error().unwrap_or(0) as i64;
            let msg = format!("{}: {}", filename, e);
            Ok(vec![
                Value::Nil,
                Value::Object(gc.new_string(msg.as_bytes())),
                Value::Integer(errno),
            ])
        }
    }
}

pub fn io_close(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        // Close the default output file (standard handles are not closable).
        let fref = ensure_default_output(gc);
        let file = file_mut_from_ref(fref, "close")?;
        if file.closable {
            file.close();
        }
        return Ok(vec![Value::Boolean(true)]);
    }
    let file = get_file_mut(args, 0, "close")?;
    if file.is_closed() {
        return Err(LuaError::new("attempt to use a closed file"));
    }
    if !file.closable {
        let msg = gc.new_string(b"cannot close standard file");
        return Ok(vec![Value::Nil, Value::Object(msg)]);
    }
    match file.close() {
        Some((what, code)) => {
            let ok = what == "exit" && code == 0;
            Ok(vec![
                if ok { Value::Boolean(true) } else { Value::Nil },
                Value::Object(gc.new_string(what.as_bytes())),
                Value::Integer(code),
            ])
        }
        None => Ok(vec![Value::Boolean(true)]),
    }
}

pub fn io_read(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fref = ensure_default_input(gc);
    let file = file_mut_from_ref(fref, "read")?;
    if file.is_closed() {
        return Err(LuaError::new("default input file is closed"));
    }
    if args.is_empty() {
        let v = do_read_one(file, Value::Nil, gc)?;
        return Ok(vec![v]);
    }
    let mut results = Vec::new();
    for arg in args {
        match do_read_one(file, *arg, gc) {
            Ok(v) => {
                let failed = v.is_nil();
                results.push(v);
                if failed {
                    break;
                }
            }
            Err(e) if e.message.contains("invalid format") => return Err(e),
            Err(e) => {
                let msg = gc.new_string(e.message.as_bytes());
                return Ok(vec![
                    Value::Nil,
                    Value::Object(msg),
                    Value::Integer(0),
                ]);
            }
        }
    }
    Ok(results)
}

pub fn io_write(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fref = ensure_default_output(gc);
    let file = file_mut_from_ref(fref, "write")?;
    if file.is_closed() {
        return Err(LuaError::new("default output file is closed"));
    }
    for arg in args {
        let data = match arg {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                r.as_object().as_string().unwrap().as_bytes().to_vec()
            }
            Value::Integer(n) => format!("{n}").into_bytes(),
            Value::Float(n) => format!("{n}").into_bytes(),
            _ => return Err(LuaError::new("bad argument to 'write' (string or number expected)")),
        };
        file.write_bytes(&data).map_err(|e| LuaError::new(e.to_string()))?;
    }
    // On success, return the file handle (matches reference Lua).
    Ok(vec![Value::Object(fref)])
}

pub fn io_flush(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fref = ensure_default_output(gc);
    let file = file_mut_from_ref(fref, "flush")?;
    if let Err(e) = file.flush() {
        let errno = e.raw_os_error().unwrap_or(0) as i64;
        let msg = gc.new_string(e.to_string().as_bytes());
        return Ok(vec![
            Value::Nil,
            Value::Object(msg),
            Value::Integer(errno),
        ]);
    }
    Ok(vec![Value::Boolean(true)])
}

pub fn io_type(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    match args.first() {
        Some(Value::Object(r)) => {
            if let Some(ud) = r.as_object().as_userdata() {
                if let Some(file) = ud.data.downcast_ref::<LuaFile>() {
                    let s = if file.is_closed() { "closed file" } else { "file" };
                    return Ok(vec![Value::Object(gc.new_string(s.as_bytes()))]);
                }
            }
            Ok(vec![Value::Boolean(false)])
        }
        _ => Ok(vec![Value::Boolean(false)]),
    }
}

pub fn io_tmpfile(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let dir = std::env::temp_dir();
    let path = dir.join(format!("lua_tmpfile_{}", std::process::id()));
    let file = OpenOptions::new()
        .read(true).write(true).create(true).truncate(true)
        .open(&path)
        .map_err(|e| LuaError::new(e.to_string()))?;
    // Try to delete on creation so it auto-cleans (Unix-like)
    let _ = std::fs::remove_file(&path);
    let handle = new_file_handle(gc, LuaFile::from_file(file));
    Ok(vec![Value::Object(handle)])
}

pub fn io_lines(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Parse optional formats (after the filename; nil means default input).
    let use_default = args.is_empty() || args[0].is_nil();
    let fmt_start = if args.is_empty() { 0 } else { 1 };
    if args.len() - fmt_start > 250 {
        return Err(LuaError::new("too many arguments"));
    }
    let mut formats: Vec<ReadFormat> = Vec::new();
    for v in &args[fmt_start..] {
        formats.push(parse_read_format(*v)?);
    }
    if formats.is_empty() {
        formats.push(ReadFormat::Line);
    }

    if use_default {
        // io.lines() / io.lines(nil, ...) — iterate from the default input.
        let fref = ensure_default_input(gc);
        let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
            let file = file_mut_from_ref(fref, "lines")?;
            let mut results = Vec::with_capacity(formats.len());
            for fmt in &formats {
                let v = do_read_format(file, *fmt, gc)?;
                let failed = v.is_nil();
                results.push(v);
                if failed {
                    break;
                }
            }
            Ok(results)
        };
        let closure = Closure::new_native_dyn("io_lines_default".into(), iter_fn);
        let closure_ref = gc.new_closure(closure);
        return Ok(vec![Value::Object(closure_ref)]);
    }

    // io.lines(filename, ...) — open file, iterate, close at EOF.
    let filename = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid filename"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'lines' (string expected)")),
    };

    let file = File::open(&filename)
        .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?;
    let file_handle = LuaFile::from_file(file);
    let file_ref = new_file_handle(gc, file_handle);

    let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let ud = file_ref
            .as_object_mut()
            .as_userdata_mut()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        let file = ud
            .data
            .downcast_mut::<LuaFile>()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        if file.is_closed() {
            return Err(LuaError::new("file is already closed"));
        }
        let mut results = Vec::with_capacity(formats.len());
        let mut failed = false;
        for fmt in &formats {
            match do_read_format(file, *fmt, gc) {
                Ok(v) => {
                    let is_nil = v.is_nil();
                    results.push(v);
                    if is_nil {
                        failed = true;
                        break;
                    }
                }
                Err(e) => {
                    file.close();
                    return Err(e);
                }
            }
        }
        // Only close when the whole iteration produced nothing (the first
        // read hit EOF); a later format failing still yields the earlier
        // values and the loop continues.
        if failed && results.first().map_or(true, |v| v.is_nil()) {
            file.close();
        }
        Ok(results)
    };
    let closure = Closure::new_native_dyn("io_lines_file".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    // Four values: iterator, state, closing value (TBC), control.
    Ok(vec![
        Value::Object(closure_ref),
        Value::Nil,
        Value::Nil,
        Value::Object(file_ref),
    ])
}

pub fn io_input(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    match args.first().copied() {
        None | Some(Value::Nil) => {
            Ok(vec![Value::Object(ensure_default_input(gc))])
        }
        Some(Value::Object(r)) if r.as_object().as_userdata().is_some() => {
            // Must be a file handle.
            file_mut_from_ref(r, "input")?;
            gc.io_input = Some(r);
            Ok(vec![Value::Object(r)])
        }
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let filename = String::from_utf8_lossy(
                r.as_object().as_string().unwrap().as_bytes(),
            )
            .to_string();
            let file = File::open(&filename)
                .map_err(|e| LuaError::new(format!("{filename}: {e}")))?;
            let handle = new_file_handle(gc, LuaFile::from_file_readonly(file));
            gc.io_input = Some(handle);
            Ok(vec![Value::Object(handle)])
        }
        Some(v) => Err(LuaError::new(format!(
            "bad argument #1 to 'input' (FILE* expected, got {})",
            v.type_name()
        ))),
    }
}

pub fn io_output(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    match args.first().copied() {
        None | Some(Value::Nil) => {
            Ok(vec![Value::Object(ensure_default_output(gc))])
        }
        Some(Value::Object(r)) if r.as_object().as_userdata().is_some() => {
            file_mut_from_ref(r, "output")?;
            gc.io_output = Some(r);
            Ok(vec![Value::Object(r)])
        }
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let filename = String::from_utf8_lossy(
                r.as_object().as_string().unwrap().as_bytes(),
            )
            .to_string();
            let file = File::create(&filename)
                .map_err(|e| LuaError::new(format!("{filename}: {e}")))?;
            let handle = new_file_handle(gc, LuaFile::from_file(file));
            gc.io_output = Some(handle);
            Ok(vec![Value::Object(handle)])
        }
        Some(v) => Err(LuaError::new(format!(
            "bad argument #1 to 'output' (FILE* expected, got {})",
            v.type_name()
        ))),
    }
}

/// Interpret a child's exit status the way `pclose` does.
fn pclose_result(status: std::process::ExitStatus) -> (String, i64) {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            return ("signal".to_string(), sig as i64);
        }
    }
    ("exit".to_string(), status.code().unwrap_or(0) as i64)
}

pub fn io_popen(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let cmd = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            String::from_utf8_lossy(r.as_object().as_string().unwrap().as_bytes()).to_string()
        }
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'popen' (string expected)",
            ))
        }
    };
    let mode = args
        .get(1)
        .and_then(|v| v.as_str_bytes())
        .map(|b| String::from_utf8_lossy(b).to_string())
        .unwrap_or_else(|| "r".to_string());

    let mut command = Command::new("/bin/sh");
    command.arg("-c").arg(&cmd);

    let file = if mode == "r" {
        command.stdout(Stdio::piped());
        let mut child = command
            .spawn()
            .map_err(|e| LuaError::new(format!("{cmd}: {e}")))?;
        let out = child.stdout.take().unwrap();
        LuaFile {
            kind: FileKind::ReadPipe(child, BufReader::new(out)),
            closable: true,
            pending: Vec::new(),
            write_buf: Vec::new(),
            writable: false,
            bufmode: BufMode::Full,
            bufsize: 0,
        }
    } else if mode == "w" {
        command.stdin(Stdio::piped());
        let mut child = command
            .spawn()
            .map_err(|e| LuaError::new(format!("{cmd}: {e}")))?;
        let inp = child.stdin.take().unwrap();
        LuaFile {
            kind: FileKind::WritePipe(child, inp),
            closable: true,
            pending: Vec::new(),
            write_buf: Vec::new(),
            writable: true,
            bufmode: BufMode::Full,
            bufsize: 0,
        }
    } else {
        return Err(LuaError::new(format!("invalid mode '{mode}'")));
    };

    let handle = new_file_handle(gc, file);
    Ok(vec![Value::Object(handle)])
}

// ── Registration helpers ───────────────────────────────────────────

/// Return io library functions for registration.
pub fn io_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("open", io_open as NativeFn),
        ("close", io_close),
        ("read", io_read),
        ("write", io_write),
        ("flush", io_flush),
        ("type", io_type),
        ("tmpfile", io_tmpfile),
        ("lines", io_lines),
        ("input", io_input),
        ("output", io_output),
        ("popen", io_popen),
    ]
}

/// Return file method functions for the file metatable __index.
pub fn file_methods() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("read", file_read as NativeFn),
        ("write", file_write),
        ("close", file_close),
        ("seek", file_seek),
        ("flush", file_flush),
        ("lines", file_lines),
        ("setvbuf", file_setvbuf),
    ]
}
