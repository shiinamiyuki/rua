//! io library: file I/O operations.

use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Seek, SeekFrom, Write};

use crate::closure::{Closure, NativeFn};
use crate::error::LuaError;
use crate::gc::{Gc, GcRef};
use crate::value::Value;

// ── LuaFile ────────────────────────────────────────────────────────

/// Internal file handle stored as userdata.
pub struct LuaFile {
    kind: FileKind,
    closable: bool,
}

enum FileKind {
    Regular(BufReader<File>),
    Stdin,
    Stdout,
    Stderr,
    Closed,
}

impl LuaFile {
    pub fn stdin() -> Self {
        LuaFile { kind: FileKind::Stdin, closable: false }
    }
    pub fn stdout() -> Self {
        LuaFile { kind: FileKind::Stdout, closable: false }
    }
    pub fn stderr() -> Self {
        LuaFile { kind: FileKind::Stderr, closable: false }
    }
    fn from_file(file: File) -> Self {
        LuaFile { kind: FileKind::Regular(BufReader::new(file)), closable: true }
    }
    pub fn is_closed(&self) -> bool {
        matches!(self.kind, FileKind::Closed)
    }

    // ── Read operations ────────────────────────────────────────

    fn read_line(&mut self, keep_nl: bool) -> io::Result<Option<Vec<u8>>> {
        let mut buf = String::new();
        let n = match &mut self.kind {
            FileKind::Regular(r) => r.read_line(&mut buf)?,
            FileKind::Stdin => io::stdin().lock().read_line(&mut buf)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "file is closed")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        };
        if n == 0 {
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
        match &mut self.kind {
            FileKind::Regular(r) => { r.read_to_end(&mut buf)?; }
            FileKind::Stdin => { io::stdin().lock().read_to_end(&mut buf)?; }
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "file is closed")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        }
        Ok(buf)
    }

    fn read_bytes(&mut self, n: usize) -> io::Result<Option<Vec<u8>>> {
        let mut buf = vec![0u8; n];
        let bytes_read = match &mut self.kind {
            FileKind::Regular(r) => r.read(&mut buf)?,
            FileKind::Stdin => io::stdin().lock().read(&mut buf)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "file is closed")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        };
        if bytes_read == 0 {
            return Ok(None); // EOF
        }
        buf.truncate(bytes_read);
        Ok(Some(buf))
    }

    fn read_number(&mut self) -> io::Result<Option<f64>> {
        // Read whitespace-delimited token and parse as number
        let mut s = String::new();
        let mut started = false;
        loop {
            let b = match self.read_byte() {
                Ok(Some(b)) => b,
                Ok(None) => break,
                Err(e) => return Err(e),
            };
            if b.is_ascii_whitespace() {
                if started { break; }
                continue;
            }
            started = true;
            s.push(b as char);
        }
        if s.is_empty() {
            return Ok(None);
        }
        match s.parse::<f64>() {
            Ok(n) => Ok(Some(n)),
            Err(_) => Ok(None),
        }
    }

    fn read_byte(&mut self) -> io::Result<Option<u8>> {
        let mut buf = [0u8; 1];
        let n = match &mut self.kind {
            FileKind::Regular(r) => r.read(&mut buf)?,
            FileKind::Stdin => io::stdin().lock().read(&mut buf)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "file is closed")),
            _ => return Err(io::Error::new(io::ErrorKind::Other, "not open for reading")),
        };
        if n == 0 { Ok(None) } else { Ok(Some(buf[0])) }
    }

    // ── Write operations ───────────────────────────────────────

    fn write_bytes(&mut self, data: &[u8]) -> io::Result<()> {
        match &mut self.kind {
            FileKind::Regular(r) => r.get_mut().write_all(data)?,
            FileKind::Stdout => io::stdout().write_all(data)?,
            FileKind::Stderr => io::stderr().write_all(data)?,
            FileKind::Closed => return Err(io::Error::new(io::ErrorKind::Other, "file is closed")),
            FileKind::Stdin => return Err(io::Error::new(io::ErrorKind::Other, "not open for writing")),
        }
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        match &mut self.kind {
            FileKind::Regular(r) => r.get_mut().flush(),
            FileKind::Stdout => io::stdout().flush(),
            FileKind::Stderr => io::stderr().flush(),
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
            FileKind::Regular(r) => r.seek(pos),
            _ => Err(io::Error::new(io::ErrorKind::Other, "cannot seek on this file")),
        }
    }

    fn close(&mut self) {
        self.kind = FileKind::Closed;
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
        _ => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (FILE* expected)", idx + 1, fname))),
    }
}

/// Perform a read operation on a LuaFile for one format argument.
fn do_read_one(file: &mut LuaFile, fmt: Value, gc: &mut Gc) -> Result<Value, LuaError> {
    // Default is "*l" (read a line)
    let format = match fmt {
        Value::Nil => "l",
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = std::str::from_utf8(s.as_bytes()).unwrap_or("");
            // Strip optional leading '*' for back-compat
            let text = text.strip_prefix('*').unwrap_or(text);
            // SAFETY: we need a 'static str but text borrows from GC.
            // We'll match immediately, so this is safe.
            match text {
                "n" => "n",
                "a" => "a",
                "l" => "l",
                "L" => "L",
                _ => return Err(LuaError::new(format!("invalid format"))),
            }
        }
        Value::Integer(n) => {
            if n < 0 {
                return Err(LuaError::new("invalid format"));
            }
            let result = file.read_bytes(n as usize)
                .map_err(|e| LuaError::new(e.to_string()))?;
            return match result {
                Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
                None => Ok(Value::Nil),
            };
        }
        Value::Float(n) => {
            let n = n as i64;
            if n < 0 {
                return Err(LuaError::new("invalid format"));
            }
            let result = file.read_bytes(n as usize)
                .map_err(|e| LuaError::new(e.to_string()))?;
            return match result {
                Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
                None => Ok(Value::Nil),
            };
        }
        _ => return Err(LuaError::new("invalid read format")),
    };

    match format {
        "n" => {
            match file.read_number().map_err(|e| LuaError::new(e.to_string()))? {
                Some(n) => {
                    if n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
                        Ok(Value::Integer(n as i64))
                    } else {
                        Ok(Value::Float(n))
                    }
                }
                None => Ok(Value::Nil),
            }
        }
        "a" => {
            let bytes = file.read_all().map_err(|e| LuaError::new(e.to_string()))?;
            Ok(Value::Object(gc.new_string(&bytes)))
        }
        "l" => {
            match file.read_line(false).map_err(|e| LuaError::new(e.to_string()))? {
                Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
                None => Ok(Value::Nil),
            }
        }
        "L" => {
            match file.read_line(true).map_err(|e| LuaError::new(e.to_string()))? {
                Some(bytes) => Ok(Value::Object(gc.new_string(&bytes))),
                None => Ok(Value::Nil),
            }
        }
        _ => unreachable!(),
    }
}

/// Create a new file handle userdata with the file metatable.
fn new_file_handle(gc: &mut Gc, file: LuaFile) -> GcRef {
    let mt = gc.file_metatable;
    gc.new_userdata(Box::new(file), mt)
}

// ── File methods (first arg is the file handle) ────────────────────

pub fn file_read(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file = get_file_mut(args, 0, "read")?;
    if args.len() <= 1 {
        // Default: read a line
        let v = do_read_one(file, Value::Nil, gc)?;
        return Ok(vec![v]);
    }
    let mut results = Vec::new();
    for i in 1..args.len() {
        let v = do_read_one(file, args[i], gc)?;
        results.push(v);
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
        file.write_bytes(&data).map_err(|e: io::Error| LuaError::new(e.to_string()))?;
    }
    Ok(vec![file_val])
}

pub fn file_close(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file = get_file_mut(args, 0, "close")?;
    if !file.closable {
        return Err(LuaError::new("cannot close standard file"));
    }
    file.close();
    Ok(vec![Value::Boolean(true)])
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
            let msg: String = e.to_string();
            Ok(vec![Value::Nil, Value::Object(gc.new_string(msg.as_bytes()))])
        }
    }
}

pub fn file_flush(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file_val = args.first().copied().unwrap_or(Value::Nil);
    let file = get_file_mut(args, 0, "flush")?;
    file.flush().map_err(|e: io::Error| LuaError::new(e.to_string()))?;
    Ok(vec![file_val])
}

pub fn file_setvbuf(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Stub: Rust handles its own buffering
    let file_val = args.first().copied().unwrap_or(Value::Nil);
    Ok(vec![file_val])
}

pub fn file_lines(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let file_ref = match args.first() {
        Some(Value::Object(r)) => *r,
        _ => return Err(LuaError::new("bad argument #1 to 'lines' (FILE* expected)")),
    };
    // Return an iterator closure that reads lines from this file handle
    let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let ud = file_ref.as_object_mut().as_userdata_mut()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        let file = ud.data.downcast_mut::<LuaFile>()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        match file.read_line(false) {
            Ok(Some(bytes)) => Ok(vec![Value::Object(gc.new_string(&bytes))]),
            Ok(None) => Ok(vec![Value::Nil]),
            Err(e) => Err(LuaError::new(e.to_string())),
        }
    };
    let closure = Closure::new_native_dyn("file_lines_iterator".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    Ok(vec![Value::Object(closure_ref)])
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
    let mode = args.get(1)
        .and_then(|v| v.as_str_bytes())
        .map(|b| std::str::from_utf8(b).unwrap_or("r"))
        .unwrap_or("r");

    let file = match mode {
        "r" | "rb" => {
            File::open(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        "w" | "wb" => {
            File::create(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        "a" | "ab" => {
            OpenOptions::new().append(true).create(true).open(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        "r+" | "r+b" | "rb+" => {
            OpenOptions::new().read(true).write(true).open(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        "w+" | "w+b" | "wb+" => {
            OpenOptions::new().read(true).write(true).create(true).truncate(true).open(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        "a+" | "a+b" | "ab+" => {
            OpenOptions::new().read(true).append(true).create(true).open(&filename)
                .map_err(|e| LuaError::new(format!("{}: {}", filename, e)))?
        }
        _ => return Err(LuaError::new(format!("invalid mode '{}'", mode))),
    };

    let handle = new_file_handle(gc, LuaFile::from_file(file));
    Ok(vec![Value::Object(handle)])
}

pub fn io_close(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        // Close default output (stdout) — no-op for standard files
        return Ok(vec![Value::Boolean(true)]);
    }
    let file = get_file_mut(args, 0, "close")?;
    if !file.closable {
        return Err(LuaError::new("cannot close standard file"));
    }
    file.close();
    Ok(vec![Value::Boolean(true)])
}

pub fn io_read(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut stdin_file = LuaFile::stdin();
    if args.is_empty() {
        let v = do_read_one(&mut stdin_file, Value::Nil, gc)?;
        return Ok(vec![v]);
    }
    let mut results = Vec::new();
    for arg in args {
        let v = do_read_one(&mut stdin_file, *arg, gc)?;
        results.push(v);
    }
    Ok(results)
}

pub fn io_write(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    for arg in args {
        let data = match arg {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                r.as_object().as_string().unwrap().as_bytes().to_vec()
            }
            Value::Integer(n) => format!("{n}").into_bytes(),
            Value::Float(n) => format!("{n}").into_bytes(),
            _ => return Err(LuaError::new("bad argument to 'write' (string or number expected)")),
        };
        io::stdout().write_all(&data).map_err(|e| LuaError::new(e.to_string()))?;
    }
    // Return io.stdout handle... but we don't have it here.
    // Return true for compatibility.
    Ok(vec![Value::Boolean(true)])
}

pub fn io_flush(_args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    io::stdout().flush().map_err(|e| LuaError::new(e.to_string()))?;
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
    if args.is_empty() {
        // io.lines() — iterate lines from stdin
        let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
            let mut line = String::new();
            let n = io::stdin().lock().read_line(&mut line)
                .map_err(|e| LuaError::new(e.to_string()))?;
            if n == 0 {
                return Ok(vec![Value::Nil]);
            }
            if line.ends_with('\n') { line.pop(); }
            if line.ends_with('\r') { line.pop(); }
            Ok(vec![Value::Object(gc.new_string(line.as_bytes()))])
        };
        let closure = Closure::new_native_dyn("io_lines_stdin".into(), iter_fn);
        let closure_ref = gc.new_closure(closure);
        return Ok(vec![Value::Object(closure_ref)]);
    }

    // io.lines(filename) — open file, iterate lines, close at end
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
        let ud = file_ref.as_object_mut().as_userdata_mut()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        let file = ud.data.downcast_mut::<LuaFile>()
            .ok_or_else(|| LuaError::new("file handle expected"))?;
        match file.read_line(false) {
            Ok(Some(bytes)) => Ok(vec![Value::Object(gc.new_string(&bytes))]),
            Ok(None) => {
                file.close(); // Auto-close at EOF
                Ok(vec![Value::Nil])
            }
            Err(e) => {
                file.close();
                Err(LuaError::new(e.to_string()))
            }
        }
    };
    let closure = Closure::new_native_dyn("io_lines_file".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    Ok(vec![Value::Object(closure_ref)])
}

pub fn io_input(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Simplified: always returns stdin handle
    let handle = new_file_handle(gc, LuaFile::stdin());
    Ok(vec![Value::Object(handle)])
}

pub fn io_output(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Simplified: always returns stdout handle
    let handle = new_file_handle(gc, LuaFile::stdout());
    Ok(vec![Value::Object(handle)])
}

pub fn io_popen(_args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    Err(LuaError::new("io.popen is not supported"))
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
