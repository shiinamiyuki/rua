//! Lua string library implementation.

use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

type NativeFn = fn(&[Value], &mut Gc) -> Result<Vec<Value>, LuaError>;

/// Extract string bytes from a Value.
fn check_string(args: &[Value], idx: usize, fname: &str) -> Result<Vec<u8>, LuaError> {
    let v = args.get(idx).copied().unwrap_or(Value::Nil);
    match v {
        Value::Object(r) if r.as_object().as_string().is_some() => {
            Ok(r.as_object().as_string().unwrap().as_bytes().to_vec())
        }
        // Numbers are auto-coerced to strings
        Value::Integer(i) => Ok(format!("{i}").into_bytes()),
        Value::Float(f) => Ok(format!("{f}").into_bytes()),
        _ => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (string expected, got {})",
            idx + 1,
            fname,
            v.type_name()
        ))),
    }
}

fn check_integer(args: &[Value], idx: usize, fname: &str) -> Result<i64, LuaError> {
    let v = args.get(idx).copied().unwrap_or(Value::Nil);
    match v {
        Value::Integer(i) => Ok(i),
        Value::Float(f) => {
            let i = f as i64;
            if i as f64 == f {
                Ok(i)
            } else {
                Err(LuaError::new(format!(
                    "bad argument #{} to '{}' (number has no integer representation)",
                    idx + 1,
                    fname
                )))
            }
        }
        _ => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (number expected, got {})",
            idx + 1,
            fname,
            v.type_name()
        ))),
    }
}

/// Resolve a Lua string position (1-based, negative from end).
fn resolve_pos(pos: i64, len: usize) -> usize {
    if pos >= 0 {
        (pos as usize).saturating_sub(1).min(len)
    } else {
        let abs = (-pos) as usize;
        len.saturating_sub(abs)
    }
}

// ── Simple string functions ────────────────────────────────────────

pub fn string_byte(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "byte")?;
    let i = args
        .get(1)
        .and_then(|v| v.as_integer())
        .unwrap_or(1);
    let j = args
        .get(2)
        .and_then(|v| v.as_integer())
        .unwrap_or(i);
    let len = s.len() as i64;
    let start = if i >= 0 { (i - 1).max(0) as usize } else { (len + i).max(0) as usize };
    let end = if j >= 0 { j.min(len) as usize } else { (len + j + 1).max(0) as usize };
    let mut results = Vec::new();
    for idx in start..end {
        results.push(Value::Integer(s[idx] as i64));
    }
    Ok(results)
}

pub fn string_char(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut bytes = Vec::with_capacity(args.len());
    for (i, arg) in args.iter().enumerate() {
        let n = match arg {
            Value::Integer(n) => *n,
            Value::Float(f) => *f as i64,
            _ => {
                return Err(LuaError::new(format!(
                    "bad argument #{} to 'char' (number expected, got {})",
                    i + 1,
                    arg.type_name()
                )))
            }
        };
        if !(0..=255).contains(&n) {
            return Err(LuaError::new(format!(
                "bad argument #{} to 'char' (value out of range)",
                i + 1
            )));
        }
        bytes.push(n as u8);
    }
    let s = gc.new_string(&bytes);
    Ok(vec![Value::Object(s)])
}

pub fn string_len(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "len")?;
    Ok(vec![Value::Integer(s.len() as i64)])
}

pub fn string_sub(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "sub")?;
    let i = args
        .get(1)
        .and_then(|v| v.as_integer())
        .unwrap_or(1);
    let j = args
        .get(2)
        .and_then(|v| v.as_integer())
        .unwrap_or(-1);
    let len = s.len() as i64;
    let start = if i >= 0 { (i - 1).max(0) as usize } else { (len + i).max(0) as usize };
    let end = if j >= 0 { j.min(len) as usize } else { (len + j + 1).max(0) as usize };
    if start >= end || start >= s.len() {
        let r = gc.new_string(b"");
        return Ok(vec![Value::Object(r)]);
    }
    let r = gc.new_string(&s[start..end]);
    Ok(vec![Value::Object(r)])
}

pub fn string_rep(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "rep")?;
    let n = check_integer(args, 1, "rep")?;
    let sep = if args.len() > 2 {
        check_string(args, 2, "rep")?
    } else {
        Vec::new()
    };
    if n <= 0 {
        let r = gc.new_string(b"");
        return Ok(vec![Value::Object(r)]);
    }
    let n = n as usize;
    let mut result = Vec::with_capacity(s.len() * n + sep.len() * n.saturating_sub(1));
    for i in 0..n {
        if i > 0 && !sep.is_empty() {
            result.extend_from_slice(&sep);
        }
        result.extend_from_slice(&s);
    }
    let r = gc.new_string(&result);
    Ok(vec![Value::Object(r)])
}

pub fn string_reverse(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "reverse")?;
    let reversed: Vec<u8> = s.iter().rev().copied().collect();
    let r = gc.new_string(&reversed);
    Ok(vec![Value::Object(r)])
}

pub fn string_lower(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "lower")?;
    let lowered: Vec<u8> = s.iter().map(|b| b.to_ascii_lowercase()).collect();
    let r = gc.new_string(&lowered);
    Ok(vec![Value::Object(r)])
}

pub fn string_upper(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "upper")?;
    let uppered: Vec<u8> = s.iter().map(|b| b.to_ascii_uppercase()).collect();
    let r = gc.new_string(&uppered);
    Ok(vec![Value::Object(r)])
}

// ── string.format ──────────────────────────────────────────────────

pub fn string_format(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fmt = check_string(args, 0, "format")?;
    let mut result = Vec::new();
    let mut arg_idx = 1;
    let mut i = 0;
    while i < fmt.len() {
        if fmt[i] == b'%' {
            i += 1;
            if i >= fmt.len() {
                return Err(LuaError::new("invalid format string (ends with '%')"));
            }
            if fmt[i] == b'%' {
                result.push(b'%');
                i += 1;
                continue;
            }
            // Parse flags
            let spec_start = i - 1;
            let mut flags = String::new();
            while i < fmt.len() && b"-+ #0".contains(&fmt[i]) {
                flags.push(fmt[i] as char);
                i += 1;
            }
            // Parse width
            let mut width = String::new();
            while i < fmt.len() && fmt[i].is_ascii_digit() {
                width.push(fmt[i] as char);
                i += 1;
            }
            // Parse precision
            let mut prec = String::new();
            if i < fmt.len() && fmt[i] == b'.' {
                i += 1;
                while i < fmt.len() && fmt[i].is_ascii_digit() {
                    prec.push(fmt[i] as char);
                    i += 1;
                }
                if prec.is_empty() {
                    prec = "0".to_string();
                }
            }
            if i >= fmt.len() {
                return Err(LuaError::new(format!(
                    "invalid format string near '{}'",
                    String::from_utf8_lossy(&fmt[spec_start..])
                )));
            }
            let spec = fmt[i] as char;
            i += 1;
            let val = args.get(arg_idx).copied().unwrap_or(Value::Nil);
            arg_idx += 1;

            let w: usize = width.parse().unwrap_or(0);
            let p: usize = prec.parse().unwrap_or(6);
            let left = flags.contains('-');
            let plus = flags.contains('+');
            let space = flags.contains(' ');
            let zero = flags.contains('0');

            match spec {
                'd' | 'i' => {
                    let n = val_to_integer(val, "format")?;
                    let sign = if n < 0 {
                        "-"
                    } else if plus {
                        "+"
                    } else if space {
                        " "
                    } else {
                        ""
                    };
                    let abs = (n as i128).unsigned_abs();
                    let digits = format!("{abs}");
                    let s = format!("{sign}{digits}");
                    pad_string(&mut result, &s, w, left, if zero { '0' } else { ' ' }, sign.len());
                }
                'u' => {
                    let n = val_to_integer(val, "format")? as u64;
                    let s = format!("{n}");
                    pad_string(&mut result, &s, w, left, if zero { '0' } else { ' ' }, 0);
                }
                'f' => {
                    let f = val_to_float(val, "format")?;
                    let s = format!("{f:.prec$}", prec = p);
                    if w > 0 {
                        pad_string(&mut result, &s, w, left, if zero { '0' } else { ' ' }, if s.starts_with('-') { 1 } else { 0 });
                    } else {
                        result.extend_from_slice(s.as_bytes());
                    }
                }
                'e' | 'E' => {
                    let f = val_to_float(val, "format")?;
                    let s = if spec == 'e' {
                        format_exp_lower(f, p)
                    } else {
                        format_exp_upper(f, p)
                    };
                    pad_string(&mut result, &s, w, left, ' ', 0);
                }
                'g' | 'G' => {
                    let f = val_to_float(val, "format")?;
                    let s = format_g(f, p, spec == 'G');
                    pad_string(&mut result, &s, w, left, ' ', 0);
                }
                'x' | 'X' => {
                    let n = val_to_integer(val, "format")? as u64;
                    let s = if spec == 'x' {
                        if flags.contains('#') {
                            format!("0x{n:x}")
                        } else {
                            format!("{n:x}")
                        }
                    } else if flags.contains('#') {
                        format!("0X{n:X}")
                    } else {
                        format!("{n:X}")
                    };
                    pad_string(&mut result, &s, w, left, if zero { '0' } else { ' ' }, 0);
                }
                'o' => {
                    let n = val_to_integer(val, "format")? as u64;
                    let s = format!("{n:o}");
                    pad_string(&mut result, &s, w, left, if zero { '0' } else { ' ' }, 0);
                }
                's' => {
                    let s = val_to_string(val);
                    let s = if !prec.is_empty() && s.len() > p {
                        &s[..p]
                    } else {
                        &s
                    };
                    if w > 0 {
                        pad_string(&mut result, s, w, left, ' ', 0);
                    } else {
                        result.extend_from_slice(s.as_bytes());
                    }
                }
                'c' => {
                    let n = val_to_integer(val, "format")?;
                    result.push((n & 0xFF) as u8);
                }
                'q' => {
                    let s = val_to_string(val);
                    result.push(b'"');
                    for &b in s.as_bytes() {
                        match b {
                            b'\\' => result.extend_from_slice(b"\\\\"),
                            b'"' => result.extend_from_slice(b"\\\""),
                            b'\n' => result.extend_from_slice(b"\\n"),
                            b'\r' => result.extend_from_slice(b"\\r"),
                            b'\0' => result.extend_from_slice(b"\\0"),
                            b'\x1a' => result.extend_from_slice(b"\\26"),
                            _ => result.push(b),
                        }
                    }
                    result.push(b'"');
                }
                'p' => {
                    let s = format!("{val}");
                    result.extend_from_slice(s.as_bytes());
                }
                _ => {
                    return Err(LuaError::new(format!(
                        "invalid conversion specifier '%{spec}'"
                    )));
                }
            }
        } else {
            result.push(fmt[i]);
            i += 1;
        }
    }
    let r = gc.new_string(&result);
    Ok(vec![Value::Object(r)])
}

fn val_to_integer(v: Value, fname: &str) -> Result<i64, LuaError> {
    match v {
        Value::Integer(i) => Ok(i),
        Value::Float(f) => Ok(f as i64),
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = std::str::from_utf8(s.as_bytes()).unwrap_or("");
            text.trim()
                .parse::<i64>()
                .map_err(|_| LuaError::new(format!("bad argument to '{fname}'")))
        }
        _ => Err(LuaError::new(format!("bad argument to '{fname}'"))),
    }
}

fn val_to_float(v: Value, fname: &str) -> Result<f64, LuaError> {
    match v {
        Value::Float(f) => Ok(f),
        Value::Integer(i) => Ok(i as f64),
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = std::str::from_utf8(s.as_bytes()).unwrap_or("");
            text.trim()
                .parse::<f64>()
                .map_err(|_| LuaError::new(format!("bad argument to '{fname}'")))
        }
        _ => Err(LuaError::new(format!("bad argument to '{fname}'"))),
    }
}

fn val_to_string(v: Value) -> String {
    format!("{v}")
}

fn pad_string(out: &mut Vec<u8>, s: &str, width: usize, left: bool, pad: char, sign_len: usize) {
    if s.len() >= width {
        out.extend_from_slice(s.as_bytes());
        return;
    }
    let padding = width - s.len();
    if left {
        out.extend_from_slice(s.as_bytes());
        for _ in 0..padding {
            out.push(pad as u8);
        }
    } else if pad == '0' && sign_len > 0 {
        // Put sign before zeros
        out.extend_from_slice(&s.as_bytes()[..sign_len]);
        for _ in 0..padding {
            out.push(b'0');
        }
        out.extend_from_slice(&s.as_bytes()[sign_len..]);
    } else {
        for _ in 0..padding {
            out.push(pad as u8);
        }
        out.extend_from_slice(s.as_bytes());
    }
}

fn format_exp_lower(f: f64, prec: usize) -> String {
    format!("{f:.prec$e}", prec = prec)
}

fn format_exp_upper(f: f64, prec: usize) -> String {
    format!("{f:.prec$E}", prec = prec)
}

fn format_g(f: f64, prec: usize, upper: bool) -> String {
    let p = if prec == 0 { 1 } else { prec };
    // Try both representations
    if f == 0.0 {
        return "0".to_string();
    }
    let exp = if f != 0.0 { f.abs().log10().floor() as i32 } else { 0 };
    if exp < -4 || exp >= p as i32 {
        // Use exponential notation
        let s = if upper {
            format!("{f:.prec$E}", prec = p.saturating_sub(1))
        } else {
            format!("{f:.prec$e}", prec = p.saturating_sub(1))
        };
        s
    } else {
        // Use fixed notation, trim trailing zeros
        let digits = (p as i32 - exp - 1).max(0) as usize;
        let s = format!("{f:.digits$}");
        // Trim trailing zeros after decimal point
        if s.contains('.') {
            let s = s.trim_end_matches('0');
            let s = s.trim_end_matches('.');
            s.to_string()
        } else {
            s
        }
    }
}

// ── Pattern matching engine ────────────────────────────────────────

/// Lua pattern match state.
struct MatchState<'a> {
    source: &'a [u8],
    pattern: &'a [u8],
    captures: Vec<Capture>,
    level: usize,
}

#[derive(Clone, Copy)]
struct Capture {
    start: usize,
    len: CaptureLen,
}

#[derive(Clone, Copy)]
enum CaptureLen {
    Len(usize),
    Position, // for %n position capture
    Unfinished,
}

impl<'a> MatchState<'a> {
    fn new(source: &'a [u8], pattern: &'a [u8]) -> Self {
        MatchState {
            source,
            pattern,
            captures: Vec::new(),
            level: 0,
        }
    }

    /// Match pattern starting at pat_idx against source starting at si.
    /// Returns the end position in source if match succeeds.
    fn match_pattern(&mut self, si: usize, pi: usize) -> Option<usize> {
        self.match_impl(si, pi, 0)
    }

    fn match_impl(&mut self, mut si: usize, mut pi: usize, depth: usize) -> Option<usize> {
        if depth > 200 {
            return None; // recursion limit
        }
        loop {
            if pi >= self.pattern.len() {
                return Some(si);
            }
            match self.pattern[pi] {
                b'(' => {
                    if pi + 1 < self.pattern.len() && self.pattern[pi + 1] == b')' {
                        // Position capture
                        let cap_idx = self.level;
                        self.captures.push(Capture {
                            start: si,
                            len: CaptureLen::Position,
                        });
                        self.level += 1;
                        let result = self.match_impl(si, pi + 2, depth + 1);
                        if result.is_some() {
                            return result;
                        }
                        self.captures.pop();
                        self.level -= 1;
                        return None;
                    } else {
                        let cap_idx = self.level;
                        self.captures.push(Capture {
                            start: si,
                            len: CaptureLen::Unfinished,
                        });
                        self.level += 1;
                        let result = self.match_impl(si, pi + 1, depth + 1);
                        if result.is_some() {
                            return result;
                        }
                        self.captures.pop();
                        self.level -= 1;
                        return None;
                    }
                }
                b')' => {
                    // Close the most recent unfinished capture
                    for i in (0..self.captures.len()).rev() {
                        if matches!(self.captures[i].len, CaptureLen::Unfinished) {
                            self.captures[i].len = CaptureLen::Len(si - self.captures[i].start);
                            let result = self.match_impl(si, pi + 1, depth + 1);
                            if result.is_some() {
                                return result;
                            }
                            self.captures[i].len = CaptureLen::Unfinished;
                            return None;
                        }
                    }
                    return None; // no matching open
                }
                b'$' if pi + 1 == self.pattern.len() => {
                    // Anchor to end
                    if si == self.source.len() {
                        return Some(si);
                    }
                    return None;
                }
                _ => {
                    // Check for quantifier after class
                    let (class_end, class_pi) = self.skip_class(pi);
                    if class_end < self.pattern.len() {
                        match self.pattern[class_end] {
                            b'*' => {
                                return self.match_greedy(si, pi, class_end + 1, depth);
                            }
                            b'+' => {
                                if si < self.source.len()
                                    && self.match_class(self.source[si], pi)
                                {
                                    return self.match_greedy(si + 1, pi, class_end + 1, depth);
                                }
                                return None;
                            }
                            b'-' => {
                                return self.match_lazy(si, pi, class_end + 1, depth);
                            }
                            b'?' => {
                                // Optional
                                if si < self.source.len()
                                    && self.match_class(self.source[si], pi)
                                {
                                    if let Some(r) =
                                        self.match_impl(si + 1, class_end + 1, depth + 1)
                                    {
                                        return Some(r);
                                    }
                                }
                                pi = class_end + 1;
                                continue;
                            }
                            _ => {}
                        }
                    }
                    // No quantifier, single match
                    if si < self.source.len() && self.match_class(self.source[si], pi) {
                        si += 1;
                        pi = class_end;
                        continue;
                    }
                    return None;
                }
            }
        }
    }

    /// Greedy match: match as many chars as possible, then try rest.
    fn match_greedy(
        &mut self,
        si: usize,
        class_pi: usize,
        rest_pi: usize,
        depth: usize,
    ) -> Option<usize> {
        let mut count = 0;
        while si + count < self.source.len()
            && self.match_class(self.source[si + count], class_pi)
        {
            count += 1;
        }
        // Try from longest to shortest
        for c in (0..=count).rev() {
            if let Some(r) = self.match_impl(si + c, rest_pi, depth + 1) {
                return Some(r);
            }
        }
        None
    }

    /// Lazy match: match as few chars as possible.
    fn match_lazy(
        &mut self,
        si: usize,
        class_pi: usize,
        rest_pi: usize,
        depth: usize,
    ) -> Option<usize> {
        let mut count = 0;
        loop {
            if let Some(r) = self.match_impl(si + count, rest_pi, depth + 1) {
                return Some(r);
            }
            if si + count < self.source.len()
                && self.match_class(self.source[si + count], class_pi)
            {
                count += 1;
            } else {
                return None;
            }
        }
    }

    /// Skip past a single pattern class at pi, return the index after the class.
    fn skip_class(&self, pi: usize) -> (usize, usize) {
        if pi >= self.pattern.len() {
            return (pi, pi);
        }
        match self.pattern[pi] {
            b'%' => {
                if pi + 1 < self.pattern.len() {
                    if self.pattern[pi + 1] == b'b' {
                        // %bxy
                        (pi + 4, pi)
                    } else if self.pattern[pi + 1] == b'f' {
                        // %f[set] — frontier pattern
                        if pi + 2 < self.pattern.len() && self.pattern[pi + 2] == b'[' {
                            let end = self.find_set_end(pi + 2);
                            (end, pi)
                        } else {
                            (pi + 2, pi)
                        }
                    } else {
                        (pi + 2, pi)
                    }
                } else {
                    (pi + 1, pi)
                }
            }
            b'[' => {
                let end = self.find_set_end(pi);
                (end, pi)
            }
            _ => (pi + 1, pi),
        }
    }

    /// Find the end of a character set [...].
    fn find_set_end(&self, pi: usize) -> usize {
        let mut i = pi + 1;
        if i < self.pattern.len() && self.pattern[i] == b'^' {
            i += 1;
        }
        if i < self.pattern.len() && self.pattern[i] == b']' {
            i += 1; // ] at start is literal
        }
        while i < self.pattern.len() {
            if self.pattern[i] == b']' {
                return i + 1;
            }
            if self.pattern[i] == b'%' && i + 1 < self.pattern.len() {
                i += 2;
            } else {
                i += 1;
            }
        }
        i
    }

    /// Match a single source byte against a pattern class starting at pi.
    fn match_class(&self, ch: u8, pi: usize) -> bool {
        if pi >= self.pattern.len() {
            return false;
        }
        match self.pattern[pi] {
            b'%' => {
                if pi + 1 >= self.pattern.len() {
                    return false;
                }
                let cls = self.pattern[pi + 1];
                if cls == b'b' {
                    // %bxy is not a single-character class
                    return false;
                }
                match_char_class(ch, cls)
            }
            b'[' => self.match_set(ch, pi),
            b'.' => true,
            c => ch == c,
        }
    }

    /// Match a byte against a character set [...]
    fn match_set(&self, ch: u8, pi: usize) -> bool {
        let mut i = pi + 1;
        let negate = if i < self.pattern.len() && self.pattern[i] == b'^' {
            i += 1;
            true
        } else {
            false
        };
        let mut matched = false;
        // First ] is literal
        if i < self.pattern.len() && self.pattern[i] == b']' {
            if ch == b']' {
                matched = true;
            }
            i += 1;
        }
        while i < self.pattern.len() && self.pattern[i] != b']' {
            if self.pattern[i] == b'%' && i + 1 < self.pattern.len() {
                if match_char_class(ch, self.pattern[i + 1]) {
                    matched = true;
                }
                i += 2;
            } else if i + 2 < self.pattern.len() && self.pattern[i + 1] == b'-' {
                if ch >= self.pattern[i] && ch <= self.pattern[i + 2] {
                    matched = true;
                }
                i += 3;
            } else {
                if ch == self.pattern[i] {
                    matched = true;
                }
                i += 1;
            }
        }
        if negate {
            !matched
        } else {
            matched
        }
    }
}

/// Match a byte against a Lua character class like %a, %d, etc.
fn match_char_class(ch: u8, cls: u8) -> bool {
    let result = match cls.to_ascii_lowercase() {
        b'a' => ch.is_ascii_alphabetic(),
        b'c' => ch.is_ascii_control(),
        b'd' => ch.is_ascii_digit(),
        b'g' => ch.is_ascii_graphic(),
        b'l' => ch.is_ascii_lowercase(),
        b'p' => ch.is_ascii_punctuation(),
        b's' => ch.is_ascii_whitespace(),
        b'u' => ch.is_ascii_uppercase(),
        b'w' => ch.is_ascii_alphanumeric(),
        b'x' => ch.is_ascii_hexdigit(),
        _ => return ch == cls, // literal match (e.g., %., %[, etc.)
    };
    if cls.is_ascii_uppercase() {
        !result // uppercase = complement
    } else {
        result
    }
}

// ── String library functions ───────────────────────────────────────

pub fn string_find(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "find")?;
    let pat = check_string(args, 1, "find")?;
    let init = args
        .get(2)
        .and_then(|v| v.as_integer())
        .unwrap_or(1);
    let plain = args
        .get(3)
        .map(|v| v.is_truthy())
        .unwrap_or(false);

    let start = if init >= 1 {
        (init - 1) as usize
    } else {
        s.len().saturating_sub((-init) as usize)
    };
    let start = start.min(s.len());

    if plain {
        // Plain search
        if let Some(pos) = find_plain(&s[start..], &pat) {
            let abs_pos = start + pos;
            Ok(vec![
                Value::Integer(abs_pos as i64 + 1),
                Value::Integer(abs_pos as i64 + pat.len() as i64),
            ])
        } else {
            Ok(vec![Value::Nil])
        }
    } else {
        // Pattern search
        let anchored = !pat.is_empty() && pat[0] == b'^';
        let pat_slice = if anchored { &pat[1..] } else { &pat };

        let search_start = start;
        if anchored {
            let mut ms = MatchState::new(&s, pat_slice);
            if let Some(end) = ms.match_pattern(search_start, 0) {
                let mut results = vec![
                    Value::Integer(search_start as i64 + 1),
                    Value::Integer(end as i64),
                ];
                for cap in &ms.captures {
                    results.push(capture_to_value(cap, &s, gc));
                }
                return Ok(results);
            }
            return Ok(vec![Value::Nil]);
        }

        for si in search_start..=s.len() {
            let mut ms = MatchState::new(&s, pat_slice);
            if let Some(end) = ms.match_pattern(si, 0) {
                let mut results = vec![
                    Value::Integer(si as i64 + 1),
                    Value::Integer(end as i64),
                ];
                for cap in &ms.captures {
                    results.push(capture_to_value(cap, &s, gc));
                }
                return Ok(results);
            }
        }
        Ok(vec![Value::Nil])
    }
}

pub fn string_match(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "match")?;
    let pat = check_string(args, 1, "match")?;
    let init = args
        .get(2)
        .and_then(|v| v.as_integer())
        .unwrap_or(1);

    let start = if init >= 1 {
        (init - 1) as usize
    } else {
        s.len().saturating_sub((-init) as usize)
    };
    let start = start.min(s.len());

    let anchored = !pat.is_empty() && pat[0] == b'^';
    let pat_slice = if anchored { &pat[1..] } else { &pat };

    if anchored {
        let mut ms = MatchState::new(&s, pat_slice);
        if let Some(end) = ms.match_pattern(start, 0) {
            return Ok(get_captures(&ms, &s, start, end, gc));
        }
        return Ok(vec![Value::Nil]);
    }

    for si in start..=s.len() {
        let mut ms = MatchState::new(&s, pat_slice);
        if let Some(end) = ms.match_pattern(si, 0) {
            return Ok(get_captures(&ms, &s, si, end, gc));
        }
    }
    Ok(vec![Value::Nil])
}

pub fn string_gmatch(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "gmatch")?;
    let pat = check_string(args, 1, "gmatch")?;

    // Collect all matches
    let anchored = !pat.is_empty() && pat[0] == b'^';
    let pat_slice: Vec<u8> = if anchored { pat[1..].to_vec() } else { pat.clone() };

    let mut matches: Vec<Vec<Value>> = Vec::new();
    let mut si = 0;
    while si <= s.len() {
        let mut ms = MatchState::new(&s, &pat_slice);
        if let Some(end) = ms.match_pattern(si, 0) {
            let caps = get_captures(&ms, &s, si, end, gc);
            matches.push(caps);
            if end == si {
                si += 1; // prevent infinite loop on empty match
            } else {
                si = end;
            }
            if anchored {
                break;
            }
        } else {
            si += 1;
        }
    }

    // Return an iterator function
    let match_idx = std::cell::Cell::new(0usize);
    let iter_fn = move |_args: &[Value], _gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let idx = match_idx.get();
        if idx < matches.len() {
            match_idx.set(idx + 1);
            Ok(matches[idx].clone())
        } else {
            Ok(vec![Value::Nil])
        }
    };

    let closure = crate::closure::Closure::new_native_dyn("gmatch_iter".to_string(), iter_fn);
    let gc_ref = gc.new_closure(closure);
    Ok(vec![Value::Object(gc_ref)])
}

pub fn string_gsub(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, 0, "gsub")?;
    let pat = check_string(args, 1, "gsub")?;
    let repl = args.get(2).copied().unwrap_or(Value::Nil);
    let max_s = args
        .get(3)
        .and_then(|v| v.as_integer())
        .map(|n| n as usize)
        .unwrap_or(usize::MAX);

    let anchored = !pat.is_empty() && pat[0] == b'^';
    let pat_slice = if anchored { &pat[1..] } else { &pat[..] };

    let mut result = Vec::new();
    let mut si = 0;
    let mut count = 0;

    while si <= s.len() && count < max_s {
        let mut ms = MatchState::new(&s, pat_slice);
        if let Some(end) = ms.match_pattern(si, 0) {
            count += 1;
            // Build replacement
            match repl {
                Value::Object(r) if r.as_object().as_string().is_some() => {
                    let repl_str = r.as_object().as_string().unwrap().as_bytes().to_vec();
                    apply_string_replacement(&mut result, &repl_str, &ms, &s, si, end);
                }
                _ => {
                    // For non-string replacements, use whole match
                    let whole = &s[si..end];
                    result.extend_from_slice(whole);
                }
            }
            if end == si {
                if si < s.len() {
                    result.push(s[si]);
                }
                si += 1;
            } else {
                si = end;
            }
            if anchored {
                break;
            }
        } else {
            if si < s.len() {
                result.push(s[si]);
            }
            si += 1;
        }
    }
    // Append remaining
    if si <= s.len() {
        result.extend_from_slice(&s[si..]);
    }

    let r = gc.new_string(&result);
    Ok(vec![Value::Object(r), Value::Integer(count as i64)])
}

fn apply_string_replacement(
    result: &mut Vec<u8>,
    repl: &[u8],
    ms: &MatchState,
    source: &[u8],
    match_start: usize,
    match_end: usize,
) {
    let mut i = 0;
    while i < repl.len() {
        if repl[i] == b'%' && i + 1 < repl.len() {
            let c = repl[i + 1];
            if c.is_ascii_digit() {
                let idx = (c - b'0') as usize;
                if idx == 0 {
                    // %0 = whole match
                    result.extend_from_slice(&source[match_start..match_end]);
                } else if idx <= ms.captures.len() {
                    let cap = &ms.captures[idx - 1];
                    match cap.len {
                        CaptureLen::Len(len) => {
                            result.extend_from_slice(&source[cap.start..cap.start + len]);
                        }
                        CaptureLen::Position => {
                            let s = format!("{}", cap.start + 1);
                            result.extend_from_slice(s.as_bytes());
                        }
                        CaptureLen::Unfinished => {}
                    }
                }
                i += 2;
            } else if c == b'%' {
                result.push(b'%');
                i += 2;
            } else {
                result.push(c);
                i += 2;
            }
        } else {
            result.push(repl[i]);
            i += 1;
        }
    }
}

// ── Helpers ────────────────────────────────────────────────────────

fn find_plain(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() {
        return Some(0);
    }
    haystack
        .windows(needle.len())
        .position(|w| w == needle)
}

fn capture_to_value(cap: &Capture, source: &[u8], gc: &mut Gc) -> Value {
    match cap.len {
        CaptureLen::Len(len) => {
            let s = gc.new_string(&source[cap.start..cap.start + len]);
            Value::Object(s)
        }
        CaptureLen::Position => Value::Integer(cap.start as i64 + 1),
        CaptureLen::Unfinished => Value::Nil,
    }
}

fn get_captures(ms: &MatchState, source: &[u8], start: usize, end: usize, gc: &mut Gc) -> Vec<Value> {
    if ms.captures.is_empty() {
        // No explicit captures: return whole match
        let s = gc.new_string(&source[start..end]);
        vec![Value::Object(s)]
    } else {
        ms.captures
            .iter()
            .map(|cap| capture_to_value(cap, source, gc))
            .collect()
    }
}

// ── Public API ─────────────────────────────────────────────────────

pub fn string_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("byte", string_byte as NativeFn),
        ("char", string_char),
        ("find", string_find),
        ("format", string_format),
        ("gmatch", string_gmatch),
        ("gsub", string_gsub),
        ("len", string_len),
        ("lower", string_lower),
        ("match", string_match),
        ("rep", string_rep),
        ("reverse", string_reverse),
        ("sub", string_sub),
        ("upper", string_upper),
    ]
}
