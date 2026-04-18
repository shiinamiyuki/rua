//! utf8 library: UTF-8 support functions.

use crate::closure::{Closure, NativeFn};
use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

/// utf8.char(···) — Convert codepoints to a UTF-8 string.
pub fn utf8_char(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut buf = String::new();
    for (i, arg) in args.iter().enumerate() {
        let cp = match arg {
            Value::Integer(n) => *n,
            Value::Float(n) => *n as i64,
            _ => return Err(LuaError::new(format!(
                "bad argument #{} to 'char' (number expected)", i + 1))),
        };
        if cp < 0 || cp > 0x7FFF_FFFF {
            return Err(LuaError::new(format!("bad argument #{} to 'char' (value out of range)", i + 1)));
        }
        match char::from_u32(cp as u32) {
            Some(c) => buf.push(c),
            None => return Err(LuaError::new(format!(
                "bad argument #{} to 'char' (value out of range)", i + 1))),
        }
    }
    Ok(vec![Value::Object(gc.new_string(buf.as_bytes()))])
}

/// utf8.codepoint(s [, i [, j]]) — Return codepoints from s[i] to s[j].
pub fn utf8_codepoint(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            r.as_object().as_string().unwrap().as_bytes()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'codepoint' (string expected)")),
    };

    let len = s.len() as i64;
    let i = match args.get(1) {
        Some(Value::Integer(n)) => *n,
        Some(Value::Float(n)) => *n as i64,
        None => 1,
        _ => return Err(LuaError::new("bad argument #2 to 'codepoint' (number expected)")),
    };
    let j = match args.get(2) {
        Some(Value::Integer(n)) => *n,
        Some(Value::Float(n)) => *n as i64,
        None => i,
        _ => return Err(LuaError::new("bad argument #3 to 'codepoint' (number expected)")),
    };

    // Convert 1-based Lua positions to byte offsets
    let start = if i < 0 { (len + i) as usize } else { (i - 1) as usize };
    let end = if j < 0 { (len + j) as usize } else { (j - 1) as usize };

    if start > s.len() || end >= s.len() || start > end + 1 {
        return Err(LuaError::new("bad argument to 'codepoint' (out of bounds)"));
    }

    // Note: i and j are character positions, not byte positions in standard Lua.
    // But the common usage treats them as byte positions. We'll iterate UTF-8 chars.
    let slice = &s[start..=end.min(s.len() - 1)];
    let text = std::str::from_utf8(slice)
        .map_err(|_| LuaError::new("invalid UTF-8 code"))?;

    let results: Vec<Value> = text.chars()
        .map(|c| Value::Integer(c as i64))
        .collect();
    Ok(results)
}

/// utf8.codes(s) — Iterator factory for UTF-8 codepoints.
pub fn utf8_codes(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s_ref = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => *r,
        _ => return Err(LuaError::new("bad argument #1 to 'codes' (string expected)")),
    };

    // Capture the string ref and a byte offset position
    let pos = std::cell::Cell::new(0usize);
    let iter_fn = move |_args: &[Value], _gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let s = s_ref.as_object().as_string()
            .ok_or_else(|| LuaError::new("string expected"))?;
        let bytes = s.as_bytes();
        let p = pos.get();
        if p >= bytes.len() {
            return Ok(vec![Value::Nil]);
        }
        let slice = &bytes[p..];
        let text = std::str::from_utf8(slice)
            .map_err(|_| LuaError::new("invalid UTF-8 code"))?;
        match text.chars().next() {
            Some(c) => {
                let byte_pos = p + 1; // 1-based position
                pos.set(p + c.len_utf8());
                Ok(vec![Value::Integer(byte_pos as i64), Value::Integer(c as i64)])
            }
            None => Ok(vec![Value::Nil]),
        }
    };

    let closure = Closure::new_native_dyn("utf8.codes iterator".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    Ok(vec![Value::Object(closure_ref)])
}

/// utf8.len(s [, i [, j]]) — Count UTF-8 characters in s[i..j].
pub fn utf8_len(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            r.as_object().as_string().unwrap().as_bytes()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'len' (string expected)")),
    };

    let len = s.len() as i64;
    let i = match args.get(1) {
        Some(Value::Integer(n)) => *n,
        Some(Value::Float(n)) => *n as i64,
        None => 1,
        _ => return Err(LuaError::new("bad argument #2 to 'len' (number expected)")),
    };
    let j = match args.get(2) {
        Some(Value::Integer(n)) => *n,
        Some(Value::Float(n)) => *n as i64,
        None => -1,
        _ => return Err(LuaError::new("bad argument #3 to 'len' (number expected)")),
    };

    let start = if i < 0 { (len + i) as usize } else { (i - 1) as usize };
    let end = if j < 0 { (len + j + 1) as usize } else { j as usize };

    if start > s.len() || end > s.len() {
        return Err(LuaError::new("bad argument to 'len' (out of bounds)"));
    }

    let slice = &s[start..end];
    match std::str::from_utf8(slice) {
        Ok(text) => Ok(vec![Value::Integer(text.chars().count() as i64)]),
        Err(e) => {
            // Return nil, position of first invalid byte
            let err_pos = start + e.valid_up_to() + 1; // 1-based
            Ok(vec![Value::Nil, Value::Integer(err_pos as i64)])
        }
    }
}

/// utf8.offset(s, n [, i]) — Returns byte position of the n-th character.
pub fn utf8_offset(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            r.as_object().as_string().unwrap().as_bytes()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'offset' (string expected)")),
    };

    let n = match args.get(1) {
        Some(Value::Integer(v)) => *v,
        Some(Value::Float(v)) => *v as i64,
        _ => return Err(LuaError::new("bad argument #2 to 'offset' (number expected)")),
    };

    let len = s.len();
    // Default i: if n >= 0, start from byte 1; if n < 0, start from byte past end
    let i = match args.get(2) {
        Some(Value::Integer(v)) => *v,
        Some(Value::Float(v)) => *v as i64,
        None => if n >= 0 { 1 } else { (len + 1) as i64 },
        _ => return Err(LuaError::new("bad argument #3 to 'offset' (number expected)")),
    };

    // Convert to 0-based byte offset
    let mut pos = if i < 0 { (len as i64 + i) as usize } else { (i - 1) as usize };

    if pos > len {
        return Err(LuaError::new("bad argument #3 to 'offset' (position out of bounds)"));
    }

    if n == 0 {
        // Move back to start of current character
        while pos > 0 && is_continuation_byte(s[pos]) {
            pos -= 1;
        }
        return Ok(vec![Value::Integer((pos + 1) as i64)]);
    }

    if n > 0 {
        // Move forward n-1 characters (we're at a char start)
        // First, ensure we're at a char boundary
        let mut count = n - 1;
        while count > 0 && pos < len {
            pos += 1;
            while pos < len && is_continuation_byte(s[pos]) {
                pos += 1;
            }
            count -= 1;
        }
        if count > 0 {
            return Ok(vec![Value::Nil]);
        }
    } else {
        // n < 0: move backward
        let mut count = -n;
        while count > 0 && pos > 0 {
            pos -= 1;
            while pos > 0 && is_continuation_byte(s[pos]) {
                pos -= 1;
            }
            count -= 1;
        }
        if count > 0 {
            return Ok(vec![Value::Nil]);
        }
    }

    Ok(vec![Value::Integer((pos + 1) as i64)])
}

#[inline]
fn is_continuation_byte(b: u8) -> bool {
    b & 0xC0 == 0x80
}

/// Return utf8 library functions.
pub fn utf8_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("char", utf8_char as NativeFn),
        ("codepoint", utf8_codepoint),
        ("codes", utf8_codes),
        ("len", utf8_len),
        ("offset", utf8_offset),
    ]
}

/// The charpattern constant: "[\0-\x7F\xC2-\xFD][\x80-\xBF]*"
pub const UTF8_CHARPATTERN: &[u8] = b"[\0-\x7F\xC2-\xFD][\x80-\xBF]*";
