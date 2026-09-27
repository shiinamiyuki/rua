//! utf8 library: UTF-8 support functions.

use crate::closure::{Closure, NativeFn};
use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

const MAXUNICODE: u32 = 0x10FFFF;
const MAXUTF: u32 = 0x7FFF_FFFF;

#[inline]
fn is_cont(b: u8) -> bool {
    b & 0xC0 == 0x80
}

/// Decode one UTF-8 sequence starting at `pos`, mirroring `utf8_decode`
/// from `lutf8lib.c`.  Returns the codepoint and the position just past
/// the sequence, or `None` if the sequence is invalid.
fn utf8_decode(s: &[u8], pos: usize, strict: bool) -> Option<(u32, usize)> {
    const LIMITS: [u32; 6] = [u32::MAX, 0x80, 0x800, 0x10000, 0x200000, 0x4000000];
    let first = s.get(pos).copied().unwrap_or(0);
    let (res, extra);
    if first < 0x80 {
        res = first as u32;
        extra = 0;
    } else {
        let mut c = first as u32;
        let mut r: u32 = 0;
        let mut count = 0usize;
        while c & 0x40 != 0 {
            count += 1;
            let cc = s.get(pos + count).copied().unwrap_or(0);
            if !is_cont(cc) {
                return None;
            }
            r = (r << 6) | (cc as u32 & 0x3F);
            c <<= 1;
        }
        r |= (c & 0x7F) << (count * 5);
        if count > 5 || r > MAXUTF || r < LIMITS[count] {
            return None;
        }
        res = r;
        extra = count;
    }
    if strict && (res > MAXUNICODE || (0xD800..=0xDFFF).contains(&res)) {
        return None;
    }
    Some((res, pos + 1 + extra))
}

/// Encode a codepoint (up to `MAXUTF`) as UTF-8, mirroring `luaO_utf8esc`.
fn utf8_encode(x: u32) -> Vec<u8> {
    if x < 0x80 {
        return vec![x as u8];
    }
    let mut buff = [0u8; 6];
    let mut n = 0usize;
    let mut x = x;
    let mut mfb: u32 = 0x3F;
    loop {
        buff[5 - n] = 0x80 | (x & 0x3F) as u8;
        n += 1;
        x >>= 6;
        mfb >>= 1;
        if x <= mfb {
            break;
        }
    }
    buff[5 - n] = ((!mfb << 1) | x) as u8;
    n += 1;
    buff[6 - n..].to_vec()
}

/// Translate a relative string position: negative means back from end.
fn posrelat(pos: i64, len: usize) -> i64 {
    if pos >= 0 {
        pos
    } else if pos.unsigned_abs() > len as u64 {
        0
    } else {
        len as i64 + pos + 1
    }
}

fn check_string<'a>(args: &'a [Value], name: &str) -> Result<&'a [u8], LuaError> {
    match args.first() {
        Some(Value::Object(r)) => match r.as_object().as_string() {
            Some(s) => Ok(s.as_bytes()),
            None => Err(LuaError::new(format!(
                "bad argument #1 to '{}' (string expected, got {})",
                name,
                args[0].type_name()
            ))),
        },
        Some(v) => Err(LuaError::new(format!(
            "bad argument #1 to '{}' (string expected, got {})",
            name,
            v.type_name()
        ))),
        None => Err(LuaError::new(format!(
            "bad argument #1 to '{}' (string expected, got no value)",
            name
        ))),
    }
}

fn check_integer(args: &[Value], idx: usize, name: &str) -> Result<i64, LuaError> {
    match args.get(idx) {
        Some(Value::Integer(n)) => Ok(*n),
        Some(Value::Float(f)) if f.floor() == *f => Ok(*f as i64),
        Some(v) => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (number expected, got {})",
            idx + 1,
            name,
            v.type_name()
        ))),
        None => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (number expected, got no value)",
            idx + 1,
            name
        ))),
    }
}

fn opt_integer(args: &[Value], idx: usize, def: i64, name: &str) -> Result<i64, LuaError> {
    if args.get(idx).is_none() {
        Ok(def)
    } else {
        check_integer(args, idx, name)
    }
}

fn lax_arg(args: &[Value], idx: usize) -> bool {
    args.get(idx).map(|v| v.is_truthy()).unwrap_or(false)
}

/// utf8.char(···) — Convert codepoints to a UTF-8 string.
pub fn utf8_char(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut buf = Vec::new();
    for i in 0..args.len() {
        let code = check_integer(args, i, "char")?;
        if code < 0 || code as u64 > MAXUTF as u64 {
            return Err(LuaError::new(format!(
                "bad argument #{} to 'char' (value out of range)",
                i + 1
            )));
        }
        buf.extend_from_slice(&utf8_encode(code as u32));
    }
    Ok(vec![Value::Object(gc.new_string(&buf))])
}

/// utf8.codepoint(s [, i [, j [, lax]]]) — Return codepoints from s[i] to s[j].
pub fn utf8_codepoint(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, "codepoint")?;
    let len = s.len();
    let posi = posrelat(opt_integer(args, 1, 1, "codepoint")?, len);
    let pose = posrelat(opt_integer(args, 2, posi, "codepoint")?, len);
    let lax = lax_arg(args, 3);
    if posi < 1 {
        return Err(LuaError::new(
            "bad argument #2 to 'codepoint' (out of bounds)",
        ));
    }
    if pose > len as i64 {
        return Err(LuaError::new(
            "bad argument #3 to 'codepoint' (out of bounds)",
        ));
    }
    if posi > pose {
        return Ok(vec![]);
    }
    let mut results = Vec::new();
    let mut pos = (posi - 1) as usize;
    let end = pose as usize;
    while pos < end {
        match utf8_decode(s, pos, !lax) {
            Some((code, next)) => {
                results.push(Value::Integer(code as i64));
                pos = next;
            }
            None => return Err(LuaError::new("invalid UTF-8 code")),
        }
    }
    Ok(results)
}

/// utf8.codes(s [, lax]) — Stateless iterator over UTF-8 codepoints.
pub fn utf8_codes(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s_ref = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => *r,
        _ => return Err(LuaError::new("bad argument #1 to 'codes' (string expected)")),
    };
    let lax = lax_arg(args, 1);
    {
        let r = s_ref.as_object().as_string().unwrap();
        let bytes = r.as_bytes();
        if bytes.first().copied().map(is_cont).unwrap_or(false) {
            return Err(LuaError::new(
                "bad argument #1 to 'codes' (invalid UTF-8 code)",
            ));
        }
    }

    let iter_fn = move |args: &[Value], _gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let s = check_string(args, "codes iterator")?;
        let len = s.len();
        let n = match args.get(1) {
            Some(Value::Integer(n)) => *n,
            Some(Value::Float(f)) => *f as i64,
            _ => 0,
        };
        if n < 0 {
            return Ok(vec![Value::Nil]);
        }
        let mut n = n as usize;
        if n < len {
            while n < len && is_cont(s[n]) {
                n += 1;
            }
        }
        if n >= len {
            return Ok(vec![Value::Nil]);
        }
        match utf8_decode(s, n, !lax) {
            Some((code, next)) => {
                if next < len && is_cont(s[next]) {
                    return Err(LuaError::new("invalid UTF-8 code"));
                }
                Ok(vec![
                    Value::Integer((n + 1) as i64),
                    Value::Integer(code as i64),
                ])
            }
            None => Err(LuaError::new("invalid UTF-8 code")),
        }
    };

    let closure = Closure::new_native_dyn("utf8.codes iterator".into(), iter_fn);
    let closure_ref = gc.new_closure(closure);
    Ok(vec![
        Value::Object(closure_ref),
        Value::Object(s_ref),
        Value::Integer(0),
    ])
}

/// utf8.len(s [, i [, j [, lax]]]) — Count UTF-8 characters in s[i..j].
pub fn utf8_len(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, "len")?;
    let len = s.len();
    let posi = posrelat(opt_integer(args, 1, 1, "len")?, len);
    let posj = posrelat(opt_integer(args, 2, -1, "len")?, len);
    let lax = lax_arg(args, 3);
    if !(1 <= posi && posi - 1 <= len as i64) {
        return Err(LuaError::new(
            "bad argument #2 to 'len' (initial position out of bounds)",
        ));
    }
    if posj - 1 >= len as i64 {
        return Err(LuaError::new(
            "bad argument #3 to 'len' (final position out of bounds)",
        ));
    }
    let mut pos = posi - 1;
    let mut n = 0i64;
    while pos <= posj - 1 {
        match utf8_decode(s, pos as usize, !lax) {
            Some((_, next)) => {
                pos = next as i64;
                n += 1;
            }
            None => return Ok(vec![Value::Nil, Value::Integer(pos + 1)]),
        }
    }
    Ok(vec![Value::Integer(n)])
}

/// utf8.offset(s, n [, i]) — Returns initial and final byte positions of
/// the n-th character.
pub fn utf8_offset(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s = check_string(args, "offset")?;
    let len = s.len();
    let n = check_integer(args, 1, "offset")?;
    let def = if n >= 0 { 1 } else { len as i64 + 1 };
    let posi = posrelat(opt_integer(args, 2, def, "offset")?, len);
    if !(1 <= posi && posi - 1 <= len as i64) {
        return Err(LuaError::new(
            "bad argument #3 to 'offset' (position out of bounds)",
        ));
    }
    let mut pos = (posi - 1) as usize;
    if n == 0 {
        while pos > 0 && is_cont(s.get(pos).copied().unwrap_or(0)) {
            pos -= 1;
        }
    } else {
        if is_cont(s.get(pos).copied().unwrap_or(0)) {
            return Err(LuaError::new(
                "initial position is a continuation byte",
            ));
        }
        if n < 0 {
            let mut count = n;
            while count < 0 && pos > 0 {
                loop {
                    pos -= 1;
                    if pos == 0 || !is_cont(s[pos]) {
                        break;
                    }
                }
                count += 1;
            }
            if count != 0 {
                return Ok(vec![Value::Nil]);
            }
        } else {
            let mut count = n - 1;
            while count > 0 && pos < len {
                loop {
                    pos += 1;
                    if pos >= len || !is_cont(s[pos]) {
                        break;
                    }
                }
                count -= 1;
            }
            if count != 0 {
                return Ok(vec![Value::Nil]);
            }
        }
    }
    let first = pos + 1;
    if s.get(pos).copied().unwrap_or(0) & 0x80 != 0 {
        if is_cont(s[pos]) {
            return Err(LuaError::new(
                "initial position is a continuation byte",
            ));
        }
        while is_cont(s.get(pos + 1).copied().unwrap_or(0)) {
            pos += 1;
        }
    }
    Ok(vec![Value::Integer(first as i64), Value::Integer((pos + 1) as i64)])
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
