//! Lua string library implementation.

use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

type NativeFn = fn(&[Value], &mut Gc) -> Result<Vec<Value>, LuaError>;

/// Extract string bytes from a Value.
pub(crate) fn check_string(args: &[Value], idx: usize, fname: &str) -> Result<Vec<u8>, LuaError> {
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
    let total = (s.len() as u128) * (n as u128)
        + (sep.len() as u128) * (n as u128 - 1);
    if total > (isize::MAX as u128) {
        return Err(LuaError::new("resulting string too large"));
    }
    let mut result = Vec::with_capacity(total as usize);
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

// ── string.format (port of str_format from lstrlib.c) ─────────────

#[derive(Clone, Copy)]
pub struct FmtSpec {
    minus: bool,
    plus: bool,
    space: bool,
    hash: bool,
    zero: bool,
    width: usize,
    prec: Option<usize>,
    spec: u8,
}

/// Entry point mirroring `str_format`: `args` are the values *after* the
/// format string; `tostring` resolves `%s` (honoring `__tostring`).
pub fn format_values(
    fmt: &[u8],
    args: &[Value],
    tostring: &mut dyn FnMut(Value) -> Result<Vec<u8>, LuaError>,
) -> Result<Vec<u8>, LuaError> {
    let mut result = Vec::new();
    let mut arg = 0usize;
    let mut i = 0usize;
    while i < fmt.len() {
        if fmt[i] != b'%' {
            result.push(fmt[i]);
            i += 1;
            continue;
        }
        i += 1;
        if i < fmt.len() && fmt[i] == b'%' {
            result.push(b'%');
            i += 1;
            continue;
        }
        let form_start = i - 1;
        let mut sp = FmtSpec {
            minus: false,
            plus: false,
            space: false,
            hash: false,
            zero: false,
            width: 0,
            prec: None,
            spec: 0,
        };
        while i < fmt.len() {
            match fmt[i] {
                b'-' => sp.minus = true,
                b'+' => sp.plus = true,
                b' ' => sp.space = true,
                b'#' => sp.hash = true,
                b'0' => sp.zero = true,
                _ => break,
            }
            i += 1;
        }
        let width_start = i;
        while i < fmt.len() && fmt[i].is_ascii_digit() {
            i += 1;
        }
        let width_digits = i - width_start;
        let mut prec_digits = 0usize;
        if i < fmt.len() && fmt[i] == b'.' {
            i += 1;
            let ps = i;
            while i < fmt.len() && fmt[i].is_ascii_digit() {
                i += 1;
            }
            prec_digits = i - ps;
        }
        // `getformat` rejects overly long specifications.
        if i - form_start + 1 > 21 {
            return Err(LuaError::new("invalid format (too long)"));
        }
        if i >= fmt.len() || !fmt[i].is_ascii_alphabetic() {
            let end = i.min(fmt.len());
            return Err(LuaError::new(format!(
                "invalid conversion specification: '{}'",
                String::from_utf8_lossy(&fmt[form_start..end])
            )));
        }
        sp.spec = fmt[i];
        i += 1;
        let form = fmt[form_start..i].to_vec();
        // `checkformat` reads at most two digits for width and precision.
        if width_digits > 2 || prec_digits > 2 {
            return Err(bad_spec(&form));
        }
        if width_digits > 0 {
            sp.width = String::from_utf8_lossy(&fmt[width_start..width_start + width_digits])
                .parse()
                .unwrap_or(0);
        }
        if fmt[form_start..i].contains(&b'.') {
            let dot = fmt[form_start..i].iter().position(|&b| b == b'.').unwrap() + form_start;
            let pstr = &fmt[dot + 1..i - 1];
            sp.prec = Some(
                String::from_utf8_lossy(pstr).parse().unwrap_or(0),
            );
        }

        // Fetch the argument (position includes the format string itself).
        if arg >= args.len() {
            return Err(LuaError::new(format!(
                "bad argument #{} to 'format' (no value)",
                arg + 2
            )));
        }
        let val = args[arg];
        let puc_arg = arg + 2;
        arg += 1;

        let text = match sp.spec {
            b'c' => {
                check_format(&sp, &form)?;
                let n = val_to_integer(val, "format")?;
                let b = (n as u64 & 0xFF) as u8;
                pad_bytes(&[b], sp.width, sp.minus)
            }
            b'd' | b'i' | b'u' | b'o' | b'x' | b'X' => {
                check_format(&sp, &form)?;
                let n = val_to_integer(val, "format")?;
                if matches!(sp.spec, b'u' | b'o' | b'x' | b'X') && sp.space {
                    // ' ' is not a valid flag for unsigned conversions
                    return Err(bad_spec(&form));
                }
                conv_integer(n, &sp)
            }
            b'a' | b'A' => {
                check_format(&sp, &form)?;
                let f = val_to_float(val, "format")?;
                conv_float(f, &sp)
            }
            b'f' | b'e' | b'E' | b'g' | b'G' => {
                check_format(&sp, &form)?;
                let f = val_to_float(val, "format")?;
                conv_float(f, &sp)
            }
            b'p' => {
                check_format(&sp, &form)?;
                let s = match val {
                    Value::Object(r) => format!("0x{:x}", r.ptr_value()).into_bytes(),
                    _ => b"(null)".to_vec(),
                };
                pad_bytes(&s, sp.width, sp.minus)
            }
            b'q' => {
                if sp.minus
                    || sp.plus
                    || sp.space
                    || sp.hash
                    || sp.zero
                    || sp.width > 0
                    || sp.prec.is_some()
                {
                    return Err(LuaError::new("specifier '%q' cannot have modifiers"));
                }
                addliteral(val, puc_arg, tostring)?
            }
            b's' => {
                let no_mods = !(sp.minus
                    || sp.plus
                    || sp.space
                    || sp.hash
                    || sp.zero
                    || sp.width > 0
                    || sp.prec.is_some());
                let bytes = tostring(val)?;
                if no_mods {
                    bytes
                } else {
                    if bytes.contains(&0) {
                        return Err(LuaError::new(format!(
                            "bad argument #{} to 'format' (string contains zeros)",
                            puc_arg
                        )));
                    }
                    check_format(&sp, &form)?;
                    if sp.prec.is_none() && bytes.len() >= 100 {
                        bytes
                    } else {
                        let cut = match sp.prec {
                            Some(p) if p < bytes.len() => &bytes[..p],
                            _ => &bytes[..],
                        };
                        pad_bytes(cut, sp.width, sp.minus)
                    }
                }
            }
            _ => {
                return Err(LuaError::new(format!(
                    "invalid conversion '{}' to 'format'",
                    String::from_utf8_lossy(&form)
                )));
            }
        };
        result.extend_from_slice(&text);
    }
    Ok(result)
}

fn bad_spec(form: &[u8]) -> LuaError {
    LuaError::new(format!(
        "invalid conversion specification: '{}'",
        String::from_utf8_lossy(form)
    ))
}

fn check_format(sp: &FmtSpec, form: &[u8]) -> Result<(), LuaError> {
    let flags: &[u8] = match sp.spec {
        b'd' | b'i' => b"-+0 ",
        b'u' => b"-0",
        b'o' | b'x' | b'X' => b"-#0",
        b'c' | b'p' | b's' => b"-",
        _ => b"-+#0 ",
    };
    let bad = (sp.minus && !flags.contains(&b'-'))
        || (sp.plus && !flags.contains(&b'+'))
        || (sp.space && !flags.contains(&b' '))
        || (sp.hash && !flags.contains(&b'#'))
        || (sp.zero && !flags.contains(&b'0'));
    if bad {
        return Err(bad_spec(form));
    }
    if sp.prec.is_some() && matches!(sp.spec, b'c' | b'p') {
        return Err(bad_spec(form));
    }
    Ok(())
}

fn pad_bytes(bytes: &[u8], width: usize, left: bool) -> Vec<u8> {
    let mut out = Vec::new();
    if width > bytes.len() && left {
        out.extend_from_slice(bytes);
        out.extend(std::iter::repeat(b' ').take(width - bytes.len()));
    } else if width > bytes.len() {
        out.extend(std::iter::repeat(b' ').take(width - bytes.len()));
        out.extend_from_slice(bytes);
    } else {
        out.extend_from_slice(bytes);
    }
    out
}

fn conv_integer(n: i64, sp: &FmtSpec) -> Vec<u8> {
    let (neg, mag): (bool, u128) = match sp.spec {
        b'd' | b'i' => (n < 0, (n as i128).unsigned_abs()),
        _ => (false, n as u64 as u128),
    };
    let mut digits = match sp.spec {
        b'o' => format!("{:o}", mag),
        b'x' => format!("{:x}", mag),
        b'X' => format!("{:X}", mag),
        _ => mag.to_string(),
    };
    if let Some(p) = sp.prec {
        if p == 0 && mag == 0 {
            digits.clear();
        } else if digits.len() < p {
            let mut s = "0".repeat(p - digits.len());
            s.push_str(&digits);
            digits = s;
        }
    }
    let mut prefix: Vec<u8> = Vec::new();
    if neg {
        prefix.push(b'-');
    } else if sp.plus {
        prefix.push(b'+');
    } else if sp.space {
        prefix.push(b' ');
    }
    match sp.spec {
        b'o' if sp.hash => {
            if !digits.starts_with('0') {
                digits.insert(0, '0');
            }
        }
        b'x' if sp.hash && mag != 0 => prefix.extend_from_slice(b"0x"),
        b'X' if sp.hash && mag != 0 => prefix.extend_from_slice(b"0X"),
        _ => {}
    }
    let core = prefix.len() + digits.len();
    let mut out = Vec::new();
    if sp.width > core {
        let pad = sp.width - core;
        if sp.minus {
            out.extend_from_slice(&prefix);
            out.extend_from_slice(digits.as_bytes());
            out.extend(std::iter::repeat(b' ').take(pad));
        } else if sp.zero && sp.prec.is_none() {
            out.extend_from_slice(&prefix);
            out.extend(std::iter::repeat(b'0').take(pad));
            out.extend_from_slice(digits.as_bytes());
        } else {
            out.extend(std::iter::repeat(b' ').take(pad));
            out.extend_from_slice(&prefix);
            out.extend_from_slice(digits.as_bytes());
        }
    } else {
        out.extend_from_slice(&prefix);
        out.extend_from_slice(digits.as_bytes());
    }
    out
}

fn strip_g_zeros(s: &mut String) {
    if s.contains('.') {
        while s.ends_with('0') {
            s.pop();
        }
        if s.ends_with('.') {
            s.pop();
        }
    }
}

fn float_special_sign(f: f64, sp: &FmtSpec) -> (Vec<u8>, String) {
    let upper = matches!(sp.spec, b'E' | b'G' | b'A');
    let name = if f.is_nan() {
        if upper { "NAN" } else { "nan" }
    } else if upper {
        "INF"
    } else {
        "inf"
    };
    let neg = f.is_sign_negative();
    let mut sign = Vec::new();
    if neg {
        sign.push(b'-');
    } else if sp.plus {
        sign.push(b'+');
    } else if sp.space {
        sign.push(b' ');
    }
    (sign, name.to_string())
}

fn conv_float(f: f64, sp: &FmtSpec) -> Vec<u8> {
    if f.is_nan() || f.is_infinite() {
        let (sign, body) = float_special_sign(f, sp);
        return assemble_float(sign, body, sp);
    }
    let a = f.abs();
    let mut sign = Vec::new();
    if f.is_sign_negative() {
        sign.push(b'-');
    } else if sp.plus {
        sign.push(b'+');
    } else if sp.space {
        sign.push(b' ');
    }
    let body = match sp.spec {
        b'f' => {
            let prec = sp.prec.unwrap_or(6);
            let mut s = format!("{a:.prec$}");
            if sp.hash && prec == 0 {
                s.push('.');
            }
            s
        }
        b'e' | b'E' => float_e_body(a, sp.prec.unwrap_or(6), sp.hash, sp.spec == b'E'),
        b'g' | b'G' => float_g_body(a, sp.prec.unwrap_or(6), sp.hash, sp.spec == b'G'),
        b'a' | b'A' => float_a_body(a, sp.prec, sp.hash, sp.spec == b'A'),
        _ => unreachable!(),
    };
    assemble_float(sign, body, sp)
}

fn assemble_float(sign: Vec<u8>, body: String, sp: &FmtSpec) -> Vec<u8> {
    let core = sign.len() + body.len();
    let mut out = Vec::new();
    if sp.width > core {
        let pad = sp.width - core;
        if sp.minus {
            out.extend_from_slice(&sign);
            out.extend_from_slice(body.as_bytes());
            out.extend(std::iter::repeat(b' ').take(pad));
        } else if sp.zero {
            out.extend_from_slice(&sign);
            out.extend(std::iter::repeat(b'0').take(pad));
            out.extend_from_slice(body.as_bytes());
        } else {
            out.extend(std::iter::repeat(b' ').take(pad));
            out.extend_from_slice(&sign);
            out.extend_from_slice(body.as_bytes());
        }
    } else {
        out.extend_from_slice(&sign);
        out.extend_from_slice(body.as_bytes());
    }
    out
}

fn float_e_body(a: f64, prec: usize, hash: bool, upper: bool) -> String {
    let s = format!("{a:.prec$e}");
    let (m, e) = s.split_once('e').unwrap();
    let exp: i32 = e.parse().unwrap();
    let mut m = m.to_string();
    if hash && prec == 0 {
        m.push('.');
    }
    format!(
        "{m}{}{}{:02}",
        if upper { 'E' } else { 'e' },
        if exp < 0 { '-' } else { '+' },
        exp.abs()
    )
}

fn float_g_body(a: f64, prec: usize, hash: bool, upper: bool) -> String {
    let p = if prec == 0 { 1 } else { prec };
    let es = format!("{a:.p$e}", p = p - 1);
    let (m0, e) = es.split_once('e').unwrap();
    let x: i32 = e.parse().unwrap();
    if x < -4 || x >= p as i32 {
        let mut m = m0.to_string();
        if !hash {
            strip_g_zeros(&mut m);
        }
        format!(
            "{m}{}{}{:02}",
            if upper { 'E' } else { 'e' },
            if x < 0 { '-' } else { '+' },
            x.abs()
        )
    } else {
        let dec = (p as i32 - 1 - x).max(0) as usize;
        let mut s = format!("{a:.dec$}");
        if !hash {
            strip_g_zeros(&mut s);
        }
        s
    }
}

fn float_a_body(a: f64, prec: Option<usize>, hash: bool, upper: bool) -> String {
    let xp = if upper { 'P' } else { 'p' };
    let pfx = if upper { "0X" } else { "0x" };
    if a == 0.0 {
        let p = prec.unwrap_or(0);
        let mut frac = "0".repeat(p);
        if p == 0 && hash {
            frac.push('.');
        }
        return format!("{pfx}0{frac}{xp}+0");
    }
    let bits = a.to_bits();
    let mut exp = ((bits >> 52) & 0x7FF) as i32;
    let mut mant = bits & 0xF_FFFF_FFFF_FFFF;
    if exp == 0 {
        exp = -1022;
        while mant & (1 << 52) == 0 {
            mant <<= 1;
            exp -= 1;
        }
    } else {
        exp -= 1023;
        mant |= 1 << 52;
    }
    let frac_bits = mant & 0xF_FFFF_FFFF_FFFF;
    let p = prec.unwrap_or(13);
    let (mut lead, mut exp, mut digits) = if p >= 13 {
        let mut s = if upper {
            format!("{frac_bits:013X}")
        } else {
            format!("{frac_bits:013x}")
        };
        for _ in 13..p {
            s.push('0');
        }
        (1u64, exp, s)
    } else {
        let shift = 4 * (13 - p) as u32;
        let kept = if shift >= 64 { 0 } else { frac_bits >> shift };
        let dropped = if shift == 0 || shift >= 64 {
            0
        } else {
            frac_bits & ((1u64 << shift) - 1)
        };
        let half = if shift == 0 { 0 } else { 1u64 << (shift - 1) };
        let mut d = kept;
        let mut l = 1u64;
        let mut e = exp;
        if dropped > half || (dropped == half && (kept & 1) == 1) {
            d += 1;
            if d == (1u64 << (4 * p as u32)) {
                d = 0;
                l += 1;
                if l == 2 {
                    l = 1;
                    e += 1;
                }
            }
        }
        let digits = if upper {
            format!("{d:0width$X}", width = p)
        } else {
            format!("{d:0width$x}", width = p)
        };
        (l, e, digits)
    };
    if prec.is_none() {
        while digits.ends_with('0') {
            digits.pop();
        }
    }
    let dot = if digits.is_empty() {
        if prec.is_some() || hash { "." } else { "" }
    } else {
        "."
    };
    let _ = &mut lead;
    format!(
        "{pfx}{lead}{dot}{digits}{xp}{}{}",
        if exp < 0 { '-' } else { '+' },
        exp.abs()
    )
}

fn addliteral(
    val: Value,
    _argn: usize,
    tostring: &mut dyn FnMut(Value) -> Result<Vec<u8>, LuaError>,
) -> Result<Vec<u8>, LuaError> {
    match val {
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let mut out = Vec::new();
            addquoted(&mut out, s.as_bytes());
            Ok(out)
        }
        Value::Integer(n) => {
            if n == i64::MIN {
                Ok(b"0x8000000000000000".to_vec())
            } else {
                Ok(n.to_string().into_bytes())
            }
        }
        Value::Float(f) => {
            if f == f64::INFINITY {
                Ok(b"1e9999".to_vec())
            } else if f == f64::NEG_INFINITY {
                Ok(b"-1e9999".to_vec())
            } else if f.is_nan() {
                Ok(b"(0/0)".to_vec())
            } else {
                let neg = f.is_sign_negative();
                let mut s = float_a_body(f.abs(), None, false, false);
                if neg {
                    s.insert(0, '-');
                }
                Ok(s.into_bytes())
            }
        }
        Value::Nil => tostring(val),
        Value::Boolean(_) => tostring(val),
        _ => Err(LuaError::new(format!(
            "bad argument #{} to 'format' (value has no literal form)",
            _argn
        ))),
    }
}

fn addquoted(out: &mut Vec<u8>, s: &[u8]) {
    out.push(b'"');
    for (idx, &b) in s.iter().enumerate() {
        if b == b'"' || b == b'\\' || b == b'\n' {
            out.push(b'\\');
            out.push(b);
        } else if b < 0x20 || b == 0x7F {
            if idx + 1 < s.len() && s[idx + 1].is_ascii_digit() {
                out.extend_from_slice(format!("\\{:03}", b).as_bytes());
            } else {
                out.extend_from_slice(format!("\\{}", b).as_bytes());
            }
        } else {
            out.push(b);
        }
    }
    out.push(b'"');
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

// ── Pattern matching engine ────────────────────────────────────────

/// Lua pattern match state.
pub(crate) struct MatchState<'a> {
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
    pub(crate) fn new(source: &'a [u8], pattern: &'a [u8]) -> Self {
        MatchState {
            source,
            pattern,
            captures: Vec::new(),
            level: 0,
        }
    }

    /// Match pattern starting at pat_idx against source starting at si.
    /// Returns the end position in source if match succeeds.
    pub(crate) fn match_pattern(&mut self, si: usize, pi: usize) -> Option<usize> {
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

    // `init` is 1-based; negative counts from the end. An init beyond
    // `len + 1` finds nothing.
    if init > s.len() as i64 + 1 {
        return Ok(vec![Value::Nil]);
    }
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

pub(crate) fn apply_string_replacement(
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

pub(crate) fn get_captures(ms: &MatchState, source: &[u8], start: usize, end: usize, gc: &mut Gc) -> Vec<Value> {
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

pub fn string_dump(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    let strip = args.get(1).map(|v| v.is_truthy()).unwrap_or(false);

    let proto = match v {
        Value::Object(r) => match r.as_object().as_closure() {
            Some(crate::closure::Closure::Lua(lc)) => Some(std::rc::Rc::clone(&lc.proto)),
            _ => None,
        },
        _ => None,
    };

    match proto {
        Some(proto) => {
            let bytes = crate::chunk::dump(&proto, strip);
            Ok(vec![Value::Object(gc.new_string(&bytes))])
        }
        None => Err(LuaError::new(
            "bad argument #1 to 'dump' (Lua function expected)",
        )),
    }
}

pub fn string_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("byte", string_byte as NativeFn),
        ("char", string_char),
        ("dump", string_dump),
        ("find", string_find),
        ("gmatch", string_gmatch),
        ("len", string_len),
        ("lower", string_lower),
        ("match", string_match),
        ("pack", crate::stdlib::pack::string_pack as NativeFn),
        ("packsize", crate::stdlib::pack::string_packsize),
        ("rep", string_rep),
        ("reverse", string_reverse),
        ("sub", string_sub),
        ("unpack", crate::stdlib::pack::string_unpack),
        ("upper", string_upper),
    ]
}
