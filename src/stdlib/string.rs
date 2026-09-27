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
    pub(crate) captures: Vec<Capture>,
    level: usize,
    error: Option<String>,
}

#[derive(Clone, Copy)]
pub(crate) struct Capture {
    pub(crate) start: usize,
    pub(crate) len: CaptureLen,
}

#[derive(Clone, Copy)]
pub(crate) enum CaptureLen {
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
            error: None,
        }
    }

    /// Match pattern starting at pat_idx against source starting at si.
    /// Returns the end position in source if match succeeds.  Malformed
    /// patterns produce an `Err`.
    pub(crate) fn match_pattern(
        &mut self,
        si: usize,
        pi: usize,
    ) -> Result<Option<usize>, LuaError> {
        let res = self.match_impl(si, pi, 0);
        match self.error.take() {
            Some(msg) => Err(LuaError::new(msg)),
            None => Ok(res),
        }
    }

    /// Return the index just past the pattern item starting at `pi`,
    /// mirroring `classend`.
    fn class_end(&mut self, pi: usize) -> Option<usize> {
        match self.pattern.get(pi).copied() {
            Some(b'%') => {
                if pi + 1 >= self.pattern.len() {
                    self.error = Some("malformed pattern (ends with '%')".into());
                    return None;
                }
                Some(pi + 2)
            }
            Some(b'[') => {
                let mut p = pi + 1;
                if self.pattern.get(p).copied() == Some(b'^') {
                    p += 1;
                }
                loop {
                    let c = match self.pattern.get(p).copied() {
                        Some(c) => c,
                        None => {
                            self.error =
                                Some("malformed pattern (missing ']')".into());
                            return None;
                        }
                    };
                    p += 1;
                    if c == b'%' && p < self.pattern.len() {
                        p += 1;
                    }
                    if self.pattern.get(p).copied() == Some(b']') {
                        return Some(p + 1);
                    }
                }
            }
            Some(_) => Some(pi + 1),
            None => Some(pi),
        }
    }

    /// Match a byte against a bracket class `[...]` (from `p` to `ec`).
    fn match_bracket_class(&self, c: u8, p: usize, ec: usize) -> bool {
        let mut sig = true;
        let mut p = p;
        if self.pattern.get(p + 1).copied() == Some(b'^') {
            sig = false;
            p += 1;
        }
        loop {
            p += 1;
            if p >= ec {
                break;
            }
            if self.pattern[p] == b'%' {
                p += 1;
                if p < self.pattern.len() && match_char_class(c, self.pattern[p]) {
                    return sig;
                }
            } else if p + 2 < ec && self.pattern[p + 1] == b'-' {
                if self.pattern[p] <= c && c <= self.pattern[p + 2] {
                    return sig;
                }
                p += 2;
            } else if self.pattern[p] == c {
                return sig;
            }
        }
        !sig
    }

    /// Match a single pattern item against source at `si`.
    fn single_match(&self, si: usize, pi: usize, ep: usize) -> bool {
        if si >= self.source.len() {
            return false;
        }
        let c = self.source[si];
        match self.pattern.get(pi).copied().unwrap_or(0) {
            b'.' => true,
            b'%' => match_char_class(c, self.pattern[pi + 1]),
            b'[' => self.match_bracket_class(c, pi, ep - 1),
            other => other == c,
        }
    }

    fn match_balance(&mut self, si: usize, pi: usize) -> Option<usize> {
        if pi + 1 >= self.pattern.len() {
            self.error =
                Some("malformed pattern (missing arguments to '%b')".into());
            return None;
        }
        if si >= self.source.len() || self.source[si] != self.pattern[pi] {
            return None;
        }
        let b = self.pattern[pi];
        let e = self.pattern[pi + 1];
        let mut cont = 1i32;
        let mut s = si;
        loop {
            s += 1;
            if s >= self.source.len() {
                break;
            }
            if self.source[s] == e {
                cont -= 1;
                if cont == 0 {
                    return Some(s + 1);
                }
            } else if self.source[s] == b {
                cont += 1;
            }
        }
        None
    }

    fn max_expand(
        &mut self,
        si: usize,
        pi: usize,
        ep: usize,
        depth: usize,
    ) -> Option<usize> {
        let mut i = 0usize;
        while self.single_match(si + i, pi, ep) {
            i += 1;
        }
        loop {
            if let Some(res) = self.match_impl(si + i, ep + 1, depth + 1) {
                return Some(res);
            }
            if i == 0 {
                return None;
            }
            i -= 1;
        }
    }

    fn min_expand(
        &mut self,
        mut si: usize,
        pi: usize,
        ep: usize,
        depth: usize,
    ) -> Option<usize> {
        loop {
            if let Some(res) = self.match_impl(si, ep + 1, depth + 1) {
                return Some(res);
            }
            if self.single_match(si, pi, ep) {
                si += 1;
            } else {
                return None;
            }
        }
    }

    fn start_capture(
        &mut self,
        si: usize,
        pi: usize,
        position: bool,
        depth: usize,
    ) -> Option<usize> {
        if self.level >= 32 {
            self.error = Some("too many captures".into());
            return None;
        }
        self.captures.push(Capture {
            start: si,
            len: if position {
                CaptureLen::Position
            } else {
                CaptureLen::Unfinished
            },
        });
        self.level += 1;
        let res = self.match_impl(si, pi, depth + 1);
        if res.is_none() {
            self.level -= 1;
            self.captures.pop();
        }
        res
    }

    fn capture_to_close(&mut self) -> Option<usize> {
        for i in (0..self.captures.len()).rev() {
            if matches!(self.captures[i].len, CaptureLen::Unfinished) {
                return Some(i);
            }
        }
        self.error = Some("invalid pattern capture".into());
        None
    }

    fn end_capture(&mut self, si: usize, pi: usize, depth: usize) -> Option<usize> {
        let l = self.capture_to_close()?;
        let old = self.captures[l].len;
        self.captures[l].len = CaptureLen::Len(si - self.captures[l].start);
        let res = self.match_impl(si, pi, depth + 1);
        if res.is_none() {
            self.captures[l].len = old;
        }
        res
    }

    fn match_capture(&mut self, si: usize, lch: u8) -> Option<usize> {
        let l = lch as i32 - b'1' as i32;
        if l < 0
            || l >= self.level as i32
            || matches!(self.captures[l as usize].len, CaptureLen::Unfinished)
        {
            self.error = Some(format!("invalid capture index %{}", l + 1));
            return None;
        }
        let cap = self.captures[l as usize];
        let len = match cap.len {
            CaptureLen::Len(n) => n,
            // Position captures cannot be used as back-references.
            _ => return None,
        };
        if self.source.len() - si >= len
            && self.source[cap.start..cap.start + len] == self.source[si..si + len]
        {
            Some(si + len)
        } else {
            None
        }
    }

    fn match_impl(&mut self, mut si: usize, mut pi: usize, depth: usize) -> Option<usize> {
        if depth > 200 {
            self.error = Some("pattern too complex".into());
            return None;
        }
        'init: loop {
            if pi >= self.pattern.len() {
                return Some(si);
            }
            match self.pattern[pi] {
                b'(' => {
                    if self.pattern.get(pi + 1).copied() == Some(b')') {
                        return self.start_capture(si, pi + 2, true, depth);
                    }
                    return self.start_capture(si, pi + 1, false, depth);
                }
                b')' => {
                    return self.end_capture(si, pi + 1, depth);
                }
                b'$' if pi + 1 == self.pattern.len() => {
                    return if si == self.source.len() { Some(si) } else { None };
                }
                b'%' => {
                    match self.pattern.get(pi + 1).copied() {
                        Some(b'b') => {
                            match self.match_balance(si, pi + 2) {
                                Some(s) => {
                                    si = s;
                                    pi += 4;
                                    continue 'init;
                                }
                                None => return None,
                            }
                        }
                        Some(b'f') => {
                            pi += 2;
                            if self.pattern.get(pi).copied() != Some(b'[') {
                                self.error =
                                    Some("missing '[' after '%f' in pattern".into());
                                return None;
                            }
                            let ep = self.class_end(pi)?;
                            let previous = if si == 0 { 0 } else { self.source[si - 1] };
                            let cur = self.source.get(si).copied().unwrap_or(0);
                            if !self.match_bracket_class(previous, pi, ep - 1)
                                && self.match_bracket_class(cur, pi, ep - 1)
                            {
                                pi = ep;
                                continue 'init;
                            }
                            return None;
                        }
                        Some(c @ b'0'..=b'9') => {
                            match self.match_capture(si, c) {
                                Some(s) => {
                                    si = s;
                                    pi += 2;
                                    continue 'init;
                                }
                                None => return None,
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
            // Default: pattern class plus optional suffix.
            let ep = self.class_end(pi)?;
            if !self.single_match(si, pi, ep) {
                match self.pattern.get(ep).copied() {
                    Some(b'*') | Some(b'?') | Some(b'-') => {
                        pi = ep + 1;
                        continue 'init;
                    }
                    _ => return None,
                }
            } else {
                match self.pattern.get(ep).copied() {
                    Some(b'?') => {
                        if let Some(res) = self.match_impl(si + 1, ep + 1, depth + 1) {
                            return Some(res);
                        }
                        pi = ep + 1;
                        continue 'init;
                    }
                    Some(b'+') => {
                        return self.max_expand(si + 1, pi, ep, depth);
                    }
                    Some(b'*') => {
                        return self.max_expand(si, pi, ep, depth);
                    }
                    Some(b'-') => {
                        return self.min_expand(si, pi, ep, depth);
                    }
                    _ => {
                        si += 1;
                        pi = ep;
                        continue 'init;
                    }
                }
            }
        }
    }
}

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
        b'z' => ch == 0,
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
            if let Some(end) = ms.match_pattern(search_start, 0)? {
                let mut results = vec![
                    Value::Integer(search_start as i64 + 1),
                    Value::Integer(end as i64),
                ];
                for cap in &ms.captures {
                    results.push(capture_to_value(cap, &s, gc)?);
                }
                return Ok(results);
            }
            return Ok(vec![Value::Nil]);
        }

        for si in search_start..=s.len() {
            let mut ms = MatchState::new(&s, pat_slice);
            if let Some(end) = ms.match_pattern(si, 0)? {
                let mut results = vec![
                    Value::Integer(si as i64 + 1),
                    Value::Integer(end as i64),
                ];
                for cap in &ms.captures {
                    results.push(capture_to_value(cap, &s, gc)?);
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
        if let Some(end) = ms.match_pattern(start, 0)? {
            return get_captures(&ms, &s, start, end, gc);
        }
        return Ok(vec![Value::Nil]);
    }

    for si in start..=s.len() {
        let mut ms = MatchState::new(&s, pat_slice);
        if let Some(end) = ms.match_pattern(si, 0)? {
            return get_captures(&ms, &s, si, end, gc);
        }
    }
    Ok(vec![Value::Nil])
}

pub fn string_gmatch(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let s_ref = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => *r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'gmatch' (string expected)",
            ))
        }
    };
    let pat = check_string(args, 1, "gmatch")?;
    if args.len() > 2 && !matches!(args[2], Value::Integer(_) | Value::Float(_)) {
        return Err(LuaError::new(
            "bad argument #3 to 'gmatch' (number expected)",
        ));
    }
    let init_arg = args
        .get(2)
        .and_then(|v| match v {
            Value::Integer(n) => Some(*n),
            Value::Float(f) => Some(*f as i64),
            _ => None,
        })
        .unwrap_or(1);
    let len = s_ref
        .as_object()
        .as_string()
        .unwrap()
        .as_bytes()
        .len() as i64;
    let mut init = if init_arg > 0 {
        init_arg - 1
    } else if init_arg < -len {
        0
    } else {
        len + init_arg
    };
    if init > len {
        init = len + 1;
    }
    let src = std::cell::Cell::new(init as usize);
    let lastmatch: std::cell::Cell<Option<usize>> = std::cell::Cell::new(None);

    let iter_fn = move |_args: &[Value], gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
        let s = s_ref
            .as_object()
            .as_string()
            .ok_or_else(|| LuaError::new("string expected"))?
            .as_bytes()
            .to_vec();
        let mut cur = src.get();
        while cur <= s.len() {
            let mut ms = MatchState::new(&s, &pat);
            if let Some(e) = ms.match_pattern(cur, 0)? {
                if Some(e) != lastmatch.get() {
                    lastmatch.set(Some(e));
                    src.set(e);
                    return get_captures(&ms, &s, cur, e, gc);
                }
            }
            cur += 1;
        }
        Ok(vec![Value::Nil])
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
) -> Result<(), LuaError> {
    let whole = &source[match_start..match_end];
    let mut i = 0;
    while i < repl.len() {
        if repl[i] != b'%' {
            result.push(repl[i]);
            i += 1;
            continue;
        }
        if i + 1 >= repl.len() {
            return Err(LuaError::new(
                "invalid use of '%' in replacement string",
            ));
        }
        let c = repl[i + 1];
        if c == b'%' {
            result.push(b'%');
        } else if c == b'0' {
            result.extend_from_slice(whole);
        } else if c.is_ascii_digit() {
            let idx = (c - b'0') as usize;
            if idx - 1 < ms.captures.len() {
                let cap = &ms.captures[idx - 1];
                match cap.len {
                    CaptureLen::Len(len) => {
                        result.extend_from_slice(&source[cap.start..cap.start + len]);
                    }
                    CaptureLen::Position => {
                        let s = format!("{}", cap.start + 1);
                        result.extend_from_slice(s.as_bytes());
                    }
                    CaptureLen::Unfinished => {
                        return Err(LuaError::new("unfinished capture"));
                    }
                }
            } else if idx == 1 {
                // No captures in the pattern: `%1` is the whole match.
                result.extend_from_slice(whole);
            } else {
                return Err(LuaError::new(format!(
                    "invalid capture index %{} in replacement string",
                    idx
                )));
            }
        } else {
            return Err(LuaError::new(
                "invalid use of '%' in replacement string",
            ));
        }
        i += 2;
    }
    Ok(())
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

fn capture_to_value(cap: &Capture, source: &[u8], gc: &mut Gc) -> Result<Value, LuaError> {
    match cap.len {
        CaptureLen::Len(len) => {
            let s = gc.new_string(&source[cap.start..cap.start + len]);
            Ok(Value::Object(s))
        }
        CaptureLen::Position => Ok(Value::Integer(cap.start as i64 + 1)),
        CaptureLen::Unfinished => Err(LuaError::new("unfinished capture")),
    }
}

pub(crate) fn get_captures(
    ms: &MatchState,
    source: &[u8],
    start: usize,
    end: usize,
    gc: &mut Gc,
) -> Result<Vec<Value>, LuaError> {
    if ms.captures.is_empty() {
        // No explicit captures: return whole match
        let s = gc.new_string(&source[start..end]);
        Ok(vec![Value::Object(s)])
    } else {
        let mut out = Vec::with_capacity(ms.captures.len());
        for cap in &ms.captures {
            out.push(capture_to_value(cap, source, gc)?);
        }
        Ok(out)
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
