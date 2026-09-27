//! `string.pack`, `string.unpack`, and `string.packsize`.
//!
//! Implements the Lua 5.5 format strings described in §6.5.2 of the
//! reference manual. The implementation mirrors PUC-Rio's `lstrlib.c`
//! semantics (including error messages) so the upstream `tpack.lua`
//! suite can be used as a reference.

use std::os::raw::{c_int, c_long, c_short};

use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

/// Maximum size for the binary representation of an integer.
const MAXINTSIZE: u64 = 16;
/// Size of a Lua integer (`lua_Integer`).
const SZINT: u64 = 8;

/// Maximum size for a packed result, representable as a Lua integer.
fn max_size() -> u64 {
    if std::mem::size_of::<usize>() < std::mem::size_of::<i64>() {
        usize::MAX as u64
    } else {
        i64::MAX as u64
    }
}

fn native_little() -> bool {
    cfg!(target_endian = "little")
}

/// Native maximum alignment: `offsetof(struct cD, u)` where
/// `struct cD { char c; union { LUAI_MAXALIGN; } u; }`.
fn native_max_align() -> u64 {
    let mut a = std::mem::align_of::<f64>() as u64;
    a = a.max(std::mem::align_of::<*const ()>() as u64);
    a = a.max(std::mem::align_of::<i64>() as u64);
    a = a.max(std::mem::align_of::<c_long>() as u64);
    a
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum KOption {
    Int,
    Uint,
    Float,
    Number,
    Double,
    Char,
    Str,
    Zstr,
    Padding,
    PaddAlign,
    Nop,
}

struct Header {
    islittle: bool,
    maxalign: u64,
}

impl Header {
    fn new() -> Self {
        Header {
            islittle: native_little(),
            maxalign: 1,
        }
    }
}

fn arg_error(fname: &str, n: usize, msg: &str) -> LuaError {
    LuaError::new(format!("bad argument #{n} to '{fname}' ({msg})"))
}

// ── Argument checking ──────────────────────────────────────────────

fn check_str(args: &[Value], idx: usize, fname: &str) -> Result<Vec<u8>, LuaError> {
    match args.get(idx).copied().unwrap_or(Value::Nil) {
        Value::Object(r) if r.as_object().as_string().is_some() => {
            Ok(r.as_object().as_string().unwrap().as_bytes().to_vec())
        }
        Value::Integer(i) => Ok(format!("{i}").into_bytes()),
        Value::Float(f) => Ok(format!("{f}").into_bytes()),
        v => Err(arg_error(
            fname,
            idx + 1,
            &format!("string expected, got {}", v.type_name()),
        )),
    }
}

fn to_integer_arg(v: Value, idx: usize, fname: &str) -> Result<i64, LuaError> {
    match v {
        Value::Integer(i) => Ok(i),
        Value::Float(f) => {
            if f.floor() == f && f >= -(2f64.powi(63)) && f < 2f64.powi(63) {
                Ok(f as i64)
            } else {
                Err(arg_error(
                    fname,
                    idx + 1,
                    "number has no integer representation",
                ))
            }
        }
        v => Err(arg_error(
            fname,
            idx + 1,
            &format!("number expected, got {}", v.type_name()),
        )),
    }
}

fn check_int(args: &[Value], idx: usize, fname: &str) -> Result<i64, LuaError> {
    to_integer_arg(args.get(idx).copied().unwrap_or(Value::Nil), idx, fname)
}

fn opt_int(args: &[Value], idx: usize, default: i64, fname: &str) -> Result<i64, LuaError> {
    match args.get(idx).copied() {
        None | Some(Value::Nil) => Ok(default),
        Some(v) => to_integer_arg(v, idx, fname),
    }
}

fn check_num(args: &[Value], idx: usize, fname: &str) -> Result<f64, LuaError> {
    match args.get(idx).copied().unwrap_or(Value::Nil) {
        Value::Integer(i) => Ok(i as f64),
        Value::Float(f) => Ok(f),
        v => Err(arg_error(
            fname,
            idx + 1,
            &format!("number expected, got {}", v.type_name()),
        )),
    }
}

// ── Format string parsing ──────────────────────────────────────────

/// Read an integer numeral from the format, or return `df`.
/// Stops consuming digits once the value could overflow `max_size()`,
/// leaving trailing digits to be parsed as (invalid) options.
fn getnum(fmt: &[u8], pos: &mut usize, df: u64) -> u64 {
    if *pos >= fmt.len() || !fmt[*pos].is_ascii_digit() {
        return df;
    }
    let mut a: u64 = 0;
    loop {
        a = a.wrapping_mul(10).wrapping_add((fmt[*pos] - b'0') as u64);
        *pos += 1;
        if !(*pos < fmt.len() && fmt[*pos].is_ascii_digit() && a <= (max_size() - 9) / 10) {
            break;
        }
    }
    a
}

fn getnumlimit(fmt: &[u8], pos: &mut usize, df: u64) -> Result<u64, LuaError> {
    let sz = getnum(fmt, pos, df);
    if sz == 0 || sz > MAXINTSIZE {
        return Err(LuaError::new(format!(
            "integral size ({sz}) out of limits [1,{MAXINTSIZE}]"
        )));
    }
    Ok(sz)
}

/// Read and classify the next option. Returns `(kind, size)`.
fn getoption(fmt: &[u8], pos: &mut usize, h: &mut Header) -> Result<(KOption, u64), LuaError> {
    let opt = fmt[*pos];
    *pos += 1;
    let size;
    let kind = match opt {
        b'b' => {
            size = std::mem::size_of::<u8>() as u64;
            KOption::Int
        }
        b'B' => {
            size = std::mem::size_of::<u8>() as u64;
            KOption::Uint
        }
        b'h' => {
            size = std::mem::size_of::<c_short>() as u64;
            KOption::Int
        }
        b'H' => {
            size = std::mem::size_of::<c_short>() as u64;
            KOption::Uint
        }
        b'l' => {
            size = std::mem::size_of::<c_long>() as u64;
            KOption::Int
        }
        b'L' => {
            size = std::mem::size_of::<c_long>() as u64;
            KOption::Uint
        }
        b'j' => {
            size = std::mem::size_of::<i64>() as u64;
            KOption::Int
        }
        b'J' => {
            size = std::mem::size_of::<i64>() as u64;
            KOption::Uint
        }
        b'T' => {
            size = std::mem::size_of::<usize>() as u64;
            KOption::Uint
        }
        b'f' => {
            size = std::mem::size_of::<f32>() as u64;
            KOption::Float
        }
        b'n' => {
            size = std::mem::size_of::<f64>() as u64;
            KOption::Number
        }
        b'd' => {
            size = std::mem::size_of::<f64>() as u64;
            KOption::Double
        }
        b'i' => {
            size = getnumlimit(fmt, pos, std::mem::size_of::<c_int>() as u64)?;
            KOption::Int
        }
        b'I' => {
            size = getnumlimit(fmt, pos, std::mem::size_of::<c_int>() as u64)?;
            KOption::Uint
        }
        b's' => {
            size = getnumlimit(fmt, pos, std::mem::size_of::<usize>() as u64)?;
            KOption::Str
        }
        b'c' => {
            size = getnum(fmt, pos, u64::MAX);
            if size == u64::MAX {
                return Err(LuaError::new("missing size for format option 'c'"));
            }
            KOption::Char
        }
        b'z' => {
            size = 0;
            KOption::Zstr
        }
        b'x' => {
            size = 1;
            KOption::Padding
        }
        b'X' => {
            size = 0;
            KOption::PaddAlign
        }
        b' ' => {
            size = 0;
            KOption::Nop
        }
        b'<' => {
            h.islittle = true;
            size = 0;
            KOption::Nop
        }
        b'>' => {
            h.islittle = false;
            size = 0;
            KOption::Nop
        }
        b'=' => {
            h.islittle = native_little();
            size = 0;
            KOption::Nop
        }
        b'!' => {
            h.maxalign = getnumlimit(fmt, pos, native_max_align())?;
            size = 0;
            KOption::Nop
        }
        _ => {
            return Err(LuaError::new(format!(
                "invalid format option '{}'",
                opt as char
            )));
        }
    };
    Ok((kind, size))
}

/// Read the next option plus its alignment requirements.
/// `totalsize` is the current output/input offset.
fn getdetails(
    fname: &str,
    h: &mut Header,
    totalsize: u64,
    fmt: &[u8],
    pos: &mut usize,
    ntoalign: &mut u64,
) -> Result<(KOption, u64), LuaError> {
    let (opt, psize) = getoption(fmt, pos, h)?;
    let mut align = psize;
    if opt == KOption::PaddAlign {
        if *pos >= fmt.len() || fmt[*pos] == 0 {
            return Err(arg_error(fname, 1, "invalid next option for option 'X'"));
        }
        let (next, asize) = getoption(fmt, pos, h)?;
        align = asize;
        if next == KOption::Char || align == 0 {
            return Err(arg_error(fname, 1, "invalid next option for option 'X'"));
        }
    }
    if align <= 1 || opt == KOption::Char {
        *ntoalign = 0;
    } else {
        if align > h.maxalign {
            align = h.maxalign;
        }
        if align & (align - 1) != 0 {
            return Err(arg_error(
                fname,
                1,
                "format asks for alignment not power of 2",
            ));
        }
        let szmoda = totalsize & (align - 1);
        *ntoalign = (align - szmoda) & (align - 1);
    }
    Ok((opt, psize))
}

// ── Integer encoding/decoding ──────────────────────────────────────

/// Append `n` as `size` bytes with the given endianness. When the
/// value is negative and `size` exceeds a Lua integer, the extra bytes
/// are sign-extended.
fn packint(out: &mut Vec<u8>, mut n: u64, islittle: bool, size: u64, neg: bool) {
    let size = size as usize;
    let start = out.len();
    out.resize(start + size, 0);
    if islittle {
        out[start] = (n & 0xFF) as u8;
        for i in 1..size {
            n >>= 8;
            out[start + i] = (n & 0xFF) as u8;
        }
        if neg && size > SZINT as usize {
            for i in SZINT as usize..size {
                out[start + i] = 0xFF;
            }
        }
    } else {
        out[start + size - 1] = (n & 0xFF) as u8;
        for i in 1..size {
            n >>= 8;
            out[start + size - 1 - i] = (n & 0xFF) as u8;
        }
        if neg && size > SZINT as usize {
            for i in SZINT as usize..size {
                out[start + size - 1 - i] = 0xFF;
            }
        }
    }
}

/// Decode an integer of `size` bytes. Sign-extends for signed sizes
/// smaller than a Lua integer; rejects unread bytes that would change
/// the value for sizes larger than a Lua integer.
fn unpackint(
    data: &[u8],
    pos: u64,
    islittle: bool,
    size: u64,
    issigned: bool,
) -> Result<i64, LuaError> {
    let size_us = size as usize;
    let limit = size_us.min(SZINT as usize);
    let mut res: u64 = 0;
    for i in (0..limit).rev() {
        res <<= 8;
        let idx = pos as usize + if islittle { i } else { size_us - 1 - i };
        res |= data[idx] as u64;
    }
    if size_us < SZINT as usize {
        if issigned {
            let mask = 1u64 << (size_us * 8 - 1);
            res = (res ^ mask).wrapping_sub(mask);
        }
    } else if size_us > SZINT as usize {
        let mask: u8 = if !issigned || (res as i64) >= 0 { 0 } else { 0xFF };
        for i in limit..size_us {
            let idx = pos as usize + if islittle { i } else { size_us - 1 - i };
            if data[idx] != mask {
                return Err(LuaError::new(format!(
                    "{size}-byte integer does not fit into Lua Integer"
                )));
            }
        }
    }
    Ok(res as i64)
}

fn posrelat_i(pos: i64, len: u64) -> u64 {
    if pos > 0 {
        pos as u64
    } else if pos == 0 {
        1
    } else if pos < -(len as i64) {
        1
    } else {
        len.wrapping_add(pos as u64).wrapping_add(1)
    }
}

// ── Public API ─────────────────────────────────────────────────────

pub fn string_pack(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fmt = check_str(args, 0, "pack")?;
    let mut out: Vec<u8> = Vec::new();
    let mut h = Header::new();
    let mut totalsize: u64 = 0;
    let mut pos = 0usize;
    let mut arg = 1usize;

    while pos < fmt.len() && fmt[pos] != 0 {
        let mut ntoalign = 0u64;
        let (opt, size) = getdetails("pack", &mut h, totalsize, &fmt, &mut pos, &mut ntoalign)?;
        if size + ntoalign > max_size() - totalsize {
            return Err(arg_error("pack", arg, "result too long"));
        }
        totalsize += ntoalign + size;
        for _ in 0..ntoalign {
            out.push(0);
        }
        arg += 1;
        match opt {
            KOption::Int => {
                let n = check_int(args, arg - 1, "pack")?;
                if size < SZINT {
                    let lim = 1i64 << (size * 8 - 1);
                    if n < -lim || n >= lim {
                        return Err(arg_error("pack", arg, "integer overflow"));
                    }
                }
                packint(&mut out, n as u64, h.islittle, size, n < 0);
            }
            KOption::Uint => {
                let n = check_int(args, arg - 1, "pack")?;
                if size < SZINT && (n as u64) >= (1u64 << (size * 8)) {
                    return Err(arg_error("pack", arg, "unsigned overflow"));
                }
                packint(&mut out, n as u64, h.islittle, size, false);
            }
            KOption::Float => {
                let f = check_num(args, arg - 1, "pack")? as f32;
                if h.islittle {
                    out.extend_from_slice(&f.to_le_bytes());
                } else {
                    out.extend_from_slice(&f.to_be_bytes());
                }
            }
            KOption::Number | KOption::Double => {
                let f = check_num(args, arg - 1, "pack")?;
                if h.islittle {
                    out.extend_from_slice(&f.to_le_bytes());
                } else {
                    out.extend_from_slice(&f.to_be_bytes());
                }
            }
            KOption::Char => {
                let s = check_str(args, arg - 1, "pack")?;
                if s.len() as u64 > size {
                    return Err(arg_error("pack", arg, "string longer than given size"));
                }
                out.extend_from_slice(&s);
                out.resize(out.len() + (size as usize - s.len()), 0);
            }
            KOption::Str => {
                let s = check_str(args, arg - 1, "pack")?;
                if size < SZINT && (s.len() as u64) >= (1u64 << (size * 8)) {
                    return Err(arg_error(
                        "pack",
                        arg,
                        "string length does not fit in given size",
                    ));
                }
                packint(&mut out, s.len() as u64, h.islittle, size, false);
                out.extend_from_slice(&s);
                totalsize += s.len() as u64;
            }
            KOption::Zstr => {
                let s = check_str(args, arg - 1, "pack")?;
                if s.contains(&0) {
                    return Err(arg_error("pack", arg, "string contains zeros"));
                }
                out.extend_from_slice(&s);
                out.push(0);
                totalsize += s.len() as u64 + 1;
            }
            KOption::Padding => {
                out.push(0);
                arg -= 1;
            }
            KOption::PaddAlign | KOption::Nop => {
                arg -= 1;
            }
        }
    }

    Ok(vec![Value::Object(gc.new_string(&out))])
}

pub fn string_packsize(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fmt = check_str(args, 0, "packsize")?;
    let mut h = Header::new();
    let mut totalsize: u64 = 0;
    let mut pos = 0usize;

    while pos < fmt.len() && fmt[pos] != 0 {
        let mut ntoalign = 0u64;
        let (opt, size) = getdetails(
            "packsize",
            &mut h,
            totalsize,
            &fmt,
            &mut pos,
            &mut ntoalign,
        )?;
        if opt == KOption::Str || opt == KOption::Zstr {
            return Err(arg_error("packsize", 1, "variable-length format"));
        }
        let size = size + ntoalign;
        if totalsize > max_size() - size {
            return Err(arg_error("packsize", 1, "format result too large"));
        }
        totalsize += size;
    }

    Ok(vec![Value::Integer(totalsize as i64)])
}

pub fn string_unpack(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let fmt = check_str(args, 0, "unpack")?;
    let data = check_str(args, 1, "unpack")?;
    let ld = data.len() as u64;
    let pos_arg = opt_int(args, 2, 1, "unpack")?;
    let mut pos = posrelat_i(pos_arg, ld) - 1;
    if pos > ld {
        return Err(arg_error("unpack", 3, "initial position out of string"));
    }

    let mut h = Header::new();
    let mut results: Vec<Value> = Vec::new();
    let mut fpos = 0usize;

    while fpos < fmt.len() && fmt[fpos] != 0 {
        let mut ntoalign = 0u64;
        let (opt, size) = getdetails("unpack", &mut h, pos, &fmt, &mut fpos, &mut ntoalign)?;
        if ntoalign + size > ld - pos {
            return Err(arg_error("unpack", 2, "data string too short"));
        }
        pos += ntoalign;
        match opt {
            KOption::Int | KOption::Uint => {
                let res = unpackint(&data, pos, h.islittle, size, opt == KOption::Int)?;
                results.push(Value::Integer(res));
            }
            KOption::Float => {
                let mut b = [0u8; 4];
                b.copy_from_slice(&data[pos as usize..pos as usize + 4]);
                let f = if h.islittle {
                    f32::from_le_bytes(b)
                } else {
                    f32::from_be_bytes(b)
                };
                results.push(Value::Float(f as f64));
            }
            KOption::Number | KOption::Double => {
                let mut b = [0u8; 8];
                b.copy_from_slice(&data[pos as usize..pos as usize + 8]);
                let f = if h.islittle {
                    f64::from_le_bytes(b)
                } else {
                    f64::from_be_bytes(b)
                };
                results.push(Value::Float(f));
            }
            KOption::Char => {
                let start = pos as usize;
                results.push(Value::Object(gc.new_string(&data[start..start + size as usize])));
            }
            KOption::Str => {
                let len = unpackint(&data, pos, h.islittle, size, false)? as u64;
                if len > ld - pos - size {
                    return Err(arg_error("unpack", 2, "data string too short"));
                }
                let start = (pos + size) as usize;
                results.push(Value::Object(
                    gc.new_string(&data[start..start + len as usize]),
                ));
                pos += len;
            }
            KOption::Zstr => {
                let start = pos as usize;
                match data[start..].iter().position(|&b| b == 0) {
                    Some(len) if pos + (len as u64) < ld => {
                        results.push(Value::Object(
                            gc.new_string(&data[start..start + len]),
                        ));
                        pos += len as u64 + 1;
                    }
                    _ => {
                        return Err(arg_error("unpack", 2, "unfinished string for format 'z'"));
                    }
                }
            }
            KOption::Padding | KOption::PaddAlign | KOption::Nop => {}
        }
        pos += size;
    }

    results.push(Value::Integer((pos + 1) as i64));
    Ok(results)
}
