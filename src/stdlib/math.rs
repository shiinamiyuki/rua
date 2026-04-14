//! Lua math library implementation.

use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

type NativeFn = fn(&[Value], &mut Gc) -> Result<Vec<Value>, LuaError>;

/// Helper: extract a float from arg, coercing integers.
fn to_float(v: Value) -> Option<f64> {
    match v {
        Value::Float(f) => Some(f),
        Value::Integer(i) => Some(i as f64),
        _ => None,
    }
}

/// Helper: get arg as float or error.
fn check_number(args: &[Value], idx: usize, fname: &str) -> Result<f64, LuaError> {
    let v = args.get(idx).copied().unwrap_or(Value::Nil);
    to_float(v).ok_or_else(|| {
        LuaError::new(format!(
            "bad argument #{} to '{}' (number expected, got {})",
            idx + 1,
            fname,
            v.type_name()
        ))
    })
}

/// Helper: get arg as integer or error.
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

/// Return int/float preserving input type.
fn integer_or_float(v: Value, result: f64) -> Value {
    match v {
        Value::Integer(_) => {
            let i = result as i64;
            if i as f64 == result {
                Value::Integer(i)
            } else {
                Value::Float(result)
            }
        }
        _ => Value::Float(result),
    }
}

pub fn math_abs(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    match v {
        Value::Integer(i) => Ok(vec![Value::Integer(i.wrapping_abs())]),
        Value::Float(f) => Ok(vec![Value::Float(f.abs())]),
        _ => Err(LuaError::new(format!(
            "bad argument #1 to 'abs' (number expected, got {})",
            v.type_name()
        ))),
    }
}

pub fn math_acos(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "acos")?;
    Ok(vec![Value::Float(x.acos())])
}

pub fn math_asin(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "asin")?;
    Ok(vec![Value::Float(x.asin())])
}

pub fn math_atan(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let y = check_number(args, 0, "atan")?;
    let x = if args.len() > 1 {
        check_number(args, 1, "atan")?
    } else {
        1.0
    };
    Ok(vec![Value::Float(y.atan2(x))])
}

pub fn math_ceil(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "ceil")?;
    let r = x.ceil();
    let i = r as i64;
    if i as f64 == r && r >= i64::MIN as f64 && r <= i64::MAX as f64 {
        Ok(vec![Value::Integer(i)])
    } else {
        Ok(vec![Value::Float(r)])
    }
}

pub fn math_cos(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "cos")?;
    Ok(vec![Value::Float(x.cos())])
}

pub fn math_deg(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "deg")?;
    Ok(vec![Value::Float(x.to_degrees())])
}

pub fn math_exp(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "exp")?;
    Ok(vec![Value::Float(x.exp())])
}

pub fn math_floor(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "floor")?;
    let r = x.floor();
    let i = r as i64;
    if i as f64 == r && r >= i64::MIN as f64 && r <= i64::MAX as f64 {
        Ok(vec![Value::Integer(i)])
    } else {
        Ok(vec![Value::Float(r)])
    }
}

pub fn math_fmod(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    let x = check_number(args, 0, "fmod")?;
    let y = check_number(args, 1, "fmod")?;
    let r = x % y;
    Ok(vec![integer_or_float(v, r)])
}

pub fn math_log(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "log")?;
    let result = if args.len() > 1 {
        let base = check_number(args, 1, "log")?;
        if base == 10.0 {
            x.log10()
        } else if base == 2.0 {
            x.log2()
        } else {
            x.ln() / base.ln()
        }
    } else {
        x.ln()
    };
    Ok(vec![Value::Float(result)])
}

pub fn math_max(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'max' (value expected)",
        ));
    }
    let mut max = args[0];
    for &v in &args[1..] {
        // Use Lua < comparison
        if lua_less_than(max, v)? {
            max = v;
        }
    }
    Ok(vec![max])
}

pub fn math_min(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'min' (value expected)",
        ));
    }
    let mut min = args[0];
    for &v in &args[1..] {
        if lua_less_than(v, min)? {
            min = v;
        }
    }
    Ok(vec![min])
}

/// Simple less-than for numbers (no metamethods).
fn lua_less_than(a: Value, b: Value) -> Result<bool, LuaError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(x < y),
        (Value::Float(x), Value::Float(y)) => Ok(x < y),
        (Value::Integer(x), Value::Float(y)) => Ok((x as f64) < y),
        (Value::Float(x), Value::Integer(y)) => Ok(x < (y as f64)),
        _ => Err(LuaError::new("attempt to compare non-number values")),
    }
}

pub fn math_modf(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "modf")?;
    let trunc = x.trunc();
    let frac = x.fract();
    let i = trunc as i64;
    let int_part = if i as f64 == trunc && trunc >= i64::MIN as f64 && trunc <= i64::MAX as f64 {
        Value::Integer(i)
    } else {
        Value::Float(trunc)
    };
    Ok(vec![int_part, Value::Float(frac)])
}

pub fn math_rad(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "rad")?;
    Ok(vec![Value::Float(x.to_radians())])
}

pub fn math_sin(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "sin")?;
    Ok(vec![Value::Float(x.sin())])
}

pub fn math_sqrt(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "sqrt")?;
    Ok(vec![Value::Float(x.sqrt())])
}

pub fn math_tan(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "tan")?;
    Ok(vec![Value::Float(x.tan())])
}

pub fn math_tointeger(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    match v {
        Value::Integer(i) => Ok(vec![Value::Integer(i)]),
        Value::Float(f) => {
            let i = f as i64;
            if i as f64 == f {
                Ok(vec![Value::Integer(i)])
            } else {
                Ok(vec![Value::Nil]) // fail
            }
        }
        _ => Ok(vec![Value::Nil]), // fail
    }
}

pub fn math_type(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    match v {
        Value::Integer(_) => {
            let s = gc.new_string(b"integer");
            Ok(vec![Value::Object(s)])
        }
        Value::Float(_) => {
            let s = gc.new_string(b"float");
            Ok(vec![Value::Object(s)])
        }
        _ => Ok(vec![Value::Nil]), // fail (not false, Lua uses nil for fail)
    }
}

pub fn math_ult(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let m = check_integer(args, 0, "ult")?;
    let n = check_integer(args, 1, "ult")?;
    Ok(vec![Value::Boolean((m as u64) < (n as u64))])
}

pub fn math_frexp(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let x = check_number(args, 0, "frexp")?;
    if x == 0.0 || x.is_nan() || x.is_infinite() {
        Ok(vec![Value::Float(x), Value::Integer(0)])
    } else {
        // frexp: x = m * 2^e, 0.5 <= |m| < 1
        let bits = x.to_bits();
        let sign = if (bits >> 63) != 0 { -1.0f64 } else { 1.0f64 };
        let exp = ((bits >> 52) & 0x7FF) as i64;
        let mant_bits = bits & 0x000F_FFFF_FFFF_FFFF;
        if exp == 0 {
            // Subnormal — use repeated multiplication
            let mut m = x.abs();
            let mut e = 0i64;
            while m < 0.5 {
                m *= 2.0;
                e -= 1;
            }
            Ok(vec![Value::Float(m * sign), Value::Integer(e)])
        } else {
            let e = exp - 1022;
            let m_bits = (0x3FE0_0000_0000_0000u64) | mant_bits;
            let m = f64::from_bits(m_bits) * sign;
            Ok(vec![Value::Float(m), Value::Integer(e)])
        }
    }
}

pub fn math_ldexp(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let m = check_number(args, 0, "ldexp")?;
    let e = check_integer(args, 1, "ldexp")?;
    Ok(vec![Value::Float(m * (2.0f64).powi(e as i32))])
}

// ── Random number generation (xoshiro256**) ────────────────────────

use std::cell::Cell;

thread_local! {
    static RANDOM_STATE: Cell<[u64; 4]> = const { Cell::new([0; 4]) };
    static RANDOM_INITIALIZED: Cell<bool> = const { Cell::new(false) };
}

fn ensure_random_init() {
    RANDOM_INITIALIZED.with(|init| {
        if !init.get() {
            let seed = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos() as u64)
                .unwrap_or(12345);
            seed_random(seed, 0);
            init.set(true);
        }
    });
}

fn seed_random(x: u64, y: u64) {
    // SplitMix64 to expand seeds
    let mut state = x;
    let mut split = |s: &mut u64| -> u64 {
        *s = s.wrapping_add(0x9e3779b97f4a7c15);
        let mut z = *s;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z ^ (z >> 31)
    };
    let s0 = split(&mut state);
    let s1 = split(&mut state);
    state = y;
    let s2 = split(&mut state);
    let s3 = split(&mut state);
    RANDOM_STATE.with(|cell| cell.set([s0, s1, s2, s3]));
}

fn xoshiro256_next() -> u64 {
    RANDOM_STATE.with(|cell| {
        let mut s = cell.get();
        let result = s[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = s[1] << 17;
        s[2] ^= s[0];
        s[3] ^= s[1];
        s[1] ^= s[2];
        s[0] ^= s[3];
        s[2] ^= t;
        s[3] = s[3].rotate_left(45);
        cell.set(s);
        result
    })
}

pub fn math_random(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    ensure_random_init();
    if args.is_empty() {
        // Return float in [0, 1)
        let r = xoshiro256_next();
        let f = (r >> 11) as f64 / (1u64 << 53) as f64;
        Ok(vec![Value::Float(f)])
    } else if args.len() == 1 {
        let v = args[0];
        let n = match v {
            Value::Integer(0) => {
                return Ok(vec![Value::Integer(xoshiro256_next() as i64)]);
            }
            Value::Integer(i) if i > 0 => i,
            _ => {
                return Err(LuaError::new(
                    "bad argument #1 to 'random' (interval is empty)",
                ))
            }
        };
        // random(n) = random(1, n)
        let r = xoshiro256_next();
        let result = (r % n as u64) as i64 + 1;
        Ok(vec![Value::Integer(result)])
    } else {
        let m = check_integer(args, 0, "random")?;
        let n = check_integer(args, 1, "random")?;
        if m > n {
            return Err(LuaError::new(
                "bad argument #2 to 'random' (interval is empty)",
            ));
        }
        let range = (n as u128 - m as u128 + 1) as u64;
        let r = xoshiro256_next();
        let result = m + (r % range) as i64;
        Ok(vec![Value::Integer(result)])
    }
}

pub fn math_randomseed(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(12345);
        seed_random(seed, 0);
        Ok(vec![
            Value::Integer(seed as i64),
            Value::Integer(0),
        ])
    } else {
        let x = check_integer(args, 0, "randomseed")?;
        let y = if args.len() > 1 {
            check_integer(args, 1, "randomseed")?
        } else {
            0
        };
        seed_random(x as u64, y as u64);
        Ok(vec![Value::Integer(x), Value::Integer(y)])
    }
}

/// Returns all math library functions as (name, function) pairs.
pub fn math_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("abs", math_abs as NativeFn),
        ("acos", math_acos),
        ("asin", math_asin),
        ("atan", math_atan),
        ("ceil", math_ceil),
        ("cos", math_cos),
        ("deg", math_deg),
        ("exp", math_exp),
        ("floor", math_floor),
        ("fmod", math_fmod),
        ("frexp", math_frexp),
        ("ldexp", math_ldexp),
        ("log", math_log),
        ("max", math_max),
        ("min", math_min),
        ("modf", math_modf),
        ("rad", math_rad),
        ("random", math_random),
        ("randomseed", math_randomseed),
        ("sin", math_sin),
        ("sqrt", math_sqrt),
        ("tan", math_tan),
        ("tointeger", math_tointeger),
        ("type", math_type),
        ("ult", math_ult),
    ]
}

/// Returns math library constants as (name, value) pairs.
pub fn math_constants() -> Vec<(&'static str, Value)> {
    vec![
        ("pi", Value::Float(std::f64::consts::PI)),
        ("huge", Value::Float(f64::INFINITY)),
        ("maxinteger", Value::Integer(i64::MAX)),
        ("mininteger", Value::Integer(i64::MIN)),
    ]
}
