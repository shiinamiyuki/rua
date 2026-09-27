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
    if let Some(Value::Integer(i)) = args.first() {
        return Ok(vec![Value::Integer(*i)]);
    }
    let x = check_number(args, 0, "ceil")?;
    let r = x.ceil();
    let i = r as i64;
    if i as f64 == r && r >= -(2f64.powi(63)) && r < 2f64.powi(63) {
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
    if let Some(Value::Integer(i)) = args.first() {
        return Ok(vec![Value::Integer(*i)]);
    }
    let x = check_number(args, 0, "floor")?;
    let r = x.floor();
    let i = r as i64;
    if i as f64 == r && r >= -(2f64.powi(63)) && r < 2f64.powi(63) {
        Ok(vec![Value::Integer(i)])
    } else {
        Ok(vec![Value::Float(r)])
    }
}

pub fn math_fmod(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Two integers use integer modulo (exact); otherwise floating fmod.
    if let (Some(Value::Integer(a)), Some(Value::Integer(b))) = (args.first(), args.get(1)) {
        let b = *b;
        if b == 0 {
            return Err(LuaError::new("bad argument #2 to 'fmod' (zero)"));
        }
        let r = if b == -1 {
            0
        } else {
            a.wrapping_rem(b)
        };
        return Ok(vec![Value::Integer(r)]);
    }
    check_number(args, 0, "fmod")?;
    check_number(args, 1, "fmod")?;
    let x = check_number(args, 0, "fmod")?;
    let y = check_number(args, 1, "fmod")?;
    let r = x % y;
    Ok(vec![Value::Float(r)])
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
    // C's modf returns a signed zero fractional part for infinities.
    let (trunc, frac) = if x.is_infinite() {
        (x, 0.0 * x.signum())
    } else {
        (x.trunc(), x.fract())
    };
    let i = trunc as i64;
    let int_part = if i as f64 == trunc && trunc >= -(2f64.powi(63)) && trunc < 2f64.powi(63) {
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
            if f.fract() == 0.0 && f >= -(2f64.powi(63)) && f < 2f64.powi(63) {
                Ok(vec![Value::Integer(f as i64)])
            } else {
                Ok(vec![Value::Nil]) // fail
            }
        }
        // Numeric strings are accepted.
        Value::Object(r) if r.as_object().as_string().is_some() => {
            match crate::stdlib::io::parse_lua_number(r.as_object().as_string().unwrap().as_bytes())
            {
                Some(Value::Integer(i)) => Ok(vec![Value::Integer(i)]),
                Some(Value::Float(f))
                    if f.fract() == 0.0 && f >= -(2f64.powi(63)) && f < 2f64.powi(63) =>
                {
                    Ok(vec![Value::Integer(f as i64)])
                }
                _ => Ok(vec![Value::Nil]),
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
    // Reference Lua's seeding: state = {n1, 0xff, n2, 0}, then discard
    // 16 values to spread the seed.
    RANDOM_STATE.with(|cell| cell.set([x, 0xff, y, 0]));
    for _ in 0..16 {
        xoshiro256_next();
    }
}

/// Reference Lua's uniform projection of a random integer into [0, n].
fn project(mut ran: u64, n: u64) -> u64 {
    let mut lim = n;
    let mut sh: u32 = 1;
    while lim & lim.wrapping_add(1) != 0 {
        lim |= lim >> sh;
        sh *= 2;
    }
    loop {
        ran &= lim;
        if ran <= n {
            return ran;
        }
        ran = xoshiro256_next();
    }
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
    if args.len() > 2 {
        return Err(LuaError::new("wrong number of arguments"));
    }
    if args.is_empty() {
        // Return float in [0, 1)
        let r = xoshiro256_next();
        let f = (r >> 11) as f64 / (1u64 << 53) as f64;
        Ok(vec![Value::Float(f)])
    } else if args.len() == 1 {
        let n = check_integer(args, 0, "random")?;
        if n == 0 {
            return Ok(vec![Value::Integer(xoshiro256_next() as i64)]);
        }
        if n < 1 {
            return Err(LuaError::new(
                "bad argument #1 to 'random' (interval is empty)",
            ));
        }
        let r = xoshiro256_next();
        let result = project(r, n as u64 - 1) as i64 + 1;
        Ok(vec![Value::Integer(result)])
    } else {
        let m = check_integer(args, 0, "random")?;
        let n = check_integer(args, 1, "random")?;
        if m > n {
            return Err(LuaError::new(
                "bad argument #2 to 'random' (interval is empty)",
            ));
        }
        let r = xoshiro256_next();
        let p = project(r, (n as u64).wrapping_sub(m as u64));
        Ok(vec![Value::Integer((p.wrapping_add(m as u64)) as i64)])
    }
}

pub fn math_randomseed(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    ensure_random_init();
    if args.is_empty() {
        ensure_random_init();
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(12345);
        let n2 = xoshiro256_next();
        seed_random(seed, n2);
        Ok(vec![
            Value::Integer(seed as i64),
            Value::Integer(n2 as i64),
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
