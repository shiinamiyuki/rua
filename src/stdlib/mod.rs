//! Standard library implementations.

pub mod coroutine;
pub mod debug;
pub mod io;
pub mod math;
pub mod os;
pub mod pack;
pub mod package;
pub mod string;
pub mod table;
pub mod utf8;

use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind};
use crate::table::Table;
use crate::value::Value;

/// print(...) — Print values separated by tabs, followed by newline.
pub fn lua_print(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!("\t");
        }
        print!("{arg}");
    }
    println!();
    Ok(vec![])
}

/// type(v) — Return the type of v as a string.
pub fn lua_type(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new("bad argument #1 to 'type' (value expected)"));
    }
    let v = args.first().copied().unwrap_or(Value::Nil);
    let name = v.type_name();
    Ok(vec![Value::Object(gc.new_string(name.as_bytes()))])
}

/// tostring(v) — Convert v to a string.
pub fn lua_tostring(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'tostring' (value expected)",
        ));
    }
    let v = args.first().copied().unwrap_or(Value::Nil);
    let s = format!("{v}");
    Ok(vec![Value::Object(gc.new_string(s.as_bytes()))])
}

/// tonumber(v [, base]) — Convert v to a number.
pub fn lua_tonumber(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'tonumber' (value expected)",
        ));
    }
    let v = args.first().copied().unwrap_or(Value::Nil);
    let explicit_base = args.get(1).map_or(false, |b| !b.is_nil());
    let base = args
        .get(1)
        .and_then(|b| b.as_integer())
        .unwrap_or(10);
    if explicit_base && !(2..=36).contains(&base) {
        return Err(LuaError::new(
            "bad argument #2 to 'tonumber' (base out of range)",
        ));
    }
    let base = base as u32;

    match v {
        Value::Integer(_) => Ok(vec![v]),
        Value::Float(_) => Ok(vec![v]),
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = std::str::from_utf8(s.as_bytes()).unwrap_or("");
            let text = text.trim();
            if !explicit_base {
                // Full Lua numeral grammar (hex wraps, decimal overflow
                // becomes a float, hex floats supported).
                if let Some(v) = crate::stdlib::io::parse_lua_number(s.as_bytes()) {
                    return Ok(vec![v]);
                }
            } else if let Some(n) = parse_base_integer(text, base) {
                return Ok(vec![Value::Integer(n)]);
            }
            Ok(vec![Value::Nil])
        }
        _ => Ok(vec![Value::Nil]),
    }
}

/// Strict base-N integer parsing for `tonumber(s, base)`: optional sign
/// and surrounding spaces, digits valid for the base, overflow -> None.
fn parse_base_integer(text: &str, base: u32) -> Option<i64> {
    let text = text.trim();
    let (neg, body) = if let Some(rest) = text.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = text.strip_prefix('+') {
        (false, rest)
    } else {
        (false, text)
    };
    if body.is_empty() {
        return None;
    }
    let mut acc: u64 = 0;
    for c in body.chars() {
        let d = c.to_digit(base)?;
        acc = acc.checked_mul(base as u64)?.checked_add(d as u64)?;
    }
    if neg {
        if acc <= (i64::MAX as u64) + 1 {
            Some((acc as i64).wrapping_neg())
        } else {
            None
        }
    } else if acc <= i64::MAX as u64 {
        Some(acc as i64)
    } else {
        None
    }
}

/// assert(v [, message]) — Error if v is falsy.
pub fn lua_assert(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    if !v.is_truthy() {
        let msg = args
            .get(1)
            .map(|m| format!("{m}"))
            .unwrap_or_else(|| "assertion failed!".to_string());
        return Err(LuaError::new(msg));
    }
    // Return all arguments
    Ok(args.to_vec())
}

/// error(message) — Raise an error.
pub fn lua_error(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let msg = args.first().copied().unwrap_or(Value::Nil);
    Err(LuaError::with_value(msg))
}

/// ipairs(t) — Return an iterator for array part of table.
pub fn lua_ipairs(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    match table {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            // Return the iterator function, the table, and 0
            let iter_fn = |args: &[Value], _gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
                let table = args.first().copied().unwrap_or(Value::Nil);
                let index = args
                    .get(1)
                    .and_then(|v| v.as_integer())
                    .unwrap_or(0);
                let next_index = index + 1;
                let key = Value::Integer(next_index);
                match table {
                    Value::Object(r) if r.as_object().as_table().is_some() => {
                        let val = r.as_object().as_table().unwrap().raw_get(&key);
                        if val.is_nil() {
                            Ok(vec![Value::Nil])
                        } else {
                            Ok(vec![Value::Integer(next_index), val])
                        }
                    }
                    _ => Ok(vec![Value::Nil]),
                }
            };
            let iter_ref = match gc.ipairs_iter {
                Some(r) => r,
                None => {
                    let iter_closure =
                        crate::closure::Closure::new_native("ipairs_iterator", iter_fn);
                    let r = gc.new_closure(iter_closure);
                    gc.ipairs_iter = Some(r);
                    r
                }
            };
            Ok(vec![Value::Object(iter_ref), table, Value::Integer(0)])
        }
        _ => Err(LuaError::new("bad argument #1 to 'ipairs' (table expected)")),
    }
}

/// next(table [, index]) — Return the next key/value pair of a table.
pub fn lua_next(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    let key = args.get(1).copied().unwrap_or(Value::Nil);
    match table {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            let t = r.as_object().as_table().unwrap();
            if !key.is_nil() && !t.has_key(&key) {
                return Err(LuaError::new("invalid key to 'next'"));
            }
            match t.next(&key) {
                Some((k, v)) => Ok(vec![k, v]),
                None => Ok(vec![Value::Nil]),
            }
        }
        _ => Err(LuaError::new("bad argument #1 to 'next' (table expected)")),
    }
}

/// pairs(t) — Return next, t, nil for generic for.
pub fn lua_pairs(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    match table {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            // Return next function, the table, and nil
            let next_fn = |args: &[Value], _gc: &mut Gc| -> Result<Vec<Value>, LuaError> {
                let table = args.first().copied().unwrap_or(Value::Nil);
                let key = args.get(1).copied().unwrap_or(Value::Nil);
                match table {
                    Value::Object(r) if r.as_object().as_table().is_some() => {
                        match r.as_object().as_table().unwrap().next(&key) {
                            Some((k, v)) => Ok(vec![k, v]),
                            None => Ok(vec![Value::Nil]),
                        }
                    }
                    _ => Ok(vec![Value::Nil]),
                }
            };
            let next_ref = match gc.pairs_next {
                Some(r) => r,
                None => {
                    let next_closure =
                        crate::closure::Closure::new_native("pairs_next", next_fn);
                    let r = gc.new_closure(next_closure);
                    gc.pairs_next = Some(r);
                    r
                }
            };
            Ok(vec![Value::Object(next_ref), table, Value::Nil])
        }
        _ => Err(LuaError::new("bad argument #1 to 'pairs' (table expected)")),
    }
}

/// rawget(table, index) — Get without metamethods.
pub fn lua_rawget(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    let key = args.get(1).copied().unwrap_or(Value::Nil);
    match table {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            Ok(vec![r.as_object().as_table().unwrap().raw_get(&key)])
        }
        _ => Err(LuaError::new(
            "bad argument #1 to 'rawget' (table expected)",
        )),
    }
}

/// rawset(table, index, value) — Set without metamethods.
pub fn lua_rawset(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    let key = args.get(1).copied().unwrap_or(Value::Nil);
    let val = args.get(2).copied().unwrap_or(Value::Nil);
    if matches!(key, Value::Float(f) if f.is_nan()) {
        return Err(LuaError::new("table index is NaN"));
    }
    match table {
        Value::Object(mut r) if r.as_object().as_table().is_some() => {
            r.as_object_mut().as_table_mut().unwrap().raw_set(key, val);
            Ok(vec![table])
        }
        _ => Err(LuaError::new(
            "bad argument #1 to 'rawset' (table expected)",
        )),
    }
}

/// rawlen(v) — Length without metamethods.
pub fn lua_rawlen(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new("bad argument #1 to 'rawlen' (table or string expected)"));
    }
    let v = args.first().copied().unwrap_or(Value::Nil);
    match v {
        Value::Object(r) => match &r.as_object().kind {
            GcObjectKind::Table(t) => Ok(vec![Value::Integer(t.length() as i64)]),
            GcObjectKind::String(s) => Ok(vec![Value::Integer(s.len() as i64)]),
            _ => Err(LuaError::new(
                "bad argument #1 to 'rawlen' (table or string expected)",
            )),
        },
        _ => Err(LuaError::new(
            "bad argument #1 to 'rawlen' (table or string expected)",
        )),
    }
}

/// rawequal(v1, v2) — Equality without metamethods.
pub fn lua_rawequal(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let a = args.first().copied().unwrap_or(Value::Nil);
    let b = args.get(1).copied().unwrap_or(Value::Nil);
    Ok(vec![Value::Boolean(a == b)])
}

/// select(index, ...) — Select from arguments.
pub fn lua_select(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'select' (number or string expected, got no value)",
        ));
    }
    let total = args.len() as i64;
    let index = &args[0];
    // select('#', ...) returns count of remaining args
    if let Value::Object(r) = index {
        if let Some(s) = r.as_object().as_string() {
            if s.as_bytes().first() == Some(&b'#') {
                return Ok(vec![Value::Integer(total - 1)]);
            }
        }
    }
    let mut i = match index {
        Value::Integer(n) => *n,
        Value::Float(f) if f.floor() == *f => *f as i64,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'select' (number expected)",
            ))
        }
    };
    if i < 0 {
        i = total + i;
    } else if i > total {
        i = total;
    }
    if i < 1 {
        return Err(LuaError::new(
            "bad argument #1 to 'select' (index out of range)",
        ));
    }
    Ok(args[i as usize..].to_vec())
}

/// setmetatable(table, metatable) — Set metatable.
pub fn lua_setmetatable(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let table = args.first().copied().unwrap_or(Value::Nil);
    let mt = args.get(1).copied().unwrap_or(Value::Nil);
    match table {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            // Check __metatable protection on existing metatable
            if let Some(existing_mt) = r.as_object().as_table().unwrap().metatable {
                if let Some(mt_table) = existing_mt.as_object().as_table() {
                    // Look for __metatable field
                    let mm_key_ref = _gc.find_string(b"__metatable");
                    if let Some(key_ref) = mm_key_ref {
                        let mm_val = mt_table.raw_get(&Value::Object(key_ref));
                        if !mm_val.is_nil() {
                            return Err(LuaError::new("cannot change a protected metatable"));
                        }
                    }
                }
            }
            let mt_ref = match mt {
                Value::Nil => None,
                Value::Object(mr) if mr.as_object().as_table().is_some() => Some(mr),
                _ => return Err(LuaError::new("bad argument #2 to 'setmetatable'")),
            };
            r.as_object_mut().as_table_mut().unwrap().metatable = mt_ref;

            // Update weak-mode flags from `__mode`.
            let mode_bytes: Option<Vec<u8>> = mt_ref.and_then(|mt_ref| {
                let mt_table = mt_ref.as_object().as_table()?;
                let mode_key = _gc.new_string(b"__mode");
                let v = mt_table.raw_get(&Value::Object(mode_key));
                if let Value::Object(sr) = v {
                    sr.as_object().as_string().map(|s| s.as_bytes().to_vec())
                } else {
                    None
                }
            });
            r.as_object_mut()
                .as_table_mut()
                .unwrap()
                .set_weak_mode(mode_bytes.as_deref());

            // Register `__gc` finalizer if present and non-nil.
            if let Some(mt_ref_v) = mt_ref {
                if let Some(mt_table) = mt_ref_v.as_object().as_table() {
                    let gc_key = _gc.new_string(b"__gc");
                    let gc_val = mt_table.raw_get(&Value::Object(gc_key));
                    if !gc_val.is_nil() {
                        _gc.register_finalizer(r);
                    }
                }
            }

            Ok(vec![table])
        }
        _ => Err(LuaError::new(
            "bad argument #1 to 'setmetatable' (table expected)",
        )),
    }
}

/// getmetatable(object) — Get metatable (returns __metatable field if set).
pub fn lua_getmetatable(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    let mt = match v {
        Value::Nil => _gc.mt_nil,
        Value::Boolean(_) => _gc.mt_bool,
        Value::Integer(_) | Value::Float(_) => _gc.mt_number,
        Value::Object(r) => match &r.as_object().kind {
            GcObjectKind::Table(t) => t.metatable,
            GcObjectKind::Userdata(ud) => ud.metatable,
            GcObjectKind::String(_) => _gc.mt_string,
            GcObjectKind::Closure(_) => _gc.mt_function,
            GcObjectKind::Thread(_) => _gc.mt_thread,
        },
    };
    match mt {
        Some(mt) => {
            // Check for __metatable field — return it instead of the actual metatable
            if let Some(mt_table) = mt.as_object().as_table() {
                if let Some(key_ref) = _gc.find_string(b"__metatable") {
                    let mm_val = mt_table.raw_get(&Value::Object(key_ref));
                    if !mm_val.is_nil() {
                        return Ok(vec![mm_val]);
                    }
                }
            }
            Ok(vec![Value::Object(mt)])
        }
        None => Ok(vec![Value::Nil]),
    }
}
