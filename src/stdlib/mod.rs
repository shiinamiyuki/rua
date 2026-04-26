//! Standard library implementations.

pub mod coroutine;
pub mod debug;
pub mod io;
pub mod math;
pub mod os;
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
    let v = args.first().copied().unwrap_or(Value::Nil);
    let name = v.type_name();
    Ok(vec![Value::Object(gc.new_string(name.as_bytes()))])
}

/// tostring(v) — Convert v to a string.
pub fn lua_tostring(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    let s = format!("{v}");
    Ok(vec![Value::Object(gc.new_string(s.as_bytes()))])
}

/// tonumber(v [, base]) — Convert v to a number.
pub fn lua_tonumber(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let v = args.first().copied().unwrap_or(Value::Nil);
    let base = args
        .get(1)
        .and_then(|b| b.as_integer())
        .unwrap_or(10) as u32;

    match v {
        Value::Integer(_) => Ok(vec![v]),
        Value::Float(_) => Ok(vec![v]),
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            let text = std::str::from_utf8(s.as_bytes()).unwrap_or("");
            let text = text.trim();
            if base == 10 {
                // Try integer first, then float
                if let Ok(n) = text.parse::<i64>() {
                    return Ok(vec![Value::Integer(n)]);
                }
                if let Ok(n) = text.parse::<f64>() {
                    return Ok(vec![Value::Float(n)]);
                }
                // Try hex notation
                if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    if let Ok(n) = i64::from_str_radix(hex, 16) {
                        return Ok(vec![Value::Integer(n)]);
                    }
                }
            } else {
                if let Ok(n) = i64::from_str_radix(text, base) {
                    return Ok(vec![Value::Integer(n)]);
                }
            }
            Ok(vec![Value::Nil])
        }
        _ => Ok(vec![Value::Nil]),
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
            let iter_closure =
                crate::closure::Closure::new_native("ipairs_iterator", iter_fn);
            let iter_ref = gc.new_closure(iter_closure);
            Ok(vec![Value::Object(iter_ref), table, Value::Integer(0)])
        }
        _ => Err(LuaError::new("bad argument #1 to 'ipairs' (table expected)")),
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
            let next_closure =
                crate::closure::Closure::new_native("pairs_next", next_fn);
            let next_ref = gc.new_closure(next_closure);
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
    let index = &args[0];
    // select('#', ...) returns count of remaining args
    if let Value::Object(r) = index {
        if let Some(s) = r.as_object().as_string() {
            if s.as_bytes() == b"#" {
                return Ok(vec![Value::Integer(args.len() as i64 - 1)]);
            }
        }
    }
    let n = match index.as_integer() {
        Some(n) => n,
        None => {
            return Err(LuaError::new(
                "bad argument #1 to 'select' (number or string expected)",
            ))
        }
    };
    if n < 1 || n as usize > args.len() - 1 {
        return Err(LuaError::new("bad argument #1 to 'select' (index out of range)"));
    }
    Ok(args[n as usize..].to_vec())
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
    match v {
        Value::Object(r) if r.as_object().as_table().is_some() => {
            match r.as_object().as_table().unwrap().metatable {
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
        _ => Ok(vec![Value::Nil]),
    }
}
