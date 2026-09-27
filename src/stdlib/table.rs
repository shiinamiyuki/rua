//! Lua table library implementation.

use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind, GcRef};
use crate::table::Table;
use crate::value::Value;

type NativeFn = fn(&[Value], &mut Gc) -> Result<Vec<Value>, LuaError>;

fn check_table<'a>(args: &[Value], idx: usize, fname: &str) -> Result<GcRef, LuaError> {
    let v = args.get(idx).copied().unwrap_or(Value::Nil);
    match v {
        Value::Object(r) if r.as_object().as_table().is_some() => Ok(r),
        _ => Err(LuaError::new(format!(
            "bad argument #{} to '{}' (table expected, got {})",
            idx + 1,
            fname,
            v.type_name()
        ))),
    }
}

fn opt_integer(args: &[Value], idx: usize) -> Option<i64> {
    args.get(idx).and_then(|v| match v {
        Value::Integer(i) => Some(*i),
        Value::Float(f) => {
            let i = *f as i64;
            if i as f64 == *f { Some(i) } else { None }
        }
        _ => None,
    })
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

/// table.insert(list, [pos,] value)
pub fn table_insert(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut t = check_table(args, 0, "insert")?;
    let tbl = t.as_object_mut().as_table_mut().unwrap();
    let len = tbl.array.len();

    match args.len() {
        // table.insert(t, value) — append
        2 => {
            let val = args[1];
            tbl.array.push(val);
        }
        // table.insert(t, pos, value)
        3 => {
            let pos = check_integer(args, 1, "insert")? as usize;
            let val = args[2];
            if pos < 1 || pos > len + 1 {
                return Err(LuaError::new(format!(
                    "bad argument #2 to 'insert' (position out of bounds)"
                )));
            }
            tbl.array.insert(pos - 1, val);
        }
        _ => {
            return Err(LuaError::new("wrong number of arguments to 'insert'"));
        }
    }
    Ok(vec![])
}

/// table.remove(list [, pos]) → removed value
pub fn table_remove(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut t = check_table(args, 0, "remove")?;
    let tbl = t.as_object_mut().as_table_mut().unwrap();
    let len = tbl.array.len();

    if len == 0 {
        return Ok(vec![Value::Nil]);
    }

    let pos = opt_integer(args, 1).unwrap_or(len as i64) as usize;
    if pos < 1 || pos > len {
        return Ok(vec![Value::Nil]);
    }
    let removed = tbl.array.remove(pos - 1);
    Ok(vec![removed])
}

/// table.concat(list [, sep [, i [, j]]]) → string
pub fn table_concat(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let t = check_table(args, 0, "concat")?;
    let tbl = t.as_object().as_table().unwrap();
    let len = tbl.array.len();

    let sep = if args.len() > 1 {
        match args[1] {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                r.as_object().as_string().unwrap().as_bytes().to_vec()
            }
            Value::Integer(i) => format!("{i}").into_bytes(),
            Value::Float(f) => format!("{f}").into_bytes(),
            Value::Nil => Vec::new(),
            _ => {
                return Err(LuaError::new(
                    "bad argument #2 to 'concat' (string expected)",
                ))
            }
        }
    } else {
        Vec::new()
    };

    let i = opt_integer(args, 2).unwrap_or(1) as usize;
    let j = opt_integer(args, 3).unwrap_or(len as i64) as usize;

    let mut result = Vec::new();
    for idx in i..=j {
        if idx > i && !sep.is_empty() {
            result.extend_from_slice(&sep);
        }
        if idx >= 1 && idx <= len {
            let val = tbl.array[idx - 1];
            match val {
                Value::Object(r) if r.as_object().as_string().is_some() => {
                    result.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
                }
                Value::Integer(n) => {
                    result.extend_from_slice(format!("{n}").as_bytes());
                }
                Value::Float(f) => {
                    result.extend_from_slice(format!("{f}").as_bytes());
                }
                _ => {
                    return Err(LuaError::new(format!(
                        "invalid value ({})) at index {} in table for 'concat'",
                        val.type_name(),
                        idx
                    )));
                }
            }
        }
    }
    let s = gc.new_string(&result);
    Ok(vec![Value::Object(s)])
}

/// table.move(a1, f, e, t [, a2]) → a2
pub fn table_move(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut a1 = check_table(args, 0, "move")?;
    let f = check_integer(args, 1, "move")?;
    let e = check_integer(args, 2, "move")?;
    let t = check_integer(args, 3, "move")?;

    // If a2 is provided, use it; otherwise use a1
    let has_a2 = args.len() > 4 && !args[4].is_nil();

    if has_a2 {
        let mut a2 = check_table(args, 4, "move")?;
        // Read from a1, write to a2
        let src = a1.as_object().as_table().unwrap();
        let mut values = Vec::new();
        for i in f..=e {
            let key = Value::Integer(i);
            values.push(src.raw_get(&key));
        }
        let dst = a2.as_object_mut().as_table_mut().unwrap();
        for (idx, val) in values.into_iter().enumerate() {
            let key = Value::Integer(t + idx as i64);
            dst.raw_set(key, val);
        }
        Ok(vec![Value::Object(a2)])
    } else {
        // Move within same table
        let tbl = a1.as_object_mut().as_table_mut().unwrap();
        let mut values = Vec::new();
        for i in f..=e {
            let key = Value::Integer(i);
            values.push(tbl.raw_get(&key));
        }
        for (idx, val) in values.into_iter().enumerate() {
            let key = Value::Integer(t + idx as i64);
            tbl.raw_set(key, val);
        }
        Ok(vec![Value::Object(a1)])
    }
}

/// table.pack(...) → table with n field
pub fn table_pack(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let mut t = Table::with_capacity(args.len(), 1);
    for (i, val) in args.iter().enumerate() {
        t.array.push(*val);
        // Ensure array indices are continuous from 1
        let _idx = i + 1;
    }
    let n_key = Value::Object(gc.new_string(b"n"));
    t.raw_set(n_key, Value::Integer(args.len() as i64));
    let r = gc.new_table(t);
    Ok(vec![Value::Object(r)])
}

/// table.unpack(list [, i [, j]]) → list[i], ..., list[j]
pub fn table_unpack(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let t = check_table(args, 0, "unpack")?;
    let tbl = t.as_object().as_table().unwrap();
    let len = tbl.array.len();

    let i = opt_integer(args, 1).unwrap_or(1);
    let j = opt_integer(args, 2).unwrap_or(len as i64);

    let mut results = Vec::new();
    for idx in i..=j {
        if idx >= 1 && (idx as usize) <= len {
            results.push(tbl.array[idx as usize - 1]);
        } else {
            results.push(Value::Nil);
        }
    }
    Ok(results)
}

/// table.sort(list [, comp])
/// table.create(narr [, nrec]) → table
/// Lua 5.5 extension: pre-allocate array and hash parts.
pub fn table_create(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let narr = check_integer(args, 0, "create")?;
    let nrec = opt_integer(args, 1).unwrap_or(0);
    if (narr as u64) > i32::MAX as u64 {
        return Err(LuaError::new(
            "bad argument #1 to 'create' (out of range)",
        ));
    }
    if (nrec as u64) > i32::MAX as u64 {
        return Err(LuaError::new(
            "bad argument #2 to 'create' (out of range)",
        ));
    }
    if nrec > (1i64 << 30) {
        return Err(LuaError::new("table overflow"));
    }
    let t = Table::with_capacity(narr as usize, nrec as usize);
    let r = gc.new_table(t);
    Ok(vec![Value::Object(r)])
}

pub fn table_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("pack", table_pack),
        ("create", table_create),
    ]
}
