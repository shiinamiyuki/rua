//! Debug library implementation.

use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind};
use crate::value::Value;

/// debug.getmetatable(value) — return raw metatable bypassing __metatable
pub fn debug_getmetatable(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'getmetatable' (value expected)",
        ));
    }
    let val = args[0];
    match val {
        Value::Object(r) => {
            let obj = r.as_object();
            match &obj.kind {
                GcObjectKind::Table(t) => {
                    if let Some(mt) = t.metatable {
                        Ok(vec![Value::Object(mt)])
                    } else {
                        Ok(vec![Value::Nil])
                    }
                }
                GcObjectKind::Userdata(ud) => {
                    if let Some(mt) = ud.metatable {
                        Ok(vec![Value::Object(mt)])
                    } else {
                        Ok(vec![Value::Nil])
                    }
                }
                _ => Ok(vec![Value::Nil]),
            }
        }
        _ => Ok(vec![Value::Nil]),
    }
}

/// debug.setmetatable(value, table) — set metatable directly, bypassing __metatable
pub fn debug_setmetatable(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.len() < 2 {
        return Err(LuaError::new(
            "bad argument #1 to 'setmetatable' (value expected)",
        ));
    }
    let val = args[0];
    let mt = match args[1] {
        Value::Nil => None,
        Value::Object(r) => {
            if r.as_object().as_table().is_some() {
                Some(r)
            } else {
                return Err(LuaError::new(
                    "bad argument #2 to 'setmetatable' (nil or table expected)",
                ));
            }
        }
        _ => {
            return Err(LuaError::new(
                "bad argument #2 to 'setmetatable' (nil or table expected)",
            ));
        }
    };
    match val {
        Value::Object(r) => {
            {
                let obj = r.as_object_mut();
                match &mut obj.kind {
                    GcObjectKind::Table(t) => {
                        t.metatable = mt;
                    }
                    GcObjectKind::Userdata(ud) => {
                        ud.metatable = mt;
                    }
                    _ => {
                        return Err(LuaError::new(
                            "bad argument #1 to 'setmetatable' (table or userdata expected)",
                        ));
                    }
                }
            }

            // Refresh weak-mode flags (tables only) and register __gc finalizer.
            if let Some(mt_ref) = mt {
                if let Some(mt_table) = mt_ref.as_object().as_table() {
                    if r.as_object().as_table().is_some() {
                        let mode_key = _gc.new_string(b"__mode");
                        let mode_val = mt_table.raw_get(&Value::Object(mode_key));
                        let mode_bytes = if let Value::Object(sr) = mode_val {
                            sr.as_object().as_string().map(|s| s.as_bytes().to_vec())
                        } else {
                            None
                        };
                        r.as_object_mut()
                            .as_table_mut()
                            .unwrap()
                            .set_weak_mode(mode_bytes.as_deref());
                    }
                    let gc_key = _gc.new_string(b"__gc");
                    let gc_val = mt_table.raw_get(&Value::Object(gc_key));
                    if !gc_val.is_nil() {
                        _gc.register_finalizer(r);
                    }
                }
            } else if r.as_object().as_table().is_some() {
                r.as_object_mut().as_table_mut().unwrap().set_weak_mode(None);
            }
        }
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'setmetatable' (table or userdata expected)",
            ));
        }
    }
    Ok(vec![val])
}

/// debug.getuservalue(u, n) — return the n-th user value
pub fn debug_getuservalue(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() {
        return Err(LuaError::new(
            "bad argument #1 to 'getuservalue' (userdata expected)",
        ));
    }
    let ud_ref = match args[0] {
        Value::Object(r) if r.as_object().as_userdata().is_some() => r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'getuservalue' (userdata expected)",
            ));
        }
    };
    let n = if args.len() >= 2 {
        match args[1] {
            Value::Integer(i) => i as usize,
            _ => 1,
        }
    } else {
        1
    };
    let ud = ud_ref.as_object().as_userdata().unwrap();
    if n >= 1 && n <= ud.user_values.len() {
        Ok(vec![ud.user_values[n - 1], Value::Boolean(true)])
    } else {
        Ok(vec![Value::Nil, Value::Boolean(false)])
    }
}

/// debug.setuservalue(udata, value, n) — set the n-th user value
pub fn debug_setuservalue(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.len() < 2 {
        return Err(LuaError::new(
            "bad argument #1 to 'setuservalue' (userdata expected)",
        ));
    }
    let ud_ref = match args[0] {
        Value::Object(r) if r.as_object().as_userdata().is_some() => r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'setuservalue' (userdata expected)",
            ));
        }
    };
    let value = args[1];
    let n = if args.len() >= 3 {
        match args[2] {
            Value::Integer(i) => i as usize,
            _ => 1,
        }
    } else {
        1
    };
    let ud = ud_ref.as_object_mut()
        .as_userdata_mut()
        .unwrap();
    // Extend user_values if needed
    if n >= 1 && n <= ud.user_values.len() {
        ud.user_values[n - 1] = value;
        Ok(vec![args[0]])
    } else if n >= 1 {
        // Grow the user_values vector
        while ud.user_values.len() < n {
            ud.user_values.push(Value::Nil);
        }
        ud.user_values[n - 1] = value;
        Ok(vec![args[0]])
    } else {
        Ok(vec![Value::Nil])
    }
}

/// debug.sethook — stub (no-op)
pub fn debug_sethook(_args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    Ok(vec![])
}

/// debug.gethook — stub (returns nil)
pub fn debug_gethook(_args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    Ok(vec![Value::Nil])
}

/// Return (name, NativeFn) pairs for debug functions that don't need VM access.
pub fn debug_native_functions() -> Vec<(&'static str, crate::closure::NativeFn)> {
    vec![
        ("getmetatable", debug_getmetatable),
        ("setmetatable", debug_setmetatable),
        ("getuservalue", debug_getuservalue),
        ("setuservalue", debug_setuservalue),
        ("sethook", debug_sethook),
        ("gethook", debug_gethook),
    ]
}
