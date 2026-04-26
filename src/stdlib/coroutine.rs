//! coroutine library: create, status, wrap.
//!
//! resume, yield, running, isyieldable, close are handled as VM specials.

use crate::closure::Closure;
use crate::coroutine::{Coroutine, CoroutineStatus};
use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind, GcRef};
use crate::value::Value;

/// coroutine.create(f) — Create a new suspended coroutine whose body is f.
pub fn lua_coroutine_create(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let func = args.first().copied().unwrap_or(Value::Nil);
    let func_ref = match func {
        Value::Object(r) if r.as_object().as_closure().is_some() => r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'create' (function expected)",
            ))
        }
    };

    let co = Coroutine {
        status: CoroutineStatus::Suspended,
        stack: vec![Value::Nil; 64],
        frames: Vec::new(),
        open_upvalues: Vec::new(),
        tbc_slots: Vec::new(),
        top: 0,
        body: Some(func_ref),
        yield_result_base: 0,
        yield_num_results: 0,
        is_main: false,
        pcall_guards: Vec::new(),
    };
    let thread_ref = gc.new_thread(co);
    Ok(vec![Value::Object(thread_ref)])
}

/// coroutine.status(co) — Return the status of coroutine co as a string.
pub fn lua_coroutine_status(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let co_val = args.first().copied().unwrap_or(Value::Nil);
    match co_val {
        Value::Object(r) if r.as_object().as_coroutine().is_some() => {
            let status = r.as_object().as_coroutine().unwrap().status;
            let s = gc.new_string(status.as_str().as_bytes());
            Ok(vec![Value::Object(s)])
        }
        _ => Err(LuaError::new(
            "bad argument #1 to 'status' (coroutine expected)",
        )),
    }
}

/// coroutine.wrap(f) — Create a coroutine and return an iterator function.
pub fn lua_coroutine_wrap(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let func = args.first().copied().unwrap_or(Value::Nil);
    let func_ref = match func {
        Value::Object(r) if r.as_object().as_closure().is_some() => r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'wrap' (function expected)",
            ))
        }
    };

    let co = Coroutine {
        status: CoroutineStatus::Suspended,
        stack: vec![Value::Nil; 64],
        frames: Vec::new(),
        open_upvalues: Vec::new(),
        tbc_slots: Vec::new(),
        top: 0,
        body: Some(func_ref),
        yield_result_base: 0,
        yield_num_results: 0,
        is_main: false,
        pcall_guards: Vec::new(),
    };
    let thread_ref = gc.new_thread(co);
    let wrap_closure = Closure::WrapIterator(thread_ref);
    let wrap_ref = gc.new_closure(wrap_closure);
    Ok(vec![Value::Object(wrap_ref)])
}
