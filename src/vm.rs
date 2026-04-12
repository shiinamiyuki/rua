//! Virtual machine: register-based bytecode execution.

use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::*;
use crate::closure::*;
use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind, GcRef};
use crate::table::Table;
use crate::value::Value;

// ── Metamethod tag names ───────────────────────────────────────────

const MM_ADD: &[u8] = b"__add";
const MM_SUB: &[u8] = b"__sub";
const MM_MUL: &[u8] = b"__mul";
const MM_DIV: &[u8] = b"__div";
const MM_MOD: &[u8] = b"__mod";
const MM_POW: &[u8] = b"__pow";
const MM_UNM: &[u8] = b"__unm";
const MM_IDIV: &[u8] = b"__idiv";
const MM_BAND: &[u8] = b"__band";
const MM_BOR: &[u8] = b"__bor";
const MM_BXOR: &[u8] = b"__bxor";
const MM_BNOT: &[u8] = b"__bnot";
const MM_SHL: &[u8] = b"__shl";
const MM_SHR: &[u8] = b"__shr";
const MM_CONCAT: &[u8] = b"__concat";
const MM_LEN: &[u8] = b"__len";
const MM_EQ: &[u8] = b"__eq";
const MM_LT: &[u8] = b"__lt";
const MM_LE: &[u8] = b"__le";
const MM_INDEX: &[u8] = b"__index";
const MM_NEWINDEX: &[u8] = b"__newindex";
const MM_CALL: &[u8] = b"__call";
const MM_TOSTRING: &[u8] = b"__tostring";
const MM_METATABLE: &[u8] = b"__metatable";
const MM_CLOSE: &[u8] = b"__close";

// ── Call frame ─────────────────────────────────────────────────────

/// A single activation record on the call stack.
struct CallFrame {
    /// GcRef to the closure being executed.
    closure: GcRef,
    /// Cached prototype (Rc clone, avoids going through GcRef each instruction).
    proto: Rc<Proto>,
    /// Cached upvalues from the closure.
    upvalues: Vec<UpvalueRef>,
    /// Base register index in the shared stack.
    base: usize,
    /// Program counter (index into proto.code).
    pc: usize,
    /// Where to place results in the caller's stack (absolute index).
    result_base: usize,
    /// Number of results the caller expects (-1 = variable).
    num_results: i32,
    /// Vararg values for this frame.
    varargs: Vec<Value>,
}

// ── VM state ───────────────────────────────────────────────────────

/// The Lua virtual machine.
pub struct Vm {
    /// Garbage collector / object allocator.
    pub gc: Gc,
    /// Shared register stack.
    stack: Vec<Value>,
    /// Call stack.
    frames: Vec<CallFrame>,
    /// Open upvalues (sorted by stack index, ascending).
    open_upvalues: Vec<UpvalueRef>,
    /// Stack top for variable-length arg/result lists.
    top: usize,
    /// GcRef to the pcall closure (for special-case detection in CALL).
    pcall_ref: Option<GcRef>,
    /// Stack indices of to-be-closed variables (sorted ascending).
    tbc_slots: Vec<usize>,
}

impl Vm {
    /// Create a new VM.
    pub fn new() -> Self {
        Vm {
            gc: Gc::new(),
            stack: vec![Value::Nil; 256],
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            top: 0,
            pcall_ref: None,
            tbc_slots: Vec::new(),
        }
    }

    /// Load and execute a compiled top-level chunk.
    pub fn execute_main(&mut self, proto: Proto) -> Result<(), LuaError> {
        // Create the global environment table (_ENV)
        let env_table = self.create_global_env();

        // Create the main closure from the proto
        let proto_rc = Rc::new(proto);
        let env_upvalue = Rc::new(RefCell::new(Upvalue::Closed(Value::Object(env_table))));
        let main_closure =
            Closure::new_lua(Rc::clone(&proto_rc), vec![Rc::clone(&env_upvalue)]);
        let main_ref = self.gc.new_closure(main_closure);

        // Push the main call frame
        let base = 0;
        self.ensure_stack(base + proto_rc.max_stack_size as usize);
        self.frames.push(CallFrame {
            closure: main_ref,
            proto: proto_rc,
            upvalues: vec![env_upvalue],
            base,
            pc: 0,
            result_base: 0,
            num_results: 0,
            varargs: Vec::new(),
        });

        self.execute()
    }

    /// Create the global environment with standard library functions.
    fn create_global_env(&mut self) -> GcRef {
        let mut env = Table::new();

        // Pre-intern all metamethod name strings so find_string() always finds them.
        for name in &[
            MM_ADD, MM_SUB, MM_MUL, MM_DIV, MM_MOD, MM_POW, MM_UNM, MM_IDIV,
            MM_BAND, MM_BOR, MM_BXOR, MM_BNOT, MM_SHL, MM_SHR,
            MM_CONCAT, MM_LEN, MM_EQ, MM_LT, MM_LE,
            MM_INDEX, MM_NEWINDEX, MM_CALL, MM_TOSTRING, MM_METATABLE,
            MM_CLOSE,
        ] {
            self.gc.new_string(name);
        }

        // Register built-in functions
        self.register_native(&mut env, "print", crate::stdlib::lua_print);
        self.register_native(&mut env, "type", crate::stdlib::lua_type);
        self.register_native(&mut env, "tostring", crate::stdlib::lua_tostring);
        self.register_native(&mut env, "tonumber", crate::stdlib::lua_tonumber);
        self.register_native(&mut env, "assert", crate::stdlib::lua_assert);
        self.register_native(&mut env, "error", crate::stdlib::lua_error);

        // pcall is special: handled by the VM directly, not as a regular native call.
        {
            let pcall_closure = Closure::new_native("pcall", |_, _| Ok(vec![]));
            let pcall_gc = self.gc.new_closure(pcall_closure);
            self.pcall_ref = Some(pcall_gc);
            let key = self.gc.new_string(b"pcall");
            env.raw_set(Value::Object(key), Value::Object(pcall_gc));
        }

        self.register_native(&mut env, "ipairs", crate::stdlib::lua_ipairs);
        self.register_native(&mut env, "pairs", crate::stdlib::lua_pairs);
        self.register_native(&mut env, "rawget", crate::stdlib::lua_rawget);
        self.register_native(&mut env, "rawset", crate::stdlib::lua_rawset);
        self.register_native(&mut env, "rawlen", crate::stdlib::lua_rawlen);
        self.register_native(&mut env, "rawequal", crate::stdlib::lua_rawequal);
        self.register_native(&mut env, "select", crate::stdlib::lua_select);
        self.register_native(&mut env, "setmetatable", crate::stdlib::lua_setmetatable);
        self.register_native(&mut env, "getmetatable", crate::stdlib::lua_getmetatable);

        // _VERSION
        let version_str = self.gc.new_string(b"Lua 5.5");
        let version_key = self.gc.new_string(b"_VERSION");
        env.raw_set(Value::Object(version_key), Value::Object(version_str));

        self.gc.new_table(env)
    }

    /// Register a native function in a table.
    fn register_native(&mut self, table: &mut Table, name: &'static str, func: NativeFn) {
        let closure = Closure::new_native(name, func);
        let gc_ref = self.gc.new_closure(closure);
        let key = self.gc.new_string(name.as_bytes());
        table.raw_set(Value::Object(key), Value::Object(gc_ref));
    }

    /// Ensure the stack has at least `size` slots.
    fn ensure_stack(&mut self, size: usize) {
        if self.stack.len() <= size {
            self.stack.resize(size + 64, Value::Nil);
        }
    }

    // ── Register accessors ─────────────────────────────────────────

    #[inline]
    fn reg(&self, base: usize, idx: usize) -> Value {
        self.stack[base + idx]
    }

    #[inline]
    fn set_reg(&mut self, base: usize, idx: usize, val: Value) {
        self.stack[base + idx] = val;
    }

    // ── Upvalue helpers ────────────────────────────────────────────

    fn get_upvalue_val(&self, upvalues: &[UpvalueRef], idx: usize) -> Value {
        match *upvalues[idx].borrow() {
            Upvalue::Open(stack_idx) => self.stack[stack_idx],
            Upvalue::Closed(val) => val,
        }
    }

    /// Find an existing open upvalue for the given stack index, or create one.
    fn find_or_create_upvalue(&mut self, stack_idx: usize) -> UpvalueRef {
        for uv in &self.open_upvalues {
            if let Upvalue::Open(idx) = *uv.borrow() {
                if idx == stack_idx {
                    return Rc::clone(uv);
                }
            }
        }
        let uv = Rc::new(RefCell::new(Upvalue::Open(stack_idx)));
        self.open_upvalues.push(Rc::clone(&uv));
        uv
    }

    /// Close all open upvalues with stack index >= `from`.
    fn close_upvalues(&mut self, from: usize) {
        for uv in &self.open_upvalues {
            let should_close = match *uv.borrow() {
                Upvalue::Open(idx) => idx >= from,
                Upvalue::Closed(_) => false,
            };
            if should_close {
                let val = match *uv.borrow() {
                    Upvalue::Open(idx) => self.stack[idx],
                    _ => unreachable!(),
                };
                *uv.borrow_mut() = Upvalue::Closed(val);
            }
        }
        self.open_upvalues
            .retain(|uv| matches!(*uv.borrow(), Upvalue::Open(_)));
    }

    /// Close all to-be-closed variables with stack index >= `from`.
    /// Calls `__close` metamethod in reverse order. `err_obj` is the error
    /// object (if any) that caused the scope exit.
    fn close_tbc_vars(&mut self, from: usize, err_obj: Option<Value>) -> Result<(), LuaError> {
        // Collect TBC slots at or above `from` in reverse order
        let mut to_close: Vec<usize> = Vec::new();
        while let Some(&slot) = self.tbc_slots.last() {
            if slot >= from {
                self.tbc_slots.pop();
                to_close.push(slot);
            } else {
                break;
            }
        }

        let mut first_err: Option<LuaError> = None;
        for slot in to_close {
            let val = self.stack[slot];
            // nil and false are silently ignored
            if val == Value::Nil || val == Value::Boolean(false) {
                continue;
            }
            if let Some(mm) = self.get_metamethod(val, MM_CLOSE) {
                let err_arg = err_obj.unwrap_or(Value::Nil);
                let result = self.call_value(mm, &[val, err_arg]);
                if let Err(e) = result {
                    if first_err.is_none() {
                        first_err = Some(e);
                    }
                }
            }
            // Per spec: value must have __close or be nil/false
            // If it has neither, that's an error (already validated at TBC time)
        }

        if let Some(e) = first_err {
            return Err(e);
        }
        Ok(())
    }

    // ── Metamethod infrastructure ─────────────────────────────────

    /// Get the metatable of a value (if any).
    fn get_metatable(&self, val: Value) -> Option<GcRef> {
        match val {
            Value::Object(r) => match &r.as_object().kind {
                GcObjectKind::Table(t) => t.metatable,
                // Strings share a per-type metatable (set by string library, not yet)
                _ => None,
            },
            _ => None,
        }
    }

    /// Look up a metamethod by name in a value's metatable.
    /// Returns the metamethod value, or None if not found.
    fn get_metamethod(&self, val: Value, name: &[u8]) -> Option<Value> {
        let mt_ref = self.get_metatable(val)?;
        let mt = mt_ref.as_object().as_table()?;
        let key_ref = self.gc.find_string(name)?;
        let result = mt.raw_get(&Value::Object(key_ref));
        if result.is_nil() { None } else { Some(result) }
    }

    /// Look up a metamethod from the first operand, then the second.
    fn get_binop_metamethod(&self, a: Value, b: Value, name: &[u8]) -> Option<Value> {
        self.get_metamethod(a, name).or_else(|| self.get_metamethod(b, name))
    }

    /// Call a metamethod with the given arguments and return one result.
    fn call_metamethod(&mut self, mm: Value, args: &[Value]) -> Result<Value, LuaError> {
        let results = self.call_value(mm, args)?;
        Ok(results.into_iter().next().unwrap_or(Value::Nil))
    }

    /// Call a metamethod and return all results.
    fn call_metamethod_multi(&mut self, mm: Value, args: &[Value]) -> Result<Vec<Value>, LuaError> {
        self.call_value(mm, args)
    }

    /// Call a value (function or callable via __call) with args and return results.
    /// This handles synchronous native calls and pushes frames for Lua calls.
    fn call_value(&mut self, func: Value, args: &[Value]) -> Result<Vec<Value>, LuaError> {
        // Find a place on the stack for this call
        let call_base = self.find_call_base();
        self.ensure_stack(call_base + args.len() + 2);
        self.stack[call_base] = func;
        for (i, &arg) in args.iter().enumerate() {
            self.stack[call_base + 1 + i] = arg;
        }

        let saved_depth = self.frames.len();

        // Handle __call chain
        let mut actual_func = func;
        let mut call_limit = 16;
        loop {
            match actual_func {
                Value::Object(r) if r.as_object().as_closure().is_some() => break,
                _ => {
                    call_limit -= 1;
                    if call_limit == 0 {
                        return Err(LuaError::new("'__call' chain too long"));
                    }
                    match self.get_metamethod(actual_func, MM_CALL) {
                        Some(mm) => {
                            // Shift args: prepend the original value
                            let mut new_args = Vec::with_capacity(args.len() + 1);
                            new_args.push(actual_func);
                            new_args.extend_from_slice(args);
                            // Recurse with the metamethod as the function
                            return self.call_value(mm, &new_args);
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to call a {} value",
                                actual_func.type_name()
                            )));
                        }
                    }
                }
            }
        }

        let gc_ref = actual_func.as_gc_ref().unwrap();
        let is_native = matches!(gc_ref.as_object().as_closure().unwrap(), Closure::Native(_));

        if is_native {
            let func_ptr = match gc_ref.as_object().as_closure().unwrap() {
                Closure::Native(nc) => nc.func,
                _ => unreachable!(),
            };
            return func_ptr(args, &mut self.gc);
        }

        // Lua function: push frame and execute
        let result_base = call_base;
        self.do_call(actual_func, call_base, args, result_base, -1)?;

        if self.frames.len() > saved_depth {
            self.execute_to_depth(saved_depth)?;
        }

        // Collect results from result_base..self.top
        let results: Vec<Value> = self.stack[result_base..self.top].to_vec();
        Ok(results)
    }

    /// Find a safe call_base for internal metamethod calls (above all active frames).
    fn find_call_base(&self) -> usize {
        let stack_top = if self.frames.is_empty() {
            self.top
        } else {
            let last = &self.frames[self.frames.len() - 1];
            let frame_top = last.base + last.proto.max_stack_size as usize;
            self.top.max(frame_top)
        };
        stack_top + 2
    }

    // ── String-to-number coercion (M2.2) ───────────────────────────

    /// Try to coerce a value to a number for arithmetic.
    /// Strings are converted to numbers if they represent valid numerals.
    fn coerce_to_number(v: Value) -> Option<Value> {
        match v {
            Value::Integer(_) | Value::Float(_) => Some(v),
            Value::Object(r) => {
                let s = r.as_object().as_string()?;
                let text = std::str::from_utf8(s.as_bytes()).ok()?;
                let text = text.trim();
                // Try integer first
                if let Ok(n) = text.parse::<i64>() {
                    return Some(Value::Integer(n));
                }
                // Try hex integer
                if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    if let Ok(n) = i64::from_str_radix(hex, 16) {
                        return Some(Value::Integer(n));
                    }
                }
                // Try float
                if let Ok(n) = text.parse::<f64>() {
                    return Some(Value::Float(n));
                }
                None
            }
            _ => None,
        }
    }

    /// Try to coerce a value to an integer for bitwise ops.
    /// Strings are converted, floats with exact integer representation are converted.
    fn coerce_to_integer(v: Value) -> Option<i64> {
        match v {
            Value::Integer(n) => Some(n),
            Value::Float(f) => {
                let i = f as i64;
                if i as f64 == f { Some(i) } else { None }
            }
            Value::Object(r) => {
                let s = r.as_object().as_string()?;
                let text = std::str::from_utf8(s.as_bytes()).ok()?;
                let text = text.trim();
                if let Ok(n) = text.parse::<i64>() {
                    return Some(n);
                }
                if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    if let Ok(n) = i64::from_str_radix(hex, 16) {
                        return Some(n);
                    }
                }
                // Try float string → integer
                if let Ok(f) = text.parse::<f64>() {
                    let i = f as i64;
                    if i as f64 == f {
                        return Some(i);
                    }
                }
                None
            }
            _ => None,
        }
    }

    // ── Arithmetic helpers ─────────────────────────────────────────

    /// Try raw arithmetic (including string-to-number coercion). Returns None if
    /// operands are not numeric/coercible (metamethod needed).
    fn try_arith_add(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_add(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x + y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 + y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x + y as f64)),
            _ => {
                // Try string-to-number coercion
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_add(a2, b2)
            }
        }
    }

    fn try_arith_sub(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_sub(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x - y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 - y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x - y as f64)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_sub(a2, b2)
            }
        }
    }

    fn try_arith_mul(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_mul(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x * y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 * y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x * y as f64)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_mul(a2, b2)
            }
        }
    }

    fn try_arith_div(a: Value, b: Value) -> Option<Value> {
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_div(a2, b2);
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_div(Value::Float(x), b2);
            }
        };
        Some(Value::Float(x / y))
    }

    fn try_arith_idiv(a: Value, b: Value) -> Result<Option<Value>, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n//0'"));
                }
                Ok(Some(Value::Integer(lua_idiv(x, y))))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Some(Value::Float((x / y).floor()))),
            (Value::Integer(x), Value::Float(y)) => Ok(Some(Value::Float((x as f64 / y).floor()))),
            (Value::Float(x), Value::Integer(y)) => Ok(Some(Value::Float((x / y as f64).floor()))),
            _ => {
                let a2 = Self::coerce_to_number(a);
                let b2 = Self::coerce_to_number(b);
                if let (Some(a2), Some(b2)) = (a2, b2) {
                    Self::try_arith_idiv(a2, b2)
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn try_arith_mod(a: Value, b: Value) -> Result<Option<Value>, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n%0'"));
                }
                Ok(Some(Value::Integer(lua_imod(x, y))))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Some(Value::Float(lua_fmod(x, y)))),
            (Value::Integer(x), Value::Float(y)) => Ok(Some(Value::Float(lua_fmod(x as f64, y)))),
            (Value::Float(x), Value::Integer(y)) => Ok(Some(Value::Float(lua_fmod(x, y as f64)))),
            _ => {
                let a2 = Self::coerce_to_number(a);
                let b2 = Self::coerce_to_number(b);
                if let (Some(a2), Some(b2)) = (a2, b2) {
                    Self::try_arith_mod(a2, b2)
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn try_arith_pow(a: Value, b: Value) -> Option<Value> {
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_pow(a2, b2);
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_pow(Value::Float(x), b2);
            }
        };
        Some(Value::Float(x.powf(y)))
    }

    fn try_arith_unm(a: Value) -> Option<Value> {
        match a {
            Value::Integer(x) => Some(Value::Integer(x.wrapping_neg())),
            Value::Float(x) => Some(Value::Float(-x)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                Self::try_arith_unm(a2)
            }
        }
    }

    /// Perform a binary arithmetic operation: try raw, then metamethod.
    fn arith_binop(
        &mut self,
        a: Value,
        b: Value,
        try_fn: fn(Value, Value) -> Option<Value>,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        if let Some(result) = try_fn(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
            return self.call_metamethod(mm, &[a, b]);
        }
        Err(LuaError::new(format!(
            "attempt to perform arithmetic on a {} value",
            if !a.is_number() && Self::coerce_to_number(a).is_none() { a.type_name() } else { b.type_name() }
        )))
    }

    /// Perform a binary arithmetic op that can error (idiv, mod) - try raw, then metamethod.
    fn arith_binop_err(
        &mut self,
        a: Value,
        b: Value,
        try_fn: fn(Value, Value) -> Result<Option<Value>, LuaError>,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        match try_fn(a, b)? {
            Some(result) => Ok(result),
            None => {
                if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
                    return self.call_metamethod(mm, &[a, b]);
                }
                Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    if !a.is_number() && Self::coerce_to_number(a).is_none() { a.type_name() } else { b.type_name() }
                )))
            }
        }
    }

    /// Perform unary minus: try raw, then metamethod.
    fn arith_unm(&mut self, a: Value) -> Result<Value, LuaError> {
        if let Some(result) = Self::try_arith_unm(a) {
            return Ok(result);
        }
        if let Some(mm) = self.get_metamethod(a, MM_UNM) {
            // Unary ops get a dummy second argument equal to the first
            return self.call_metamethod(mm, &[a, a]);
        }
        Err(LuaError::new(format!(
            "attempt to perform arithmetic on a {} value",
            a.type_name()
        )))
    }

    // ── Bitwise helpers ────────────────────────────────────────────

    /// Perform binary bitwise op with coercion + metamethod fallback.
    fn bitwise_binop(
        &mut self,
        a: Value,
        b: Value,
        raw_fn: fn(i64, i64) -> i64,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        if let (Some(x), Some(y)) = (Self::coerce_to_integer(a), Self::coerce_to_integer(b)) {
            return Ok(Value::Integer(raw_fn(x, y)));
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
            return self.call_metamethod(mm, &[a, b]);
        }
        Err(LuaError::new(format!(
            "attempt to perform bitwise operation on a {} value",
            if Self::coerce_to_integer(a).is_none() { a.type_name() } else { b.type_name() }
        )))
    }

    /// Perform unary bitwise NOT with coercion + metamethod fallback.
    fn bitwise_bnot(&mut self, a: Value) -> Result<Value, LuaError> {
        if let Some(x) = Self::coerce_to_integer(a) {
            return Ok(Value::Integer(!x));
        }
        if let Some(mm) = self.get_metamethod(a, MM_BNOT) {
            return self.call_metamethod(mm, &[a, a]);
        }
        Err(LuaError::new(format!(
            "attempt to perform bitwise operation on a {} value",
            a.type_name()
        )))
    }

    // ── Comparison helpers ─────────────────────────────────────────

    /// Raw equality (no metamethods). Used by PartialEq on Value.
    fn compare_eq_raw(a: Value, b: Value) -> bool {
        a == b
    }

    /// Equality with __eq metamethod support.
    fn compare_eq(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        // Primitive equality first
        if a == b {
            return Ok(true);
        }
        // __eq is only tried when both are tables or both are full userdata
        // and they are not primitively equal
        let both_tables = a.is_table() && b.is_table();
        if !both_tables {
            return Ok(false);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_EQ) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Ok(false)
    }

    fn try_compare_lt(a: Value, b: Value) -> Option<bool> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(x < y),
            (Value::Float(x), Value::Float(y)) => Some(x < y),
            (Value::Integer(x), Value::Float(y)) => Some((x as f64) < y),
            (Value::Float(x), Value::Integer(y)) => Some(x < (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Some(sa.as_bytes() < sb.as_bytes())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn compare_lt(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        if let Some(result) = Self::try_compare_lt(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_LT) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Err(LuaError::new(format!(
            "attempt to compare {} with {}",
            a.type_name(),
            b.type_name()
        )))
    }

    fn try_compare_le(a: Value, b: Value) -> Option<bool> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(x <= y),
            (Value::Float(x), Value::Float(y)) => Some(x <= y),
            (Value::Integer(x), Value::Float(y)) => Some((x as f64) <= y),
            (Value::Float(x), Value::Integer(y)) => Some(x <= (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Some(sa.as_bytes() <= sb.as_bytes())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn compare_le(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        if let Some(result) = Self::try_compare_le(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_LE) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Err(LuaError::new(format!(
            "attempt to compare {} with {}",
            a.type_name(),
            b.type_name()
        )))
    }

    // ── String concatenation helper ────────────────────────────────

    /// Check if a value can be raw-concatenated (string or number).
    fn is_concat_raw(v: Value) -> bool {
        v.is_string() || v.is_number()
    }

    fn concat_values(&mut self, base: usize, from: usize, to: usize) -> Result<Value, LuaError> {
        // Check if all values can be raw-concatenated
        let all_raw = (from..=to).all(|i| Self::is_concat_raw(self.stack[base + i]));

        if all_raw {
            let mut result = Vec::new();
            for i in from..=to {
                let v = self.stack[base + i];
                match v {
                    Value::Object(r) if r.as_object().as_string().is_some() => {
                        result.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
                    }
                    Value::Integer(n) => {
                        result.extend_from_slice(format!("{n}").as_bytes());
                    }
                    Value::Float(n) => {
                        result.extend_from_slice(format!("{n}").as_bytes());
                    }
                    _ => unreachable!(),
                }
            }
            return Ok(Value::Object(self.gc.new_string(&result)));
        }

        // Need metamethods: fold right-to-left
        let mut acc = self.stack[base + to];
        for i in (from..to).rev() {
            let lhs = self.stack[base + i];
            if Self::is_concat_raw(lhs) && Self::is_concat_raw(acc) {
                // Raw concat of these two
                let mut buf = Vec::new();
                Self::append_to_buf(lhs, &mut buf);
                Self::append_to_buf(acc, &mut buf);
                acc = Value::Object(self.gc.new_string(&buf));
            } else {
                // Try __concat metamethod
                if let Some(mm) = self.get_binop_metamethod(lhs, acc, MM_CONCAT) {
                    acc = self.call_metamethod(mm, &[lhs, acc])?;
                } else {
                    return Err(LuaError::new(format!(
                        "attempt to concatenate a {} value",
                        if !Self::is_concat_raw(lhs) { lhs.type_name() } else { acc.type_name() }
                    )));
                }
            }
        }
        Ok(acc)
    }

    fn append_to_buf(v: Value, buf: &mut Vec<u8>) {
        match v {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                buf.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
            }
            Value::Integer(n) => buf.extend_from_slice(format!("{n}").as_bytes()),
            Value::Float(n) => buf.extend_from_slice(format!("{n}").as_bytes()),
            _ => {}
        }
    }

    // ── Table access helpers ───────────────────────────────────────

    /// Table get with __index metamethod support.
    fn table_get(&mut self, table_val: Value, key: Value) -> Result<Value, LuaError> {
        let mut current = table_val;
        let mut limit = 16;
        loop {
            limit -= 1;
            if limit == 0 {
                return Err(LuaError::new("'__index' chain too long; possible loop"));
            }

            match current {
                Value::Object(r) if r.as_object().as_table().is_some() => {
                    let raw = r.as_object().as_table().unwrap().raw_get(&key);
                    if !raw.is_nil() {
                        return Ok(raw);
                    }
                    // Key not found: check __index metamethod
                    match self.get_metamethod(current, MM_INDEX) {
                        Some(mm) if mm.is_function() => {
                            return self.call_metamethod(mm, &[current, key]);
                        }
                        Some(mm) => {
                            // __index is a table (or other value with __index)
                            current = mm;
                            continue;
                        }
                        None => return Ok(Value::Nil),
                    }
                }
                _ => {
                    // Not a table: check __index metamethod
                    match self.get_metamethod(current, MM_INDEX) {
                        Some(mm) if mm.is_function() => {
                            return self.call_metamethod(mm, &[current, key]);
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to index a {} value",
                                table_val.type_name()
                            )));
                        }
                    }
                }
            }
        }
    }

    /// Table set with __newindex metamethod support.
    fn table_set(&mut self, table_val: Value, key: Value, val: Value) -> Result<(), LuaError> {
        let mut current = table_val;
        let mut limit = 16;
        loop {
            limit -= 1;
            if limit == 0 {
                return Err(LuaError::new("'__newindex' chain too long; possible loop"));
            }

            match current {
                Value::Object(r) if r.as_object().as_table().is_some() => {
                    // Check if key already exists (raw)
                    let exists = !r.as_object().as_table().unwrap().raw_get(&key).is_nil();
                    if exists {
                        // Existing key: always do raw set (no __newindex)
                        r.as_object().as_table().unwrap(); // validate
                        // Need mutable access
                        let mut r2 = r;
                        r2.as_object_mut().as_table_mut().unwrap().raw_set(key, val);
                        return Ok(());
                    }
                    // New key: check __newindex
                    match self.get_metamethod(current, MM_NEWINDEX) {
                        Some(mm) if mm.is_function() => {
                            self.call_metamethod(mm, &[current, key, val])?;
                            return Ok(());
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            // No metamethod: raw set
                            let mut r2 = r;
                            r2.as_object_mut().as_table_mut().unwrap().raw_set(key, val);
                            return Ok(());
                        }
                    }
                }
                _ => {
                    match self.get_metamethod(current, MM_NEWINDEX) {
                        Some(mm) if mm.is_function() => {
                            self.call_metamethod(mm, &[current, key, val])?;
                            return Ok(());
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to index a {} value",
                                table_val.type_name()
                            )));
                        }
                    }
                }
            }
        }
    }

    // ── Garbage collection ───────────────────────────────────────────

    /// Gather all GC roots from the VM state and run a collection cycle.
    fn collect_garbage(&mut self) {
        let mut roots = Vec::new();

        // Root: stack values (only up to the active region)
        let stack_limit = if self.frames.is_empty() {
            self.top
        } else {
            let last = &self.frames[self.frames.len() - 1];
            let frame_top = last.base + last.proto.max_stack_size as usize;
            self.top.max(frame_top)
        };
        for val in &self.stack[..stack_limit.min(self.stack.len())] {
            if let Value::Object(r) = val {
                roots.push(*r);
            }
        }

        // Root: each call frame's closure, varargs, and closed upvalues
        for frame in &self.frames {
            roots.push(frame.closure);
            for val in &frame.varargs {
                if let Value::Object(r) = val {
                    roots.push(*r);
                }
            }
            for uv in &frame.upvalues {
                if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                    roots.push(*r);
                }
            }
        }

        // Root: closed values in open_upvalues (open ones point into stack, already covered)
        for uv in &self.open_upvalues {
            if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                roots.push(*r);
            }
        }

        self.gc.collect(&roots);
    }

    /// Check if GC should run and trigger it if so.
    #[inline]
    fn maybe_collect(&mut self) {
        if self.gc.should_collect() {
            self.collect_garbage();
        }
    }

    // ── Main dispatch loop ─────────────────────────────────────────

    fn execute(&mut self) -> Result<(), LuaError> {
        self.execute_to_depth(0)
    }

    fn  execute_to_depth(&mut self, min_depth: usize) -> Result<(), LuaError> {
        loop {
            if self.frames.len() <= min_depth {
                return Ok(());
            }

            let fi = self.frames.len() - 1;
            let pc = self.frames[fi].pc;
            let base = self.frames[fi].base;
            let inst = self.frames[fi].proto.code[pc];
            self.frames[fi].pc += 1;

            let op = OpCode::from_u8(decode_op(inst))
                .ok_or_else(|| LuaError::new(format!("invalid opcode: {}", decode_op(inst))))?;
            let a = decode_a(inst) as usize;
            let b = decode_b(inst) as usize;
            let c = decode_c(inst) as usize;
            let bx = decode_bx(inst) as usize;
            let sbx = decode_sbx(inst);

            match op {
                // ── Loading ────────────────────────────────────────
                OpCode::Move => {
                    let val = self.reg(base, b);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadI => {
                    self.set_reg(base, a, Value::Integer(sbx as i64));
                }

                OpCode::LoadK => {
                    let val = self.frames[fi].proto.constants[bx].to_value(&mut self.gc);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadKX => {
                    let next_inst = self.frames[fi].proto.code[self.frames[fi].pc];
                    self.frames[fi].pc += 1;
                    let ax = decode_ax(next_inst) as usize;
                    let val = self.frames[fi].proto.constants[ax].to_value(&mut self.gc);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadBool => {
                    self.set_reg(base, a, Value::Boolean(b != 0));
                    if c != 0 {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::LoadNil => {
                    for i in a..=a + b {
                        self.set_reg(base, i, Value::Nil);
                    }
                }

                // ── Upvalues ───────────────────────────────────────
                OpCode::GetUpval => {
                    let upvalues = &self.frames[fi].upvalues;
                    let val = self.get_upvalue_val(upvalues, b);
                    self.set_reg(base, a, val);
                }

                OpCode::SetUpval => {
                    let val = self.reg(base, a);
                    let uv = Rc::clone(&self.frames[fi].upvalues[b]);
                    match &mut *uv.borrow_mut() {
                        Upvalue::Open(stack_idx) => self.stack[*stack_idx] = val,
                        Upvalue::Closed(v) => *v = val,
                    }
                }

                OpCode::GetTabUp => {
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, b);
                    let key = self.frames[fi].proto.constants[c].to_value(&mut self.gc);
                    let result = self.table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTabUp => {
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, a);
                    let key = self.frames[fi].proto.constants[b].to_value(&mut self.gc);
                    let val = self.reg(base, c);
                    self.table_set(table_val, key, val)?;
                }

                // ── Tables ─────────────────────────────────────────
                OpCode::NewTable => {
                    self.maybe_collect();
                    let table = Table::with_capacity(b, c);
                    let gc_ref = self.gc.new_table(table);
                    self.set_reg(base, a, Value::Object(gc_ref));
                }

                OpCode::GetTable => {
                    let table_val = self.reg(base, b);
                    let key = self.reg(base, c);
                    let result = self.table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTable => {
                    let table_val = self.reg(base, a);
                    let key = self.reg(base, b);
                    let val = self.reg(base, c);
                    self.table_set(table_val, key, val)?;
                }

                OpCode::SetList => {
                    let table_val = self.reg(base, a);
                    let num = if b > 0 {
                        b
                    } else {
                        self.top - (base + a) - 1
                    };
                    let offset = (c as u32 - 1) * FIELDS_PER_FLUSH;

                    if let Value::Object(mut r) = table_val {
                        let obj = r.as_object_mut();
                        if let GcObjectKind::Table(t) = &mut obj.kind {
                            for i in 1..=num {
                                let val = self.stack[base + a + i];
                                let key = offset as i64 + i as i64;
                                while t.array.len() < key as usize {
                                    t.array.push(Value::Nil);
                                }
                                t.array[key as usize - 1] = val;
                            }
                        }
                    }
                }

                // ── Arithmetic ─────────────────────────────────────
                OpCode::Add => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_add, MM_ADD)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Sub => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_sub, MM_SUB)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Mul => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_mul, MM_MUL)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Div => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_div, MM_DIV)?;
                    self.set_reg(base, a, result);
                }

                OpCode::IDiv => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop_err(rb, rc, Self::try_arith_idiv, MM_IDIV)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Mod => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop_err(rb, rc, Self::try_arith_mod, MM_MOD)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Pow => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_pow, MM_POW)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Unm => {
                    let rb = self.reg(base, b);
                    let result = self.arith_unm(rb)?;
                    self.set_reg(base, a, result);
                }

                // ── Bitwise ────────────────────────────────────────
                OpCode::BAnd => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x & y, MM_BAND)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BOr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x | y, MM_BOR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BXor => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x ^ y, MM_BXOR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shl => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shl, MM_SHL)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shr, MM_SHR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BNot => {
                    let rb = self.reg(base, b);
                    let result = self.bitwise_bnot(rb)?;
                    self.set_reg(base, a, result);
                }

                // ── Logic ──────────────────────────────────────────
                OpCode::Not => {
                    let rb = self.reg(base, b);
                    self.set_reg(base, a, Value::Boolean(!rb.is_truthy()));
                }

                // ── String / Length ────────────────────────────────
                OpCode::Concat => {
                    self.maybe_collect();
                    let result = self.concat_values(base, b, c)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Len => {
                    let rb = self.reg(base, b);
                    // __len is checked first for tables (if metatable exists), always for other types
                    let has_mm = match rb {
                        Value::Object(r) if r.as_object().as_string().is_some() => false,
                        _ => self.get_metamethod(rb, MM_LEN).is_some(),
                    };
                    if has_mm {
                        let mm = self.get_metamethod(rb, MM_LEN).unwrap();
                        let result = self.call_metamethod(mm, &[rb, rb])?;
                        self.set_reg(base, a, result);
                    } else {
                        match rb {
                            Value::Object(r) => match &r.as_object().kind {
                                GcObjectKind::String(s) => {
                                    self.set_reg(base, a, Value::Integer(s.len() as i64));
                                }
                                GcObjectKind::Table(t) => {
                                    self.set_reg(base, a, Value::Integer(t.length() as i64));
                                }
                                _ => {
                                    return Err(LuaError::new(format!(
                                        "attempt to get length of a {} value",
                                        rb.type_name()
                                    )))
                                }
                            },
                            _ => {
                                return Err(LuaError::new(format!(
                                    "attempt to get length of a {} value",
                                    rb.type_name()
                                )))
                            }
                        }
                    }
                }

                // ── Comparison & Conditional ───────────────────────
                OpCode::Eq => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_eq(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Lt => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_lt(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Le => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_le(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Test => {
                    let ra = self.reg(base, a);
                    let cond = !ra.is_truthy();
                    if cond == (c != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::TestSet => {
                    let rb = self.reg(base, b);
                    let cond = !rb.is_truthy();
                    if cond == (c != 0) {
                        self.frames[fi].pc += 1;
                    } else {
                        self.set_reg(base, a, rb);
                    }
                }

                // ── Control flow ───────────────────────────────────
                OpCode::Jmp => {
                    self.frames[fi].pc = (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::ForPrep => {
                    let init = self.reg(base, a);
                    let limit = self.reg(base, a + 1);
                    let step = self.reg(base, a + 2);
                    self.for_prep_validate(init, limit, step)?;
                    // Pre-subtract step so that ForLoop's first increment yields init
                    let adjusted = Self::try_arith_sub(init, step)
                        .ok_or_else(|| LuaError::new("'for' initial value must be a number"))?;
                    self.set_reg(base, a, adjusted);
                    // Jump forward to FORLOOP
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::ForLoop => {
                    let index = self.reg(base, a);
                    let limit = self.reg(base, a + 1);
                    let step = self.reg(base, a + 2);

                    let new_index = Self::try_arith_add(index, step)
                        .ok_or_else(|| LuaError::new("'for' step must be a number"))?;
                    self.set_reg(base, a, new_index);

                    if self.for_loop_check(new_index, limit, step) {
                        self.set_reg(base, a + 3, new_index);
                        self.frames[fi].pc =
                            (self.frames[fi].pc as i64 + sbx as i64) as usize;
                    }
                }

                OpCode::TForPrep => {
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::TForLoop => {
                    // Call R[A](R[A+1], R[A+2]) -> results into R[A+4], ...
                    // B = number of loop variables
                    let iter = self.reg(base, a);
                    let state = self.reg(base, a + 1);
                    let control = self.reg(base, a + 2);

                    // Set up call: place function and args in temp registers
                    let call_base = base + a + 4;
                    self.ensure_stack(call_base + 3);
                    self.stack[call_base] = iter;
                    self.stack[call_base + 1] = state;
                    self.stack[call_base + 2] = control;

                    // Call with 2 args, B results
                    self.call_function(call_base, 3, b as i32)?;

                    // Check first result
                    let first_result = self.reg(base, a + 4);
                    if !first_result.is_nil() {
                        self.set_reg(base, a + 2, first_result);
                        // Jump back
                        let jump_offset = -(c as i64);
                        self.frames[fi].pc =
                            (self.frames[fi].pc as i64 + jump_offset) as usize;
                    }
                }

                // ── Functions ──────────────────────────────────────
                OpCode::Closure => {
                    self.maybe_collect();
                    let child_proto = {
                        let parent_proto = &self.frames[fi].proto;
                        Rc::new(parent_proto.protos[bx].clone())
                    };

                    let mut new_upvalues = Vec::new();
                    for uv_desc in &child_proto.upvalues {
                        if uv_desc.in_stack {
                            let stack_idx = base + uv_desc.index as usize;
                            new_upvalues.push(self.find_or_create_upvalue(stack_idx));
                        } else {
                            let parent_uv =
                                Rc::clone(&self.frames[fi].upvalues[uv_desc.index as usize]);
                            new_upvalues.push(parent_uv);
                        }
                    }

                    let closure = Closure::new_lua(child_proto, new_upvalues);
                    let gc_ref = self.gc.new_closure(closure);
                    self.set_reg(base, a, Value::Object(gc_ref));
                }

                OpCode::Call => {
                    let func_val = self.stack[base + a];
                    let is_pcall = if let (Value::Object(r), Some(pcall_r)) = (func_val, self.pcall_ref) {
                        r == pcall_r
                    } else {
                        false
                    };

                    if is_pcall {
                        let num_results = if c == 0 { -1 } else { c as i32 - 1 };
                        let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                        let args: Vec<Value> = (0..num_args)
                            .map(|i| self.stack[base + a + 1 + i])
                            .collect();
                        self.handle_pcall(&args, base + a, num_results)?;
                    } else {
                        self.call_function(base + a, b, if c == 0 { -1 } else { c as i32 - 1 })?;
                    }
                }

                OpCode::TailCall => {
                    let func_val = self.reg(base, a);
                    let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                    let args: Vec<Value> = (0..num_args)
                        .map(|i| self.stack[base + a + 1 + i])
                        .collect();

                    self.close_tbc_vars(base, None)?;
                    self.close_upvalues(base);

                    let result_base = self.frames[fi].result_base;
                    let num_results = self.frames[fi].num_results;
                    self.frames.pop();

                    // Place arguments at the base for the new frame
                    for (i, &arg) in args.iter().enumerate() {
                        self.stack[base + 1 + i] = arg;
                    }
                    self.stack[base] = func_val;

                    self.do_call(func_val, base, &args, result_base, num_results)?;
                }

                OpCode::Return => {
                    let num_ret = if b > 0 {
                        b - 1
                    } else {
                        self.top.saturating_sub(base + a)
                    };
                    let results: Vec<Value> =
                        (0..num_ret).map(|i| self.stack[base + a + i]).collect();

                    self.close_tbc_vars(base, None)?;
                    self.close_upvalues(base);

                    let result_base = self.frames[fi].result_base;
                    let num_results = self.frames[fi].num_results;
                    self.frames.pop();

                    if self.frames.is_empty() {
                        return Ok(());
                    }

                    self.place_results(result_base, num_results, &results);
                }

                OpCode::VarArg => {
                    let varargs = self.frames[fi].varargs.clone();
                    if c == 0 {
                        for (i, &val) in varargs.iter().enumerate() {
                            self.ensure_stack(base + a + i);
                            self.stack[base + a + i] = val;
                        }
                        self.top = base + a + varargs.len();
                    } else {
                        let n = c - 1;
                        for i in 0..n {
                            self.ensure_stack(base + a + i);
                            self.stack[base + a + i] =
                                varargs.get(i).copied().unwrap_or(Value::Nil);
                        }
                    }
                }

                OpCode::VarArgPrep => {
                    // Top-level: no-op. Varargs are set up by the caller.
                }

                // ── Scope & cleanup ────────────────────────────────
                OpCode::Close => {
                    self.close_tbc_vars(base + a, None)?;
                    self.close_upvalues(base + a);
                }

                OpCode::Tbc => {
                    // Validate: value must have __close or be nil/false
                    let val = self.reg(base, a);
                    if val != Value::Nil && val != Value::Boolean(false) {
                        if self.get_metamethod(val, MM_CLOSE).is_none() {
                            return Err(LuaError::new(
                                "variable is not closable (no __close metamethod)",
                            ));
                        }
                    }
                    self.tbc_slots.push(base + a);
                }

                OpCode::ExtraArg => {
                    return Err(LuaError::new("unexpected EXTRAARG instruction"));
                }
            }
        }
    }

    // ── Function call implementation ───────────────────────────────

    /// Call a function at stack[func_idx] with arguments.
    fn call_function(
        &mut self,
        func_idx: usize,
        arg_count: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let func_val = self.stack[func_idx];
        let num_args = if arg_count > 0 {
            arg_count - 1
        } else {
            self.top - func_idx - 1
        };
        let args: Vec<Value> = (0..num_args)
            .map(|i| self.stack[func_idx + 1 + i])
            .collect();

        self.do_call(func_val, func_idx, &args, func_idx, num_results)
    }

    /// Internal call dispatch: handles both Lua and native closures, plus __call.
    fn do_call(
        &mut self,
        func_val: Value,
        call_base: usize,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // Resolve __call chain
        let mut actual_func = func_val;
        let mut actual_args = args.to_vec();
        let mut call_limit = 16;
        loop {
            let gc_ref = match actual_func {
                Value::Object(r) if r.as_object().as_closure().is_some() => r,
                _ => {
                    call_limit -= 1;
                    if call_limit == 0 {
                        return Err(LuaError::new("'__call' chain too long"));
                    }
                    match self.get_metamethod(actual_func, MM_CALL) {
                        Some(mm) => {
                            // Prepend the original value as first arg
                            let mut new_args = Vec::with_capacity(actual_args.len() + 1);
                            new_args.push(actual_func);
                            new_args.extend(actual_args);
                            actual_func = mm;
                            actual_args = new_args;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to call a {} value",
                                func_val.type_name()
                            )));
                        }
                    }
                }
            };

            let is_native = matches!(
                gc_ref.as_object().as_closure().unwrap(),
                Closure::Native(_)
            );

            if is_native {
                let func = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Native(nc) => nc.func,
                    _ => unreachable!(),
                };
                let results = func(&actual_args, &mut self.gc)?;
                self.place_results(result_base, num_results, &results);
            } else {
                let (proto, upvalues) = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Lua(lc) => (Rc::clone(&lc.proto), lc.upvalues.clone()),
                    _ => unreachable!(),
                };

                let new_base = call_base + 1;
                let num_params = proto.num_params as usize;
                let is_vararg = proto.is_vararg;
                let max_stack = proto.max_stack_size as usize;

                self.ensure_stack(new_base + max_stack);

                // Place arguments into registers
                for i in 0..num_params.min(actual_args.len()) {
                    self.stack[new_base + i] = actual_args[i];
                }
                for i in actual_args.len()..num_params {
                    self.stack[new_base + i] = Value::Nil;
                }

                let varargs = if is_vararg && actual_args.len() > num_params {
                    actual_args[num_params..].to_vec()
                } else {
                    Vec::new()
                };

                self.frames.push(CallFrame {
                    closure: gc_ref,
                    proto,
                    upvalues,
                    base: new_base,
                    pc: 0,
                    result_base,
                    num_results,
                    varargs,
                });
            }

            return Ok(());
        }
    }

    /// Place call results at the given position.
    fn place_results(&mut self, result_base: usize, num_results: i32, results: &[Value]) {
        if num_results < 0 {
            for (i, &val) in results.iter().enumerate() {
                self.ensure_stack(result_base + i);
                self.stack[result_base + i] = val;
            }
            self.top = result_base + results.len();
        } else {
            let nr = num_results as usize;
            for i in 0..nr {
                self.ensure_stack(result_base + i);
                self.stack[result_base + i] = results.get(i).copied().unwrap_or(Value::Nil);
            }
        }
    }

    // ── Protected call (pcall) ─────────────────────────────────────

    /// Handle pcall(f, ...).
    ///
    /// pcall_args: the arguments passed to pcall itself (f, arg1, arg2, ...)
    /// result_base: where to place (true, results...) or (false, err)
    /// num_results: how many results the caller expects
    fn handle_pcall(
        &mut self,
        pcall_args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let func = pcall_args.first().copied().unwrap_or(Value::Nil);
        let call_args: Vec<Value> = if pcall_args.len() > 1 {
            pcall_args[1..].to_vec()
        } else {
            vec![]
        };

        let saved_depth = self.frames.len();
        let saved_open_uv_len = self.open_upvalues.len();

        // Inner call: results go to result_base+1 to leave room for the boolean
        let inner_result_base = result_base + 1;
        let inner_num_results = if num_results < 0 {
            -1
        } else if num_results > 1 {
            num_results - 1
        } else {
            0
        };

        // Set up the call on the stack
        self.ensure_stack(inner_result_base + call_args.len() + 1);
        self.stack[inner_result_base] = func;
        for (i, arg) in call_args.iter().enumerate() {
            self.stack[inner_result_base + 1 + i] = *arg;
        }

        let call_result = self.do_call(
            func,
            inner_result_base,
            &call_args,
            inner_result_base,
            inner_num_results,
        );

        match call_result {
            Ok(()) => {
                if self.frames.len() > saved_depth {
                    // Lua function: a frame was pushed, execute it protectedly
                    match self.execute_to_depth(saved_depth) {
                        Ok(()) => {
                            // Success: prepend true
                            self.stack[result_base] = Value::Boolean(true);
                            if num_results < 0 {
                                // self.top was set by the inner Return; it points
                                // past the last inner result. That's already correct
                                // since results sit at inner_result_base..self.top
                                // and we wrote true at result_base = inner_result_base - 1.
                            }
                        }
                        Err(e) => {
                            let err_val = e.to_value(&mut self.gc);
                            self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                            self.place_results(
                                result_base,
                                num_results,
                                &[Value::Boolean(false), err_val],
                            );
                        }
                    }
                } else {
                    // Native function completed synchronously
                    // do_call already placed results at inner_result_base
                    self.stack[result_base] = Value::Boolean(true);
                    if num_results < 0 {
                        // top was set by place_results inside do_call. The results
                        // are at inner_result_base..self.top. Result_base has true.
                    }
                }
            }
            Err(e) => {
                // The call itself failed (e.g. calling a non-function)
                let err_val = e.to_value(&mut self.gc);
                self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Boolean(false), err_val],
                );
            }
        }
        Ok(())
    }

    /// Unwind frames and upvalues back to a saved checkpoint after an error.
    fn recover_from_error(&mut self, saved_depth: usize, saved_open_uv_len: usize, err_obj: Option<Value>) {
        // Close TBC vars for all frames being unwound
        if self.frames.len() > saved_depth {
            let from = self.frames[saved_depth].base;
            let _ = self.close_tbc_vars(from, err_obj);
        }
        while self.frames.len() > saved_depth {
            let frame_base = self.frames.last().unwrap().base;
            self.close_upvalues(frame_base);
            self.frames.pop();
        }
        // Trim any open upvalues that were created inside the failed call
        self.open_upvalues.truncate(saved_open_uv_len);
    }

    // ── For-loop helpers ───────────────────────────────────────────

    fn for_prep_validate(
        &self,
        init: Value,
        limit: Value,
        step: Value,
    ) -> Result<(), LuaError> {
        if !init.is_number() {
            return Err(LuaError::new("'for' initial value must be a number"));
        }
        if !limit.is_number() {
            return Err(LuaError::new("'for' limit must be a number"));
        }
        if !step.is_number() {
            return Err(LuaError::new("'for' step must be a number"));
        }
        match step {
            Value::Integer(0) => Err(LuaError::new("'for' step is zero")),
            Value::Float(f) if f == 0.0 => Err(LuaError::new("'for' step is zero")),
            _ => Ok(()),
        }
    }

    fn for_loop_check(&self, index: Value, limit: Value, step: Value) -> bool {
        let step_positive = match step {
            Value::Integer(s) => s > 0,
            Value::Float(f) => f > 0.0,
            _ => true,
        };
        if step_positive {
            Self::try_compare_le(index, limit).unwrap_or(false)
        } else {
            Self::try_compare_le(limit, index).unwrap_or(false)
        }
    }
}

// ── Lua integer arithmetic helpers ─────────────────────────────────

/// Lua floor division for integers.
fn lua_idiv(a: i64, b: i64) -> i64 {
    let d = a / b;
    if (a ^ b) < 0 && d * b != a {
        d - 1
    } else {
        d
    }
}

/// Lua modulo for integers.
fn lua_imod(a: i64, b: i64) -> i64 {
    let r = a % b;
    if r != 0 && (r ^ b) < 0 {
        r + b
    } else {
        r
    }
}

/// Lua modulo for floats.
fn lua_fmod(a: f64, b: f64) -> f64 {
    let r = a % b;
    if r != 0.0 && r.is_sign_negative() != b.is_sign_negative() {
        r + b
    } else {
        r
    }
}

/// Lua left shift.
fn lua_shl(x: i64, y: i64) -> i64 {
    if y >= 64 || y <= -64 {
        0
    } else if y >= 0 {
        (x as u64).wrapping_shl(y as u32) as i64
    } else {
        (x as u64).wrapping_shr((-y) as u32) as i64
    }
}

/// Lua right shift.
fn lua_shr(x: i64, y: i64) -> i64 {
    lua_shl(x, -y)
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run_lua(source: &str) -> Result<(), LuaError> {
        let mut lexer = Lexer::new(source.as_bytes(), "test");
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let block = parser.parse_chunk().expect("parser failed");
        let proto = compiler::compile(&block, Some("test".into())).expect("compiler failed");
        let mut vm = Vm::new();
        vm.execute_main(proto)
    }

    #[test]
    fn test_empty_program() {
        run_lua("").unwrap();
    }

    #[test]
    fn test_print_hello() {
        run_lua(r#"print("hello, world!")"#).unwrap();
    }

    #[test]
    fn test_print_arithmetic() {
        run_lua("print(1 + 2)").unwrap();
    }

    #[test]
    fn test_local_variables() {
        run_lua("local x = 10\nlocal y = 20\nprint(x + y)").unwrap();
    }

    #[test]
    fn test_if_else() {
        run_lua(
            r#"
            local x = 10
            if x > 5 then
                print("big")
            else
                print("small")
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_while_loop() {
        run_lua(
            r#"
            local i = 1
            while i <= 5 do
                print(i)
                i = i + 1
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_numeric_for() {
        run_lua(
            r#"
            for i = 1, 5 do
                print(i)
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_function_def_and_call() {
        run_lua(
            r#"
            function add(a, b)
                return a + b
            end
            print(add(3, 4))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_local_function() {
        run_lua(
            r#"
            local function fib(n)
                if n <= 1 then
                    return n
                end
                return fib(n - 1) + fib(n - 2)
            end
            print(fib(10))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_table_constructor() {
        run_lua(
            r#"
            local t = {1, 2, 3}
            print(t[1], t[2], t[3])
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_string_concatenation() {
        run_lua(
            r#"
            local name = "world"
            print("hello, " .. name .. "!")
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_type_function() {
        run_lua(
            r#"
            print(type(42))
            print(type("hello"))
            print(type(nil))
            print(type(true))
            print(type({}))
            print(type(print))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_closures() {
        run_lua(
            r#"
            function counter()
                local n = 0
                return function()
                    n = n + 1
                    return n
                end
            end
            local c = counter()
            print(c())
            print(c())
            print(c())
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_multiple_return() {
        run_lua(
            r#"
            function multi()
                return 1, 2, 3
            end
            print(multi())
            "#,
        )
        .unwrap();
    }
}
