//! Virtual machine: register-based bytecode execution.

use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::*;
use crate::closure::*;
use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind, GcRef};
use crate::table::Table;
use crate::value::Value;

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

        // Register built-in functions
        self.register_native(&mut env, "print", crate::stdlib::lua_print);
        self.register_native(&mut env, "type", crate::stdlib::lua_type);
        self.register_native(&mut env, "tostring", crate::stdlib::lua_tostring);
        self.register_native(&mut env, "tonumber", crate::stdlib::lua_tonumber);
        self.register_native(&mut env, "assert", crate::stdlib::lua_assert);
        self.register_native(&mut env, "error", crate::stdlib::lua_error);
        self.register_native(&mut env, "pcall", crate::stdlib::lua_pcall);
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

    // ── Arithmetic helpers ─────────────────────────────────────────

    fn arith_add(a: Value, b: Value) -> Result<Value, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x.wrapping_add(y))),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 + y)),
            (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x + y as f64)),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    fn arith_sub(a: Value, b: Value) -> Result<Value, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x.wrapping_sub(y))),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
            (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 - y)),
            (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x - y as f64)),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    fn arith_mul(a: Value, b: Value) -> Result<Value, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x.wrapping_mul(y))),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
            (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 * y)),
            (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x * y as f64)),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    fn arith_div(a: Value, b: Value) -> Result<Value, LuaError> {
        // Float division always returns float
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                return Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    a.type_name()
                )))
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                return Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    b.type_name()
                )))
            }
        };
        Ok(Value::Float(x / y))
    }

    fn arith_idiv(a: Value, b: Value) -> Result<Value, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n//0'"));
                }
                Ok(Value::Integer(lua_idiv(x, y)))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float((x / y).floor())),
            (Value::Integer(x), Value::Float(y)) => Ok(Value::Float((x as f64 / y).floor())),
            (Value::Float(x), Value::Integer(y)) => Ok(Value::Float((x / y as f64).floor())),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    fn arith_mod(a: Value, b: Value) -> Result<Value, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n%0'"));
                }
                Ok(Value::Integer(lua_imod(x, y)))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(lua_fmod(x, y))),
            (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(lua_fmod(x as f64, y))),
            (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(lua_fmod(x, y as f64))),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    fn arith_pow(a: Value, b: Value) -> Result<Value, LuaError> {
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                return Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    a.type_name()
                )))
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                return Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    b.type_name()
                )))
            }
        };
        Ok(Value::Float(x.powf(y)))
    }

    fn arith_unm(a: Value) -> Result<Value, LuaError> {
        match a {
            Value::Integer(x) => Ok(Value::Integer(x.wrapping_neg())),
            Value::Float(x) => Ok(Value::Float(-x)),
            _ => Err(LuaError::new(format!(
                "attempt to perform arithmetic on a {} value",
                a.type_name()
            ))),
        }
    }

    // ── Bitwise helpers ────────────────────────────────────────────

    fn to_integer(v: Value) -> Result<i64, LuaError> {
        match v {
            Value::Integer(n) => Ok(n),
            Value::Float(f) => {
                let i = f as i64;
                if i as f64 == f {
                    Ok(i)
                } else {
                    Err(LuaError::new("number has no integer representation"))
                }
            }
            _ => Err(LuaError::new(format!(
                "attempt to perform bitwise operation on a {} value",
                v.type_name()
            ))),
        }
    }

    // ── Comparison helpers ─────────────────────────────────────────

    fn compare_eq(a: Value, b: Value) -> bool {
        a == b
    }

    fn compare_lt(a: Value, b: Value) -> Result<bool, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Ok(x < y),
            (Value::Float(x), Value::Float(y)) => Ok(x < y),
            (Value::Integer(x), Value::Float(y)) => Ok((x as f64) < y),
            (Value::Float(x), Value::Integer(y)) => Ok(x < (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Ok(sa.as_bytes() < sb.as_bytes())
                    }
                    _ => Err(LuaError::new("attempt to compare two non-comparable values")),
                }
            }
            _ => Err(LuaError::new(format!(
                "attempt to compare {} with {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn compare_le(a: Value, b: Value) -> Result<bool, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Ok(x <= y),
            (Value::Float(x), Value::Float(y)) => Ok(x <= y),
            (Value::Integer(x), Value::Float(y)) => Ok((x as f64) <= y),
            (Value::Float(x), Value::Integer(y)) => Ok(x <= (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Ok(sa.as_bytes() <= sb.as_bytes())
                    }
                    _ => Err(LuaError::new("attempt to compare two non-comparable values")),
                }
            }
            _ => Err(LuaError::new(format!(
                "attempt to compare {} with {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    // ── String concatenation helper ────────────────────────────────

    fn concat_values(&mut self, base: usize, from: usize, to: usize) -> Result<Value, LuaError> {
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
                _ => {
                    return Err(LuaError::new(format!(
                        "attempt to concatenate a {} value",
                        v.type_name()
                    )))
                }
            }
        }
        Ok(Value::Object(self.gc.new_string(&result)))
    }

    // ── Table access helpers ───────────────────────────────────────

    fn table_get(table_val: Value, key: Value) -> Result<Value, LuaError> {
        match table_val {
            Value::Object(r) => match &r.as_object().kind {
                GcObjectKind::Table(t) => Ok(t.raw_get(&key)),
                _ => Err(LuaError::new(format!(
                    "attempt to index a {} value",
                    table_val.type_name()
                ))),
            },
            _ => Err(LuaError::new(format!(
                "attempt to index a {} value",
                table_val.type_name()
            ))),
        }
    }

    fn table_set(table_val: Value, key: Value, val: Value) -> Result<(), LuaError> {
        match table_val {
            Value::Object(mut r) => match &mut r.as_object_mut().kind {
                GcObjectKind::Table(t) => {
                    t.raw_set(key, val);
                    Ok(())
                }
                _ => Err(LuaError::new(format!(
                    "attempt to index a {} value",
                    table_val.type_name()
                ))),
            },
            _ => Err(LuaError::new(format!(
                "attempt to index a {} value",
                table_val.type_name()
            ))),
        }
    }

    // ── Main dispatch loop ─────────────────────────────────────────

    fn execute(&mut self) -> Result<(), LuaError> {
        loop {
            if self.frames.is_empty() {
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
                    let result = Self::table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTabUp => {
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, a);
                    let key = self.frames[fi].proto.constants[b].to_value(&mut self.gc);
                    let val = self.reg(base, c);
                    Self::table_set(table_val, key, val)?;
                }

                // ── Tables ─────────────────────────────────────────
                OpCode::NewTable => {
                    let table = Table::with_capacity(b, c);
                    let gc_ref = self.gc.new_table(table);
                    self.set_reg(base, a, Value::Object(gc_ref));
                }

                OpCode::GetTable => {
                    let table_val = self.reg(base, b);
                    let key = self.reg(base, c);
                    let result = Self::table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTable => {
                    let table_val = self.reg(base, a);
                    let key = self.reg(base, b);
                    let val = self.reg(base, c);
                    Self::table_set(table_val, key, val)?;
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
                    self.set_reg(base, a, Self::arith_add(rb, rc)?);
                }

                OpCode::Sub => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_sub(rb, rc)?);
                }

                OpCode::Mul => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_mul(rb, rc)?);
                }

                OpCode::Div => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_div(rb, rc)?);
                }

                OpCode::IDiv => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_idiv(rb, rc)?);
                }

                OpCode::Mod => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_mod(rb, rc)?);
                }

                OpCode::Pow => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    self.set_reg(base, a, Self::arith_pow(rb, rc)?);
                }

                OpCode::Unm => {
                    let rb = self.reg(base, b);
                    self.set_reg(base, a, Self::arith_unm(rb)?);
                }

                // ── Bitwise ────────────────────────────────────────
                OpCode::BAnd => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    let y = Self::to_integer(self.reg(base, c))?;
                    self.set_reg(base, a, Value::Integer(x & y));
                }

                OpCode::BOr => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    let y = Self::to_integer(self.reg(base, c))?;
                    self.set_reg(base, a, Value::Integer(x | y));
                }

                OpCode::BXor => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    let y = Self::to_integer(self.reg(base, c))?;
                    self.set_reg(base, a, Value::Integer(x ^ y));
                }

                OpCode::Shl => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    let y = Self::to_integer(self.reg(base, c))?;
                    self.set_reg(base, a, Value::Integer(lua_shl(x, y)));
                }

                OpCode::Shr => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    let y = Self::to_integer(self.reg(base, c))?;
                    self.set_reg(base, a, Value::Integer(lua_shr(x, y)));
                }

                OpCode::BNot => {
                    let x = Self::to_integer(self.reg(base, b))?;
                    self.set_reg(base, a, Value::Integer(!x));
                }

                // ── Logic ──────────────────────────────────────────
                OpCode::Not => {
                    let rb = self.reg(base, b);
                    self.set_reg(base, a, Value::Boolean(!rb.is_truthy()));
                }

                // ── String / Length ────────────────────────────────
                OpCode::Concat => {
                    let result = self.concat_values(base, b, c)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Len => {
                    let rb = self.reg(base, b);
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

                // ── Comparison & Conditional ───────────────────────
                OpCode::Eq => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = Self::compare_eq(rb, rc);
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Lt => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = Self::compare_lt(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Le => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = Self::compare_le(rb, rc)?;
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
                    let adjusted = Self::arith_sub(init, step)?;
                    self.set_reg(base, a, adjusted);
                    // Jump forward to FORLOOP
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::ForLoop => {
                    let index = self.reg(base, a);
                    let limit = self.reg(base, a + 1);
                    let step = self.reg(base, a + 2);

                    let new_index = Self::arith_add(index, step)?;
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
                    self.call_function(base + a, b, if c == 0 { -1 } else { c as i32 - 1 })?;
                }

                OpCode::TailCall => {
                    let func_val = self.reg(base, a);
                    let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                    let args: Vec<Value> = (0..num_args)
                        .map(|i| self.stack[base + a + 1 + i])
                        .collect();

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
                    self.close_upvalues(base + a);
                }

                OpCode::Tbc => {
                    // To-be-closed: tracked but __close not called yet (Phase 2)
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

    /// Internal call dispatch: handles both Lua and native closures.
    fn do_call(
        &mut self,
        func_val: Value,
        call_base: usize,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let gc_ref = match func_val {
            Value::Object(r) if r.as_object().as_closure().is_some() => r,
            _ => {
                return Err(LuaError::new(format!(
                    "attempt to call a {} value",
                    func_val.type_name()
                )))
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
            let results = func(args, &mut self.gc)?;
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
            for i in 0..num_params.min(args.len()) {
                self.stack[new_base + i] = args[i];
            }
            for i in args.len()..num_params {
                self.stack[new_base + i] = Value::Nil;
            }

            let varargs = if is_vararg && args.len() > num_params {
                args[num_params..].to_vec()
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

        Ok(())
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
            Self::compare_le(index, limit).unwrap_or(false)
        } else {
            Self::compare_le(limit, index).unwrap_or(false)
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
