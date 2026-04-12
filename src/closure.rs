//! Lua closure (function + upvalues).

use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::Proto;
use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

/// A native function: takes arguments + GC reference, returns results.
pub type NativeFn = fn(&[Value], &mut Gc) -> Result<Vec<Value>, LuaError>;

/// Runtime upvalue: may be open (on stack) or closed (captured).
#[derive(Debug)]
pub enum Upvalue {
    /// Points to a stack slot (still on the stack).
    Open(usize),
    /// Value has been captured off the stack.
    Closed(Value),
}

/// Shared reference to an upvalue.
pub type UpvalueRef = Rc<RefCell<Upvalue>>;

/// A Lua closure: either a compiled Lua function or a native Rust function.
pub enum Closure {
    Lua(LuaClosure),
    Native(NativeClosure),
}

/// A compiled Lua function closure: bytecode prototype + runtime upvalues.
pub struct LuaClosure {
    pub proto: Rc<Proto>,
    pub upvalues: Vec<UpvalueRef>,
}

/// A native (Rust) function closure.
pub struct NativeClosure {
    pub name: &'static str,
    pub func: NativeFn,
}

impl Closure {
    /// Create a new Lua closure.
    pub fn new_lua(proto: Rc<Proto>, upvalues: Vec<UpvalueRef>) -> Self {
        Closure::Lua(LuaClosure { proto, upvalues })
    }

    /// Create a new native closure.
    pub fn new_native(name: &'static str, func: NativeFn) -> Self {
        Closure::Native(NativeClosure { name, func })
    }

    /// Stub constructor for backward compatibility with tests.
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for Closure {
    fn default() -> Self {
        Closure::Native(NativeClosure {
            name: "<default>",
            func: |_, _| Ok(vec![]),
        })
    }
}
