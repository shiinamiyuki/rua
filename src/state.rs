use crate::{bytecode::ByteCode, gc::Gc, runtime::Globals, value::{Closure, Managed, Value, ValueData}, vm::Instance};
use std::{rc::Rc};

pub const MAX_LOCALS: usize = 256;
struct Frame {
    locals: [Value; MAX_LOCALS],
    frame_bottom: usize,
    prev_frame_top: usize, // stack[prev_frame_top..frame_bottom] are arguments
    closure: *const Managed<Closure>,
    ip: usize,
    n_args: usize,
}
impl Frame {
    fn new(frame_bottom: usize, prev_frame_top: usize, closure: *const Managed<Closure>) -> Self {
        Self {
            locals: [Value::default(); MAX_LOCALS],
            frame_bottom,
            closure,
            ip: 0,
            prev_frame_top,
            n_args: frame_bottom - prev_frame_top,
        }
    }
}
pub struct ArithmeticError {
    pub msg: String,
}
/*
A state is the execution state of the vm
An instance have only one state
A runtime has multiple instances
*/
pub struct State {
    gc: Rc<Gc>,
    globals: Rc<Globals>,
    frames: Vec<Frame>,
    eval_stack: Vec<Value>,
}

macro_rules! binary_op_impl {
    ($op:tt,$a:expr,$b:expr) => {
        if let (Some(a), Some(b)) = ($a.number(), $b.number()) {
            Ok(Value::from_number(a $op b))
        } else {
            Err(ArithmeticError {
                msg: format!(
                    "attempt to perform arithmetic operation '{}' between {} and {}",
                    stringify!($op),
                    $a.type_of(),
                    $b.type_of()
                ),
            })
        }
    };
}
macro_rules! int_binary_op_impl {
    ($op:tt,$a:expr,$b:expr) => {
        if let (Some(a), Some(b)) = ($a.number(), $b.number()) {
            let a = {
                if a.fract() != 0.0 {
                    return Err(ArithmeticError {
                        msg:format!("number {} has no integer representation", a)
                    });
                }
                a.trunc() as i64
            };
            let b = {
                if b.fract() != 0.0 {
                    return Err(ArithmeticError {
                        msg:format!("number {} has no integer representation", b)
                    });
                }
                b.trunc() as i64
            };
            Ok(Value::from_number((a $op b) as f64))
        } else {
            Err(ArithmeticError {
                msg: format!(
                    "attempt to perform arithmetic operation '{}' between {} and {}",
                    stringify!($op),
                    $a.type_of(),
                    $b.type_of()
                ),
            })
        }
    };
}
impl State {
    pub fn get_arg_count(&self) -> usize {
        0
    }
    pub fn arg(&self, i: usize) -> Option<Value> {
        None
    }
    pub fn ret(&self, i: usize, value: Value) {}

    pub fn add(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        binary_op_impl!(+, a,b)
    }
    pub fn sub(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        binary_op_impl!(-, a,b)
    }
    pub fn mul(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        binary_op_impl!(*, a,b)
    }
    pub fn div(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        binary_op_impl!(/, a,b)
    }
    pub fn mod_(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        binary_op_impl!(%, a,b)
    }
    pub fn pow(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            Ok(Value::from_number(a.powf(b)))
        } else {
            Err(ArithmeticError {
                msg: format!(
                    "attempt to perform arithmetic operation '^' between {} and {}",
                    a.type_of(),
                    b.type_of()
                ),
            })
        }
    }
    pub fn bitwise_and(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        int_binary_op_impl!(&, a, b)
    }
    pub fn bitwise_or(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        int_binary_op_impl!(|, a, b)
    }

    pub fn idiv(&self, a: Value, b: Value) -> Result<Value, ArithmeticError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            Ok(Value::from_number((a/b).floor()))
        } else {
            Err(ArithmeticError {
                msg: format!(
                    "attempt to perform arithmetic operation '//' between {} and {}",
                    a.type_of(),
                    b.type_of()
                ),
            })
        }
    }
}
