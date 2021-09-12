use crate::{Stack, bytecode::ByteCode, closure::Closure, gc::Gc, runtime::{ErrorKind, RuntimeError}, table::Table, value::{Managed, ManagedCell, Tuple, TupleUnpack, Value, ValueData}, vm::Instance};
use std::{cell::RefCell, cmp::Ordering, rc::Rc};

pub const MAX_LOCALS: usize = 256;
pub(crate) struct Frame {
    pub(crate) locals: [Value; MAX_LOCALS],
    pub(crate) frame_bottom: usize, //stack[frame_buttom..frame_buttom_n_args]
    pub(crate) closure: *const Managed<Closure>,
    pub(crate) ip: usize,
    pub(crate) n_args: usize,
}
impl Frame {
    pub(crate) fn get_ip(closure: *const Managed<Closure>) -> usize {
        unsafe { closure.as_ref().map_or(0, |c| c.data.entry) }
    }
    pub(crate) fn new(
        frame_bottom: usize,
        n_args: usize,
        closure: *const Managed<Closure>,
    ) -> Self {
        Self {
            locals: [Value::default(); MAX_LOCALS],
            frame_bottom,
            closure,
            ip: Self::get_ip(closure),
            n_args,
        }
    }
}
/*
A state is the execution state of the vm
An instance have only one state
A runtime has multiple instances
*/
pub struct State {
    pub(crate) gc: Rc<Gc>,
    pub(crate) globals: Value,
    pub(crate) frames: RefCell<Stack<Frame>>,
    pub(crate) eval_stack: RefCell<Vec<Value>>,
}

macro_rules! binary_op_impl {
    ($op:tt,$a:expr,$b:expr) => {
        if let (Some(a), Some(b)) = ($a.number(), $b.number()) {
            Ok(Value::from_number(a $op b))
        } else {
            Err(RuntimeError {
                kind:ErrorKind::ArithmeticError,
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
                    return Err(RuntimeError {
                        kind:ErrorKind::ArithmeticError,
                        msg:format!("number {} has no integer representation", a)
                    });
                }
                a.trunc() as i64
            };
            let b = {
                if b.fract() != 0.0 {
                    return Err(RuntimeError {
                        kind:ErrorKind::ArithmeticError,
                        msg:format!("number {} has no integer representation", b)
                    });
                }
                b.trunc() as i64
            };
            Ok(Value::from_number((a $op b) as f64))
        } else {
            Err(RuntimeError {
                kind:ErrorKind::ArithmeticError,
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
pub struct CallContext<'a> {
    pub(crate) state: &'a State,
    pub(crate) ret_values: RefCell<Vec<Value>>,
}
impl<'a> CallContext<'a> {
    pub fn get_arg_count(&self) -> usize {
        self.state.get_arg_count()
    }
    pub fn arg(&self, i: usize) -> Value {
        self.state.arg(i)
    }
    pub fn ret(&self, i: usize, value: Value) {
        let mut ret_values = self.ret_values.borrow_mut();
        if i >= ret_values.len() {
            ret_values.resize(i + 1, Value::nil());
        }
        ret_values[i] = value;
    }
}
impl<'a> Drop for CallContext<'a> {
    fn drop(&mut self) {
        let mut ret_values = self.ret_values.borrow_mut();
        let ret = if ret_values.is_empty() {
            Value::nil()
        } else if ret_values.len() == 1 {
            ret_values[0]
        } else {
            let rv = std::mem::replace(&mut *ret_values, vec![]);
            Value {
                data: ValueData::Tuple(self.state.gc.manage(Managed {
                    data: Tuple {
                        values: rv,
                        unpack: TupleUnpack::TruncateFill,
                    },
                })),
                metatable: std::ptr::null(),
            }
        };
        let mut st = self.state.eval_stack.borrow_mut();
        st.push(ret);
    }
}
impl State {
    pub fn get_global(&self, name: Value) -> Value {
        let globals = self.globals.as_table().unwrap();
        let globals = globals.borrow();
        globals.get(name)
    }
    pub fn set_global(&self, name: Value, value: Value) {
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(name, value)
    }
    fn get_arg_count(&self) -> usize {
        let frames = self.frames.borrow();
        let frame = frames.last().unwrap();
        frame.n_args
    }
    fn arg(&self, i: usize) -> Value {
        let frames = self.frames.borrow();
        let frame = frames.last().unwrap();
        if i < frame.n_args {
            frame.locals[i]
            // Some(self.eval_stack.borrow()[frame.frame_bottom + i])
        } else {
            Value::nil()
        }
    }
    pub fn create_string(&self, s: String) -> Value {
        let s = self.gc.manage(Managed { data: s });
        Value {
            data: ValueData::String(s),
            metatable: std::ptr::null(),
        }
    }
    pub fn create_table(&self, t: Table) -> Value {
        let t = self.gc.manage(ManagedCell::new(RefCell::new(t)));
        Value {
            data: ValueData::Table(t),
            metatable: std::ptr::null(),
        }
    }
    pub fn create_closure(&self, c: Closure) -> Value {
        let c = self.gc.manage(Managed::new(c));
        Value {
            data: ValueData::Closure(c),
            metatable: std::ptr::null(),
        }
    }
    pub fn add(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(+, a,b)
    }
    pub fn sub(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(-, a,b)
    }
    pub fn mul(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(*, a,b)
    }
    pub fn div(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(/, a,b)
    }
    pub fn mod_(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(%, a,b)
    }
    pub fn pow(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            Ok(Value::from_number(a.powf(b)))
        } else {
            Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(
                    "attempt to perform arithmetic operation '^' between {} and {}",
                    a.type_of(),
                    b.type_of()
                ),
            })
        }
    }
    pub fn bitwise_and(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        int_binary_op_impl!(&, a, b)
    }
    pub fn bitwise_or(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        int_binary_op_impl!(|, a, b)
    }

    pub fn cmp(&self, a: Value, b: Value) -> Result<Ordering, RuntimeError> {
        let res = match (a.data, b.data) {
            (ValueData::Nil, ValueData::Nil) => Ok(Ordering::Equal),
            (ValueData::Bool(a), ValueData::Bool(b)) => Ok(a.cmp(&b)),
            (ValueData::Number(a), ValueData::Number(b)) => Ok(a.cmp(&b)),
            (ValueData::String(a), ValueData::String(b)) => unsafe {
                let a = &(*a).data;
                let b = &(*b).data;
                Ok(a.cmp(b))
            },
            _ => Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(
                    "attempt to perform compare {} with {}",
                    a.type_of(),
                    b.type_of()
                ),
            }),
        };
        #[cfg(debug_assertions)]
        {
            match res {
                Ok(ordering) => {
                    if ordering == Ordering::Equal {
                        debug_assert!(a == b);
                    } else {
                        debug_assert!(a != b);
                    }
                }
                _ => {}
            }
        }
        res
    }
    pub fn lt(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Less))
    }
    pub fn le(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(
            ordering == Ordering::Less || ordering == Ordering::Equal,
        ))
    }
    pub fn gt(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Greater))
    }
    pub fn ge(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(
            ordering == Ordering::Greater || ordering == Ordering::Equal,
        ))
    }
    pub fn eq(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Equal))
    }
    pub fn ne(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering != Ordering::Equal))
    }
    pub fn idiv(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            Ok(Value::from_number((a / b).floor()))
        } else {
            Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(
                    "attempt to perform arithmetic operation '//' between {} and {}",
                    a.type_of(),
                    b.type_of()
                ),
            })
        }
    }
}
