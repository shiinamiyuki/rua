use crate::{
    bytecode::ByteCode,
    closure::Closure,
    gc::{Gc, GcState, Traceable},
    runtime::{ErrorKind, RuntimeError, ValueRef},
    table::Table,
    value::{Managed, ManagedCell, Tuple, TupleUnpack, UserData, Value, ValueData},
    vm::Instance,
    Stack,
};
use std::{any::TypeId, cell::RefCell, cmp::Ordering, marker::PhantomData, rc::Rc};

pub const MAX_LOCALS: usize = 256;
pub(crate) struct Frame {
    pub(crate) locals: [Value; MAX_LOCALS],
    pub(crate) frame_bottom: usize, //stack[frame_buttom..frame_buttom_n_args]
    pub(crate) closure: Option<Gc<Closure>>,
    pub(crate) ip: usize,
    pub(crate) n_args: usize,
}
impl Frame {
    pub(crate) fn get_ip(closure: Option<Gc<Closure>>) -> usize {
        {
            closure.as_ref().map_or(0, |c| c.entry)
        }
    }
    pub(crate) fn new(frame_bottom: usize, n_args: usize, closure: Option<Gc<Closure>>) -> Self {
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
    pub(crate) gc: Rc<GcState>,
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
    pub(crate) instance: &'a Instance,
    pub(crate) ret_values: RefCell<Vec<Value>>,
    pub(crate) frames: &'a Stack<Frame>,
}

impl<'a> CallContext<'a> {
    pub fn create_number(&self, x: f64) -> ValueRef<'a> {
        ValueRef {
            phantom: PhantomData {},
            value: Value::from_number(x),
        }
    }
    pub fn create_bool(&self, x: bool) -> ValueRef<'a> {
        ValueRef {
            phantom: PhantomData {},
            value: Value::from_bool(x),
        }
    }
    pub fn create_userdata<T: UserData + Traceable>(&self, userdata: T) -> ValueRef<'a> {
        ValueRef {
            phantom: PhantomData {},
            value: self.state.create_userdata(userdata),
        }
    }
    pub fn create_string(&self, s: String) -> ValueRef<'a> {
        ValueRef {
            phantom: PhantomData {},
            value: self.state.create_string(s),
        }
    }
    pub fn get_arg_count(&self) -> usize {
        let frame = self.frames.last().unwrap();
        frame.n_args
    }
    pub fn arg(&'a self, i: usize) -> Result<ValueRef<'a>, RuntimeError> {
        let frame = self.frames.last().unwrap();
        if i < frame.n_args {
            Ok(ValueRef {
                value: frame.locals[i],
                phantom: PhantomData {},
            })
            // Some(self.eval_stack.borrow()[frame.frame_bottom + i])
        } else {
            Err(RuntimeError {
                kind: ErrorKind::ArgumentArityError,
                msg: format!("arg {} is not supplied", i,),
            })
        }
    }
    pub fn table_set(
        &self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
        value: ValueRef<'a>,
    ) -> Result<(), RuntimeError> {
        self.state.table_set(table.value, key.value, value.value)
    }
    pub fn table_get(
        &self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
    ) -> Result<ValueRef<'a>, RuntimeError> {
        Ok(ValueRef {
            value: self.state.table_get(table.value, key.value)?,
            phantom: PhantomData {},
        })
    }
    pub fn ret(&self, i: usize, value: ValueRef<'a>) {
        let mut ret_values = self.ret_values.borrow_mut();
        if i >= ret_values.len() {
            ret_values.resize(i + 1, Value::nil());
        }
        ret_values[i] = value.value;
    }
    pub fn call(
        &self,
        closure: ValueRef<'a>,
        args: &[ValueRef<'a>],
    ) -> Result<ValueRef<'a>, RuntimeError> {
        let args: Vec<_> = args.iter().map(|x| x.value).collect();
        Ok(ValueRef {
            value: self.instance.call(closure.value, &args)?,
            phantom: PhantomData {},
        })
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
                data: ValueData::Tuple(self.state.gc.allocate(Tuple {
                    values: rv,
                    unpack: TupleUnpack::TruncateFill,
                })),
                metatable: None,
            }
        };
        let mut st = self.state.eval_stack.borrow_mut();
        st.push(ret);
    }
}
impl State {
    pub(crate) fn get_global(&self, name: Value) -> Value {
        let globals = self.globals.as_table().unwrap();
        let globals = globals.borrow();
        globals.get(name)
    }
    pub(crate) fn set_global(&self, name: Value, value: Value) {
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(name, value)
    }
    // fn get_arg_count(&self) -> usize {
    //     let frames = self.frames.borrow();
    //     let frame = frames.last().unwrap();
    //     frame.n_args
    // }
    // fn arg(&self, i: usize) -> Value {
    //     let frames = self.frames.borrow();
    //     let frame = frames.last().unwrap();
    //     if i < frame.n_args {
    //         frame.locals[i]
    //         // Some(self.eval_stack.borrow()[frame.frame_bottom + i])
    //     } else {
    //         Value::nil()
    //     }
    // }
    pub(crate) fn table_get(&self, table: Value, key: Value) -> Result<Value, RuntimeError> {
        let table = match table.as_table() {
            Some(x) => x,
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!(" attempt to index a {} value, key:{}", table.type_of(), key.print()),
                })
            }
        };
        let table = table.borrow();
        Ok(table.get(key))
    }
    pub(crate) fn table_set(
        &self,
        table: Value,
        key: Value,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let table = match table.as_table() {
            Some(x) => x,
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!(
                        " attempt to index a {} value, key:{}, value:{}",
                        table.type_of(),
                        key.print(),
                        value.print()
                    ),
                })
            }
        };
        let mut table = table.borrow_mut();
        table.set(key, value);
        Ok(())
    }
    // pub fn call(&self, closure: Value, args: &[Value]) -> Result<Value, RuntimeError> {
    //     unsafe {
    //         match closure.data {
    //             ValueData::Closure(closure) => {
    //                 ip_modified = true;
    //                 *ip = Frame::get_ip(closure);
    //                 for i in 0..n_args {
    //                     frame.locals[i] = eval_stack[eval_stack.len() - n_args + i];
    //                 }
    //                 frame.n_args = n_args;
    //                 let len = eval_stack.len();
    //                 eval_stack.resize(len - n_args, Value::nil());
    //             }
    //             ValueData::Callable(callable) => {
    //                 *ip += 1;
    //                 let mut frame = Frame::new(eval_stack.len() - n_args, n_args, std::ptr::null());
    //                 for i in 0..n_args {
    //                     frame.locals[i] = eval_stack[eval_stack.len() - n_args + i];
    //                 }
    //                 let len = eval_stack.len();
    //                 eval_stack.resize(len - n_args, Value::nil());
    //                 return Ok(Continue::CallExt(callable, frame));
    //             }
    //             _ => unreachable!(),
    //         }
    //     }
    // }
    pub(crate) fn create_userdata<T: UserData + Traceable>(&self, userdata: T) -> Value {
        let p: Box<dyn UserData> = Box::new(userdata);
        let s = self.gc.allocate(p);
        Value {
            data: ValueData::UserData(s),
            metatable: None,
        }
    }
    pub(crate) fn create_string(&self, s: String) -> Value {
        let s = self.gc.allocate(Managed { data: s });
        Value {
            data: ValueData::String(s),
            metatable: None,
        }
    }
    pub(crate) fn create_table(&self, t: Table) -> Value {
        let t = self.gc.allocate(RefCell::new(t));
        Value {
            data: ValueData::Table(t),
            metatable: None,
        }
    }
    pub(crate) fn create_closure(&self, c: Closure) -> Value {
        let c = self.gc.allocate(c);
        Value {
            data: ValueData::Closure(c),
            metatable: None,
        }
    }
    pub(crate) fn add(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(+, a,b)
    }
    pub(crate) fn sub(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(-, a,b)
    }
    pub(crate) fn mul(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(*, a,b)
    }
    pub(crate) fn div(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(/, a,b)
    }
    pub(crate) fn mod_(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        binary_op_impl!(%, a,b)
    }
    pub(crate) fn pow(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
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
    pub(crate) fn bitwise_and(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        int_binary_op_impl!(&, a, b)
    }
    pub(crate) fn bitwise_or(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        int_binary_op_impl!(|, a, b)
    }

    pub(crate) fn cmp(&self, a: Value, b: Value) -> Result<Ordering, RuntimeError> {
        let res = match (a.data, b.data) {
            (ValueData::Nil, ValueData::Nil) => Ok(Ordering::Equal),
            (ValueData::Bool(a), ValueData::Bool(b)) => Ok(a.cmp(&b)),
            (ValueData::Number(a), ValueData::Number(b)) => Ok(a.cmp(&b)),
            (ValueData::String(a), ValueData::String(b)) => {
                let a = &(*a).data;
                let b = &(*b).data;
                Ok(a.cmp(b))
            }
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

    pub(crate) fn lt(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Less))
    }
    pub(crate) fn le(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(
            ordering == Ordering::Less || ordering == Ordering::Equal,
        ))
    }
    pub(crate) fn gt(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Greater))
    }
    pub(crate) fn ge(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(
            ordering == Ordering::Greater || ordering == Ordering::Equal,
        ))
    }
    pub(crate) fn eq(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering == Ordering::Equal))
    }
    pub(crate) fn ne(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        let ordering = self.cmp(a, b)?;
        Ok(Value::from_bool(ordering != Ordering::Equal))
    }
    pub(crate) fn idiv(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            if b == 0.0 {
                return Err(RuntimeError {
                    kind: ErrorKind::ArithmeticError,
                    msg: "attempt to divide by zero".into(),
                });
            }
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
    pub(crate) fn len(&self, a: Value) -> Result<Value, RuntimeError> {
        match a.data {
            // ValueData::Nil => todo!(),
            // ValueData::Bool(_) => todo!(),
            // ValueData::Number(_) => todo!(),
            ValueData::Table(x) => Ok(Value::from_number((*x).borrow_mut().len() as f64)),
            ValueData::String(x) => Ok(Value::from_number((*x).data.len() as f64)),
            // ValueData::Closure(_) => todo!(),
            // ValueData::Callable(_) => todo!(),
            ValueData::Tuple(x) => Ok(Value::from_number((*x).values.len() as f64)),
            _ => Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(" attempt to get length of a {} value", a.type_of(),),
            }),
        }
    }
    pub(crate) fn not(&self, a: Value) -> Result<Value, RuntimeError> {
        Ok(Value::from_bool(!a.to_bool()))
    }
    pub(crate) fn bitwise_not(&self, a: Value) -> Result<Value, RuntimeError> {
        match a.data {
            // ValueData::Nil => todo!(),
            // ValueData::Bool(_) => todo!(),
            ValueData::Number(_) => {
                let i = a.as_i64();
                if let Some(i) = i {
                    Ok(Value::from_number((!i) as f64))
                } else {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: "number has no integer representation".into(),
                    })
                }
            }
            _ => Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(" attempt to get length of a {} value", a.type_of(),),
            }),
        }
    }
    pub(crate) fn neg(&self, a: Value) -> Result<Value, RuntimeError> {
        match a.data {
            // ValueData::Nil => todo!(),
            // ValueData::Bool(_) => todo!(),
            ValueData::Number(x) => Ok(Value::from_number(*-x)),
            _ => Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!(" attempt to get length of a {} value", a.type_of(),),
            }),
        }
    }
}
