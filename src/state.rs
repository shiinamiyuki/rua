use smallvec::{smallvec, SmallVec};

use crate::{
    api::{BaseApi, CallApi, StateApi},
    closure::Closure,
    debug_println,
    gc::{Gc, GcState, Traceable},
    runtime::{ConstantsIndex, ErrorKind, GlobalState, RuntimeError, Value},
    table::Table,
    value::{Managed, RawValue, Tuple, UserData},
    vm::Instance,
    CloneCell, Stack,
};
use std::{
    cell::{Cell, RefCell},
    rc::{Rc, Weak},
};

pub const MAX_LOCALS: usize = 256;
pub(crate) struct Frame {
    // pub(crate) locals: [RawValue; MAX_LOCALS],
    pub(crate) locals: SmallVec<[RawValue; 32]>,
    // pub(crate) frame_bottom: usize, //stack[frame_buttom..frame_buttom_n_args]
    pub(crate) closure: Option<Gc<Closure>>,
    pub(crate) ip: usize,
    pub(crate) n_args: usize,
    pub(crate) has_closed: bool,
}

impl Frame {
    pub(crate) fn get_ip(closure: Option<Gc<Closure>>) -> usize {
        {
            closure.as_ref().map_or(0, |c| c.entry)
        }
    }
    pub(crate) fn new(n_args: usize, closure: Option<Gc<Closure>>) -> Self {
        Self {
            locals: smallvec![RawValue::default();16.max(n_args)], //[RawValue::default(); MAX_LOCALS],
            closure: closure.clone(),
            ip: Self::get_ip(closure),
            n_args,
            has_closed: false,
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
    pub(crate) globals: RawValue,
    pub(crate) global_state: Rc<GlobalState>,
    pub(crate) frames: RefCell<Stack<Frame>>,
    pub(crate) eval_stack: RefCell<Vec<RawValue>>,
    pub(crate) instance: Weak<Instance>,
}

macro_rules! binary_op_impl_closue {
    ($self:expr, $op:expr, $op_str:expr, $a:expr,$b:expr,$metamethod:ident) => {
        if let (Some(a), Some(b)) = ($a.number(), $b.number()) {
            Ok(RawValue::from_number($op(a, b)))
        } else {
            macro_rules! error {
                () => {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: format!(
                            "attempt to perform arithmetic operation '{}' between {} and {}",
                            $op_str,
                            $a.type_of(),
                            $b.type_of()
                        ),
                    })
                };
            }
            let mt = $self.get_metatable(&$a);
            if !mt.is_nil() {
                let method = $self.table_get(
                    &mt,
                    &$self.global_state.constants[ConstantsIndex::$metamethod as usize].get(),
                )?;
                if method.is_nil() {
                    return error!();
                }
                let instance = $self.instance.upgrade().unwrap();
                return instance.call(&method, &[&$a, &$b]);
            }
            let mt = $self.get_metatable(&$b);
            if !mt.is_nil() {
                let method = $self.table_get(
                    &mt,
                    &$self.global_state.constants[ConstantsIndex::$metamethod as usize].get(),
                )?;
                if method.is_nil() {
                    return error!();
                }
                let instance = $self.instance.upgrade().unwrap();
                return instance.call(&method, &[&$a, &$b]);
            } else {
                return error!();
            }
        }
    };
}

macro_rules! binary_op_impl {
    ($self:expr, $op:tt,$a:expr,$b:expr,$metamethod:ident) => {
        binary_op_impl_closue!($self, |x,y|x $op y, stringify!($op), $a,$b,$metamethod)
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
            Ok(RawValue::from_number((a $op b) as f64))
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
    pub(crate) ret_values: RefCell<SmallVec<[RawValue; 8]>>,
    pub(crate) frame: Frame,
    pub(crate) n_expected_rets: u8,
}

impl<'a> CallContext<'a> {
    pub fn error(&self, msg: String) -> Result<(), RuntimeError> {
        Err(RuntimeError {
            kind: ErrorKind::ExternalError,
            msg,
        })
    }
    fn call_raw(&self, closure: &RawValue, args: &[&RawValue]) -> Result<RawValue, RuntimeError> {
        Ok(self.instance.call(closure, args)?)
    }
}
impl<'a> Drop for CallContext<'a> {
    fn drop(&mut self) {
        let mut ret_values = self.ret_values.borrow_mut();
        if self.n_expected_rets == u8::MAX {
            let ret = if ret_values.is_empty() {
                RawValue::nil()
            } else if ret_values.len() == 1 {
                ret_values[0].clone()
            } else {
                let rv = std::mem::replace(&mut *ret_values, smallvec![]);
                RawValue::Tuple(self.state.gc.allocate(Tuple {
                    values: RefCell::new(rv),
                    metatable: CloneCell::new(RawValue::Nil),
                    flag: crate::value::TupleFlag::VarArgs,
                }))
            };
            let mut st = self.state.eval_stack.borrow_mut();
            st.push(ret);
        } else {
            ret_values.resize(self.n_expected_rets as usize, RawValue::Nil);
            let mut st = self.state.eval_stack.borrow_mut();
            for v in ret_values.iter() {
                st.push(v.clone());
            }
        }
    }
}
impl State {
    // pub(crate) fn get_global(&self, name: Value) -> Value {
    //     let globals = self.globals.as_table().unwrap();
    //     let globals = globals.borrow();
    //     globals.get(name)
    // }
    // pub(crate) fn set_global(&self, name: Value, value: Value) {
    //     let globals = self.globals.as_table().unwrap();
    //     let mut globals = globals.borrow_mut();
    //     globals.set(name, value)
    // }
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
    pub(crate) fn table_rawget(
        &self,
        table: &RawValue,
        key: &RawValue,
    ) -> Result<RawValue, RuntimeError> {
        let table = match table.as_table() {
            Some(x) => x,
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!(
                        " attempt to index a {} value, key:{}",
                        table.type_of(),
                        key.print()
                    ),
                })
            }
        };
        let table = table.borrow();
        Ok(table.get(key))
    }
    pub(crate) fn table_rawset(
        &self,
        table: &RawValue,
        key: &RawValue,
        value: &RawValue,
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
        table.set(key.clone(), value.clone());
        Ok(())
    }
    pub(crate) fn table_get(
        &self,
        table: &RawValue,
        key: &RawValue,
    ) -> Result<RawValue, RuntimeError> {
        self.table_get_impl(table.clone(), key, 0)
    }
    pub(crate) fn table_get_impl(
        &self,
        mut table: RawValue,
        key: &RawValue,
        mut depth: u32,
    ) -> Result<RawValue, RuntimeError> {
        loop {
            if depth >= 32 {
                return Err(RuntimeError {
                    kind: ErrorKind::KeyError,
                    msg: format!("__index chain too long,possible loop",),
                });
            }
            let origin_table = table.clone();
            let (value, mt) = {
                let mt = self.get_metatable(&table);
                match table.as_table() {
                    Some(x) => {
                        let table = x.borrow();
                        (table.get(&key), mt)
                    }
                    None => {
                        if mt.is_nil() {
                            return Err(RuntimeError {
                                kind: ErrorKind::TypeError,
                                msg: format!(
                                    " attempt to index a {} value, key:{}",
                                    table.type_of(),
                                    key.print()
                                ),
                            });
                        } else {
                            (RawValue::Nil, mt)
                        }
                    }
                }
            };
            if !value.is_nil() || mt.is_nil() {
                return Ok(value);
            }

            table = self.table_get_impl(
                mt,
                &self.global_state.constants[ConstantsIndex::MtKeyIndex as usize].get(),
                depth + 1,
            )?;
            if table.as_callable().is_some() || table.as_closure().is_some() {
                let instance = self.instance.upgrade().unwrap();
                return instance.call(&table, &[&origin_table, &key]);
            }
            if table.as_table().is_none() {
                return Ok(RawValue::Nil);
            }
            depth += 1;
        }
    }
    pub(crate) fn table_set(
        &self,
        table: &RawValue,
        key: &RawValue,
        value: &RawValue,
    ) -> Result<(), RuntimeError> {
        self.table_set_impl(table, key, value, 0)
    }
    pub(crate) fn table_set_impl(
        &self,
        table: &RawValue,
        key: &RawValue,
        value: &RawValue,
        depth: u32,
    ) -> Result<(), RuntimeError> {
        if let Some(table) = table.as_table() {
            let mut table = table.borrow_mut();
            if !table.get(key).is_nil() {
                table.set(key.clone(), value.clone());
                return Ok(());
            }
        }
        let mt = self.get_metatable(table);
        if !mt.is_nil() {
            let newindex = self.table_get_impl(
                mt,
                &self.global_state.constants[ConstantsIndex::MtKeyNewIndex as usize].get(),
                depth + 1,
            )?;
            if newindex.as_table().is_some() {
                return self.table_set_impl(&newindex, key, value, depth + 1);
            }
            if newindex.as_callable().is_some() || newindex.as_closure().is_some() {
                let instance = self.instance.upgrade().unwrap();
                instance.call(&newindex, &[&table, &key, &value])?;
                return Ok(());
            }
        }
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
        table.set(key.clone(), value.clone());
        Ok(())
    }

    pub(crate) fn create_userdata<T: UserData + Traceable>(&self, userdata: T) -> RawValue {
        let p: Box<dyn UserData> = Box::new(userdata);
        let s = self.gc.allocate(p);
        RawValue::UserData(s)
    }

    pub(crate) fn create_string(&self, s: String) -> RawValue {
        let s = self.gc.allocate(Managed::new(s));
        RawValue::String(s)
    }
    pub(crate) fn create_table(&self, t: Table) -> RawValue {
        let t = self.gc.allocate(RefCell::new(t));
        RawValue::Table(t)
    }
    pub(crate) fn create_closure(&self, c: Closure) -> RawValue {
        let c = self.gc.allocate(c);
        RawValue::Closure(c)
    }
    pub(crate) fn concat(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        match (a, b) {
            (RawValue::String(a), RawValue::String(b)) => {
                let mut a = a.data.clone();
                a.push_str(&b.data);
                Ok(self.create_string(a))
            }
            (RawValue::String(a), RawValue::Number(b)) => {
                let mut a = a.data.clone();
                a.push_str(&b.to_string());
                Ok(self.create_string(a))
            }
            _ => {
                let mt_a = self.get_metatable(&a);
                let mt_b = self.get_metatable(&b);
                if !mt_a.is_nil() {
                    let method = self.table_get(
                        &mt_a,
                        &self.global_state.constants[ConstantsIndex::MtKeyConcat as usize].get(),
                    )?;
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else if !mt_b.is_nil() {
                    let method = self.table_get(
                        &mt_b,
                        &self.global_state.constants[ConstantsIndex::MtKeyConcat as usize].get(),
                    )?;
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: format!(
                            "attempt to perform concat {} and {}",
                            a.type_of(),
                            b.type_of()
                        ),
                    })
                }
            }
        }
    }
    pub(crate) fn add(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl!(self, +, a,b, MtKeyAdd)
    }
    pub(crate) fn sub(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl!(self, -, a,b, MtKeySub)
    }
    pub(crate) fn mul(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl!(self, *, a,b, MtKeyMul)
    }
    pub(crate) fn div(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl!(self,/, a,b, MtKeyDiv)
    }
    pub(crate) fn mod_(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl!(self,%, a,b,MtKeyMod)
    }
    pub(crate) fn pow(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        binary_op_impl_closue!(
            self,
            |a: f64, b: f64| -> f64 { a.powf(b) },
            "^",
            a,
            b,
            MtKeyPow
        )
    }
    pub(crate) fn bitwise_and(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        int_binary_op_impl!(&, a, b)
    }
    pub(crate) fn bitwise_or(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        int_binary_op_impl!(|, a, b)
    }

    // pub(crate) fn cmp(&self, a: Value, b: Value) -> Result<Ordering, RuntimeError> {
    //     let res = match (a, b) {
    //         (Value::Nil, Value::Nil) => Ok(Ordering::Equal),
    //         (Value::Bool(a), Value::Bool(b)) => Ok(a.cmp(&b)),
    //         (Value::Number(a), Value::Number(b)) => Ok(a.cmp(&b)),
    //         (Value::String(a), Value::String(b)) => {
    //             let a = &(*a).data;
    //             let b = &(*b).data;
    //             Ok(a.cmp(b))
    //         }
    //         // (Value::Table(a), Value::Table(b))=>{
    //         //     if a == b {
    //         //         Ok(Ordering::Equal)
    //         //     }
    //         // }
    //         _ => Err(RuntimeError {
    //             kind: ErrorKind::ArithmeticError,
    //             msg: format!(
    //                 "attempt to perform compare {} with {}",
    //                 a.type_of(),
    //                 b.type_of()
    //             ),
    //         }),
    //     };
    //     #[cfg(debug_assertions)]
    //     {
    //         match res {
    //             Ok(ordering) => {
    //                 if ordering == Ordering::Equal {
    //                     debug_assert!(a == b);
    //                 } else {
    //                     debug_assert!(a != b);
    //                 }
    //             }
    //             _ => {}
    //         }
    //     }
    //     res
    // }

    pub(crate) fn lt(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        match (a, b) {
            (RawValue::Number(a), RawValue::Number(b)) => Ok(RawValue::from_bool(a < b)),
            (RawValue::String(a), RawValue::String(b)) => {
                let a = &(*a).data;
                let b = &(*b).data;
                Ok(RawValue::from_bool(a < b))
            }
            _ => {
                let mt_a = self.get_metatable(&a);
                let mt_b = self.get_metatable(&b);
                macro_rules! error {
                    () => {
                        Err(RuntimeError {
                            kind: ErrorKind::ArithmeticError,
                            msg: format!(
                                "attempt to perform compare {} with {}",
                                a.type_of(),
                                b.type_of()
                            ),
                        })
                    };
                }
                if !mt_a.is_nil() {
                    let method = self.table_get(
                        &mt_a,
                        &self.global_state.constants[ConstantsIndex::MtKeyLt as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else if !mt_b.is_nil() {
                    let method = self.table_get(
                        &mt_b,
                        &self.global_state.constants[ConstantsIndex::MtKeyLt as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: format!(
                            "attempt to perform compare {} with {}",
                            a.type_of(),
                            b.type_of()
                        ),
                    })
                }
            }
        }
    }
    pub(crate) fn le(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        match (a, b) {
            (RawValue::Number(a), RawValue::Number(b)) => Ok(RawValue::from_bool(a <= b)),
            (RawValue::String(a), RawValue::String(b)) => {
                let a = &(*a).data;
                let b = &(*b).data;
                Ok(RawValue::from_bool(a <= b))
            }
            _ => {
                macro_rules! error {
                    () => {
                        Err(RuntimeError {
                            kind: ErrorKind::ArithmeticError,
                            msg: format!(
                                "attempt to perform compare {} with {}",
                                a.type_of(),
                                b.type_of()
                            ),
                        })
                    };
                }
                let mt_a = self.get_metatable(&a);
                let mt_b = self.get_metatable(&b);
                if !mt_a.is_nil() {
                    let method = self.table_get(
                        &mt_a,
                        &self.global_state.constants[ConstantsIndex::MtKeyLe as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else if !mt_b.is_nil() {
                    let method = self.table_get(
                        &mt_b,
                        &self.global_state.constants[ConstantsIndex::MtKeyLe as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: format!(
                            "attempt to perform compare {} with {}",
                            a.type_of(),
                            b.type_of()
                        ),
                    })
                }
            }
        }
    }
    pub(crate) fn gt(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        self.lt(b, a)
    }
    pub(crate) fn ge(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        self.le(b, a)
    }
    pub(crate) fn eq(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        match (a, b) {
            (RawValue::Nil, RawValue::Nil) => Ok(RawValue::from_bool(true)),
            (RawValue::Bool(a), RawValue::Bool(b)) => Ok(RawValue::from_bool(a == b)),
            (RawValue::Number(a), RawValue::Number(b)) => Ok(RawValue::from_bool(a == b)),
            (RawValue::String(a), RawValue::String(b)) => {
                let a = &(*a).data;
                let b = &(*b).data;
                Ok(RawValue::from_bool(a == b))
            }
            (RawValue::Closure(a), RawValue::Closure(b)) => Ok(RawValue::from_bool(a == b)),
            (RawValue::Callable(a), RawValue::Callable(b)) => Ok(RawValue::from_bool(a == b)),
            (RawValue::Table(a), RawValue::Table(b)) if a == b => Ok(RawValue::from_bool(true)),
            _ => {
                let mt_a = self.get_metatable(&a);
                let mt_b = self.get_metatable(&b);
                macro_rules! error {
                    () => {
                        Ok(RawValue::from_bool(false))
                    };
                }
                if !mt_a.is_nil() {
                    let method = self.table_get(
                        &mt_a,
                        &self.global_state.constants[ConstantsIndex::MtKeyEq as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else if !mt_b.is_nil() {
                    let method = self.table_get(
                        &mt_b,
                        &self.global_state.constants[ConstantsIndex::MtKeyEq as usize].get(),
                    )?;
                    if method.is_nil() {
                        return error!();
                    }
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a, &b]);
                } else {
                    Ok(RawValue::from_bool(false))
                }
            }
        }
    }
    pub(crate) fn ne(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        Ok(RawValue::from_bool(!self.eq(a, b)?.to_bool()))
    }
    pub(crate) fn idiv(&self, a: &RawValue, b: &RawValue) -> Result<RawValue, RuntimeError> {
        if let (Some(a), Some(b)) = (a.number(), b.number()) {
            if b == 0.0 {
                return Err(RuntimeError {
                    kind: ErrorKind::ArithmeticError,
                    msg: "attempt to divide by zero".into(),
                });
            }
            Ok(RawValue::from_number((a / b).floor()))
        } else {
            macro_rules! error {
                () => {
                    Err(RuntimeError {
                        kind: ErrorKind::ArithmeticError,
                        msg: format!(
                            "attempt to perform arithmetic operation '//' between {} and {}",
                            a.type_of(),
                            b.type_of()
                        ),
                    })
                };
            }
            let mt_a = self.get_metatable(&a);
            let mt_b = self.get_metatable(&b);
            if !mt_a.is_nil() {
                let method = self.table_get(
                    &mt_a,
                    &self.global_state.constants[ConstantsIndex::MtKeyIDiv as usize].get(),
                )?;
                if method.is_nil() {
                    return error!();
                }
                let instance = self.instance.upgrade().unwrap();
                return instance.call(&method, &[a, &b]);
            } else if !mt_b.is_nil() {
                let method = self.table_get(
                    &mt_b,
                    &self.global_state.constants[ConstantsIndex::MtKeyIDiv as usize].get(),
                )?;
                if method.is_nil() {
                    return error!();
                }
                let instance = self.instance.upgrade().unwrap();
                return instance.call(&method, &[a, &b]);
            } else {
                error!()
            }
        }
    }
    pub(crate) fn len(&self, a: &RawValue) -> Result<RawValue, RuntimeError> {
        match a {
            // Value::Nil => todo!(),
            // Value::Bool(_) => todo!(),
            // Value::Number(_) => todo!(),
            RawValue::String(x) => Ok(RawValue::from_number((*x).data.len() as f64)),
            // Value::Closure(_) => todo!(),
            // Value::Callable(_) => todo!(),
            RawValue::Tuple(x) => Ok(RawValue::from_number((*x).values.borrow().len() as f64)),
            _ => {
                let mt = self.get_metatable(&a);
                if mt.is_nil() {
                    match a {
                        RawValue::Table(x) => {
                            Ok(RawValue::from_number((*x).borrow_mut().len() as f64))
                        }
                        _ => Err(RuntimeError {
                            kind: ErrorKind::ArithmeticError,
                            msg: format!(" attempt to get length of a {} value", a.type_of(),),
                        }),
                    }
                } else {
                    let method = self.table_get(
                        &mt,
                        &self.global_state.constants[ConstantsIndex::MtKeyLen as usize].get(),
                    )?;
                    if method.is_nil() {
                        Err(RuntimeError {
                            kind: ErrorKind::ArithmeticError,
                            msg: format!(" attempt to get length of a {} value", a.type_of(),),
                        })
                    } else {
                        let instance = self.instance.upgrade().unwrap();
                        return instance.call(&method, &[a]);
                    }
                }
            }
        }
    }
    pub(crate) fn not(&self, a: &RawValue) -> Result<RawValue, RuntimeError> {
        Ok(RawValue::from_bool(!a.to_bool()))
    }
    pub(crate) fn bitwise_not(&self, a: &RawValue) -> Result<RawValue, RuntimeError> {
        match a {
            // Value::Nil => todo!(),
            // Value::Bool(_) => todo!(),
            RawValue::Number(_) => {
                let i = a.as_i64();
                if let Some(i) = i {
                    Ok(RawValue::from_number((!i) as f64))
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
    pub(crate) fn neg(&self, a: &RawValue) -> Result<RawValue, RuntimeError> {
        match a {
            // Value::Nil => todo!(),
            // Value::Bool(_) => todo!(),
            RawValue::Number(x) => Ok(RawValue::from_number(*-x)),
            _ => {
                macro_rules! error {
                    () => {
                        Err(RuntimeError {
                            kind: ErrorKind::ArithmeticError,
                            msg: format!(
                                " attempt to perform unary '-' on a {} value",
                                a.type_of(),
                            ),
                        })
                    };
                }
                let mt = self.get_metatable(&a);
                if mt.is_nil() {
                    return error!();
                }
                let method = self.table_get(
                    &mt,
                    &self.global_state.constants[ConstantsIndex::MtKeyNeg as usize].get(),
                )?;
                if method.is_nil() {
                    return error!();
                } else {
                    let instance = self.instance.upgrade().unwrap();
                    return instance.call(&method, &[a]);
                }
            }
        }
    }
    pub(crate) fn get_metatable(&self, a: &RawValue) -> RawValue {
        self.global_state.get_metatable(a)
    }
}

impl<'b> BaseApi for CallContext<'b> {
    fn create_number<'a>(&'a self, x: f64) -> Value<'a> {
        Value::new(RawValue::from_number(x))
    }

    fn create_bool<'a>(&self, x: bool) -> Value<'a> {
        Value::new(RawValue::from_bool(x))
    }

    fn create_userdata<'a, T: UserData + Traceable>(&self, userdata: T) -> Value<'a> {
        Value::new(self.state.create_userdata(userdata))
    }

    fn create_string<'a>(&self, s: String) -> Value<'a> {
        Value::new(self.state.create_string(s))
    }

    fn upgrade<'a>(&'a self, v: Value<'_>) -> crate::runtime::GcValue {
        let instance = self.state.instance.upgrade().unwrap();
        instance.upgrade(v)
    }

    fn create_closure<'a>(&self, closure: Box<dyn crate::closure::Callable>) -> Value<'a> {
        Value::new(RawValue::Callable(self.state.gc.allocate(closure)))
    }

    fn create_table<'a>(&self) -> Value<'a> {
        Value::new(self.state.create_table(Table::new()))
    }

    fn set_metatable<'a>(&self, v: Value<'a>, mt: Value<'a>) {
        self.state.global_state.set_metatable(&v.value, &mt.value)
    }

    fn get_metatable<'a>(&self, v: Value<'a>) -> Value<'a> {
        Value::new(self.state.global_state.get_metatable(&v.value))
    }

    fn table_rawset<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError> {
        self.instance.table_rawset(table, key, value)
    }

    fn table_rawget<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        self.instance.table_rawget(table, key)
    }

    fn get_global_env<'a>(&self) -> Value<'a> {
        self.instance.get_global_env()
    }
}

impl<'b> StateApi for CallContext<'b> {
    fn table_set<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError> {
        self.state.table_set(&table.value, &key.value, &value.value)
    }

    fn table_get<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        Ok(Value::new(self.state.table_get(&table.value, &key.value)?))
    }
}
impl<'b> CallApi for CallContext<'b> {
    fn arg_count(&self) -> usize {
        self.frame.n_args
    }

    fn arg_or_nil<'a>(&'a self, i: usize) -> Value<'a> {
        if i < self.frame.n_args {
            Value::new(self.frame.locals[i].clone())
            // Some(self.eval_stack.borrow()[frame.frame_bottom + i])
        } else {
            Value::new(RawValue::nil())
        }
    }

    fn arg<'a>(&'a self, i: usize) -> Result<Value<'a>, RuntimeError> {
        if i < self.frame.n_args {
            Ok(Value::new(self.frame.locals[i].clone()))
            // Some(self.eval_stack.borrow()[frame.frame_bottom + i])
        } else {
            Err(RuntimeError {
                kind: ErrorKind::ArgumentArityError,
                msg: format!("arg {} is not supplied", i,),
            })
        }
    }

    fn ret<'a>(&'a self, i: usize, value: Value<'a>) {
        let mut ret_values = self.ret_values.borrow_mut();
        if i >= ret_values.len() {
            ret_values.resize(i + 1, RawValue::nil());
        }
        ret_values[i] = value.value;
    }

    fn call<'a>(&self, closure: Value<'a>, args: &[Value<'a>]) -> Result<Value<'a>, RuntimeError> {
        let args: Vec<_> = args.iter().map(|x| &x.value).collect();
        Ok(Value::new(self.instance.call(&closure.value, &args)?))
    }
}
