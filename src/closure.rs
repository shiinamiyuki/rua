use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{bytecode::ByteCodeModule, compile::UpValueInfo, gc::{Gc, Traceable}, state::CallContext, value::Value};

pub trait Callable {
    fn call<'a>(&self, ctx: &CallContext<'a>);
}
pub struct NativeFunction {
    func: Box<dyn Fn(&CallContext<'_>) -> ()>,
}
impl NativeFunction {
    pub fn new<F: Fn(&CallContext<'_>) -> () + 'static>(f: F) -> Self {
        Self { func: Box::new(f) }
    }
}
impl Callable for NativeFunction {
    fn call<'a>(&self, ctx: &CallContext<'_>) {
        (self.func)(ctx)
    }
}

#[derive(Clone)]
pub(crate) enum UpValueInner {
    Open(*mut Value),
    Closed(RefCell<Value>),
}

#[derive(Clone)]
pub(crate) struct UpValue {
    pub(crate) inner: Rc<UpValueInner>, // Rc or Gc?
}

#[derive(Clone, Debug)]
pub struct ClosurePrototype {
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    // pub(crate) n_locals: usize,
    pub(crate) upvalues: Vec<UpValueInfo>,
}
pub struct Closure {
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    // pub(crate) n_locals: usize,
    pub(crate) module: Rc<ByteCodeModule>,
    pub(crate) upvalues: Vec<UpValue>,
    // pub(crate) upvalues: HashMap<u32, UpValue>,
}
impl Closure {
    pub(crate) fn set_upvalue(&self, i: u32, value: Value) {
        unsafe {
            match *self.upvalues.get(&i).unwrap().inner {
                UpValueInner::Open(p) => {
                    *p.as_mut().unwrap() = value;
                }
                UpValueInner::Closed(p) => {
                    let mut v = p.borrow_mut();
                    *v = value;
                }
            }
        }
    }
    // pub(crate) fn insert_upvalue(&self, i: u32, value: Value) {
    //     unsafe {
    //         if let Some(p) = self.upvalues.as_ref() {
    //             p.insert(i, value)
    //         } else {
    //             unreachable!()
    //         }
    //     }
    // }
    pub(crate) fn get_upvalue(&self, i: u32) -> Value {
        unsafe {
            match *self.upvalues.get(&i).unwrap().inner {
                UpValueInner::Open(p) => *p.as_ref().unwrap(),
                UpValueInner::Closed(p) => {
                    let v = p.borrow();
                    *v
                }
            }
        }
    }
}
impl Traceable for Closure {
    fn trace(&self, gc: &Gc) {
        for (i, v) in &self.upvalues {
            match *v.inner {
                UpValueInner::Open(p) => gc.trace_ptr(p),
                UpValueInner::Closed(p) => {
                    let v = p.borrow();
                    gc.trace(&*v);
                }
            }
        }
    }
}
