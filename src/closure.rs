use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bytecode::ByteCodeModule,
    gc::{Gc, Traceable},
    state::CallContext,
    value::Value,
};

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

pub(crate) struct UpValue {
    pub(crate) values: RefCell<HashMap<u32, Value>>,
    pub(crate) parent: *const UpValue,
}
impl UpValue {
    pub(crate) fn get(&self, i: u32) -> Value {
        let mut p = self as *const UpValue;
        unsafe {
            while let Some(uv) = p.as_ref() {
                let values = uv.values.borrow();
                if let Some(v) = values.get(&i) {
                    return *v;
                } else {
                    p = uv.parent;
                }
            }
            panic!("upvalue index {} does not exist", i)
        }
    }
    pub(crate) fn set(&self, i: u32, v: Value) {
        let mut p = self as *const UpValue;
        unsafe {
            while let Some(uv) = p.as_ref() {
                let mut values = uv.values.borrow_mut();
                if let Some(u) = values.get_mut(&i) {
                    *u = v;
                } else {
                    p = uv.parent;
                }
            }
            panic!("upvalue index {} does not exist", i)
        }
    }
}
impl Traceable for UpValue {
    fn trace(&self, gc: &Gc) {
        let values = self.values.borrow();
        for v in values.values() {
            gc.trace(v);
        }
    }
}
#[derive(Clone, Debug)]
pub struct ClosurePrototype {
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    // pub(crate) n_locals: usize,
    pub(crate) upvalues: Vec<u32>,
}
pub struct Closure {
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    // pub(crate) n_locals: usize,
    pub(crate) module: Rc<ByteCodeModule>,
    pub(crate) upvalues: *const UpValue,
}
impl Closure {
    const UPVALUE_ENV: usize = 0;
    pub(crate) fn set_upvalue(&self, i: u32, value: Value) {
        unsafe { self.upvalues.as_ref().unwrap().set(i, value) }
    }
    pub(crate) fn get_upvalue(&self, i: u32) -> Value {
        unsafe { self.upvalues.as_ref().unwrap().get(i) }
    }
}
impl Traceable for Closure {
    fn trace(&self, gc: &Gc) {
        gc.trace_ptr(self.upvalues);
    }
}
