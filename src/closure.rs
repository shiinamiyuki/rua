use std::{cell::RefCell, rc::Rc};

use crate::{
    bytecode::ByteCodeModule,
    gc::{Gc, Traceable},
    state::{CallContext},
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
pub struct Closure {
    pub(crate) entry: usize,
    pub(crate) n_locals: usize,
    pub(crate) module: Rc<ByteCodeModule>,
    pub(crate) upvalues: RefCell<Vec<Value>>,
}
impl Traceable for Closure {
    fn trace(&self, gc: &Gc) {
        todo!()
    }
}
