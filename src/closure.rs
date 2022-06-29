use std::{
    cell::{Cell, RefCell},
    sync::Arc,
};

use parking_lot::RwLock;

use crate::{
    debug_println,
    gc::{Heap, TraceContext, Traceable},
    value::Value,
    vm::ByteCodeModule,
};

/*
WARNING: This is unsafe
*/
#[derive(Clone, Copy)]
pub(crate) enum UpValueInner {
    Empty,
    Open(*mut Value),
    Closed(Value),
}
unsafe impl Send for UpValueInner {}
unsafe impl Sync for UpValueInner {}

#[derive(Clone)]
pub(crate) struct UpValue {
    pub(crate) inner: RefCell<Arc<Cell<UpValueInner>>>,
}
unsafe impl Send for UpValue {}
unsafe impl Sync for UpValue {}
pub(crate) struct Closure {
    pub(crate) entry: usize,
    pub(crate) stack_frame_size: usize,
    pub(crate) module: Arc<ByteCodeModule>,
    pub(crate) upvalues: Vec<UpValue>,
    pub(crate) is_method: bool,
}
impl Closure {
    pub(crate) fn close(&self, i: usize) {
        let v = &self.upvalues[i];
        let u = v.inner.borrow().get();
        match u {
            UpValueInner::Open(p) => {
                v.inner.borrow().set(UpValueInner::Closed(unsafe { *p }));
                *v.inner.borrow_mut() = Arc::new(Cell::new(UpValueInner::Open(p)));
                //Arc::new(Cell::new(UpValueInner::Empty));
            }
            _ => {}
        }
    }
}
unsafe impl Traceable for UpValueInner {
    fn trace(&self, ctx: &mut TraceContext) {
        match self {
            UpValueInner::Empty => todo!(),
            UpValueInner::Open(p) => unsafe { ctx.trace(&**p) },
            UpValueInner::Closed(v) => ctx.trace(v),
        }
    }
}
unsafe impl Traceable for Closure {
    fn trace(&self, ctx: &mut TraceContext) {
        for i in &self.upvalues {}
    }
}
