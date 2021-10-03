use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{CloneCell, bytecode::ByteCodeModule, compile::UpValueInfo, debug_println, gc::{GcState, Traceable}, runtime::RuntimeError, state::CallContext, value::RawValue};

pub trait Callable: Traceable {
    fn call<'a>(&self, ctx: &CallContext<'a>) -> Result<(), RuntimeError>;
}
impl Traceable for Box<dyn Callable> {
    fn trace(&self, gc: &GcState) {
        self.as_ref().trace(gc)
    }
}
pub struct NativeFunction {
    func: Box<dyn Fn(&CallContext<'_>) -> Result<(), RuntimeError>>,
}
impl NativeFunction {
    pub fn new<F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(f: F) -> Self {
        Self { func: Box::new(f) }
    }
}
impl Traceable for NativeFunction {
    fn trace(&self, _gc: &GcState) {}
}
impl Callable for NativeFunction {
    fn call<'a>(&self, ctx: &CallContext<'_>) -> Result<(), RuntimeError> {
        (self.func)(ctx)
    }
}

#[derive(Clone)]
pub(crate) enum UpValueInner {
    Empty,
    Open(*mut RawValue),
    Closed(RawValue),
}

#[derive(Clone)]
pub(crate) struct UpValue {
    pub(crate) inner: RefCell<Rc<CloneCell<UpValueInner>>>, // Rc or Gc?
}

impl std::fmt::Display for UpValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner.borrow().get() {
            UpValueInner::Empty => unreachable!(),
            UpValueInner::Open(p) => unsafe { write!(f, "UpValue::Open({})", (*p).print()) },
            UpValueInner::Closed(v) => write!(f, "UpValue::Closed({})", v.print()),
        }
    }
}
#[derive(Clone, Debug)]
pub struct ClosurePrototype {
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    pub(crate) has_varargs: bool,
    // pub(crate) n_locals: usize,
    pub(crate) upvalues: Vec<UpValueInfo>,
}
pub struct Closure {
    pub(crate) proto_idx: usize,
    pub(crate) called: Cell<bool>,
    pub(crate) entry: usize,
    pub(crate) n_args: usize,
    pub(crate) has_varargs: bool,
    // pub(crate) n_locals: usize,
    pub(crate) module: Rc<ByteCodeModule>,
    pub(crate) upvalues: Vec<UpValue>,
    // pub(crate) upvalues: HashMap<u32, UpValue>,
}
impl Closure {
    pub(crate) fn set_upvalue(&self, i: u32, value: RawValue) {
        unsafe {
            debug_println!(
                "store upvalue {} {:?}, {}",
                i,
                Rc::as_ptr(&self.upvalues[i as usize].inner.borrow()),
                self.upvalues[i as usize]
            );
            let v = self.upvalues[i as usize].inner.borrow();
            match (**v).get() {
                UpValueInner::Open(p) => {
                    *p.as_mut().unwrap() = value;
                }
                UpValueInner::Closed(_) => (**v).set(UpValueInner::Closed(value)),
                UpValueInner::Empty => {
                    unreachable!()
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
    pub(crate) fn get_upvalue(&self, i: u32) -> RawValue {
        unsafe {
            // let v = (*self.upvalues[i as usize].inner).borrow();
            debug_println!(
                "load upvalue {} {:?}, {}",
                i,
                Rc::as_ptr(&self.upvalues[i as usize].inner.borrow()),
                self.upvalues[i as usize]
            );
            let v = self.upvalues[i as usize].inner.borrow();

            match (**v).get() {
                UpValueInner::Open(p) => (*p).clone(),
                UpValueInner::Closed(c) => c,
                UpValueInner::Empty => {
                    unreachable!()
                }
            }
        }
    }
}
impl Traceable for Closure {
    fn trace(&self, gc: &GcState) {
        for v in &self.upvalues {
            let v = v.inner.borrow();
            match (**v).get() {
                UpValueInner::Open(p) => gc.trace(unsafe { &*p }),
                UpValueInner::Closed(v) => {
                    gc.trace(&v);
                }
                UpValueInner::Empty => {}
            }
        }
    }
}
