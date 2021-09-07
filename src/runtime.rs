use std::{collections::HashMap, rc::Rc};

use crate::{gc::Gc, state::State, value::Value, vm::Instance};

pub(crate) type Globals = HashMap<String, Value>;

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    TypeError,
    ArithmeticError,
}
pub struct RuntimeError {
    pub kind: ErrorKind,
}
pub struct Runtime {
    gc: Rc<Gc>,
    globals: Rc<Globals>,
    instances: Vec<Rc<Instance>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            gc: Rc::new(Gc::new()),
            globals: Rc::new(Globals::new()),
            instances: vec![],
        }
    }
}
