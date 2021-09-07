use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{gc::Gc, state::State, value::Value, vm::Instance};

pub(crate) type Globals = RefCell<HashMap<String, Value>>;

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    TypeError,
    ArithmeticError,
}
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    pub msg: String,
}
pub struct Runtime {
    gc: Rc<Gc>,
    globals: Rc<Globals>,
    instances: Vec<Rc<Instance>>,
}

impl Runtime {
    pub fn create_instance(&mut self) -> Rc<Instance> {
        let instance = Rc::new(Instance {
            gc: self.gc.clone(),
            state: State {
                gc: self.gc.clone(),
                globals: self.globals.clone(),
                frames: RefCell::new(vec![]),
                eval_stack: RefCell::new(vec![]),
            },
        });
        self.instances.push(instance.clone());
        instance
    }
    pub fn new() -> Self {
        Self {
            gc: Rc::new(Gc::new()),
            globals: Rc::new(Globals::new(HashMap::new())),
            instances: vec![],
        }
    }
}
