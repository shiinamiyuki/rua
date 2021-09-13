use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    closure::{Callable, NativeFunction},
    gc::{GcState, Traceable},
    state::{CallContext, State},
    stdlib,
    table::Table,
    value::{Managed, ManagedCell, Value, ValueData},
    vm::Instance,
    Stack,
};

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    TypeError,
    ArithmeticError,
    NameError,
    ExternalError,
    ArgumentArityError,
}
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    pub msg: String,
}
// struct Locals{
//     locals:RefCell<Vec<Rc<RefCell<Value>>>>,
// }
struct RuntimeInner {
    gc: Rc<GcState>,
    globals: Value,
    instances: Vec<Rc<Instance>>,
    // local_handles:Rc<Locals>,
}

/*
Safe wrapper for Value
Local are gc roots
*/
// pub struct Local<'a>{
//     value:Rc<RefCell<Value>>,
//     local_handles:Rc<Locals>,
//     phantom:PhantomData<&'a u32>,
// }
// impl<'a>Local<'a>{
//     pub fn borrow<'a>(&'a self)->Ref<'a,Value>{
//         self.value.borrow()
//     }
// }

pub struct Module {
    runtime: Rc<RefCell<RuntimeInner>>,
    module: Value,
}
impl Module {
    pub fn function<'a, F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(
        &'a mut self,
        name: String,
        f: F,
    ) -> &mut Module {
        {
            let gc = self.runtime.borrow().gc.clone();
            let mut table = self.module.as_table().unwrap().borrow_mut();
            table.set(
                Value {
                    data: ValueData::String(gc.allocate(Managed::new(name))),
                    metatable: None,
                },
                Value {
                    data: ValueData::Callable(gc.allocate(Box::new(NativeFunction::new(f)))),
                    metatable: None,
                },
            );
        }
        self
    }
    pub fn submodule(&mut self, name: String, module: Module) -> &mut Module {
        {
            let gc = self.runtime.borrow().gc.clone();
            let mut table = self.module.as_table().unwrap().borrow_mut();
            table.set(
                Value {
                    data: ValueData::String(gc.allocate(Managed::new(name))),
                    metatable: None,
                },
                module.module,
            );
        }
        self
    }
}
#[derive(Clone)]
pub struct Runtime {
    inner: Rc<RefCell<RuntimeInner>>,
}
impl Runtime {
    pub fn new() -> Self {
        let r = Self {
            inner: Rc::new(RefCell::new(RuntimeInner::new())),
        };
        {
            let mut inner = r.inner.borrow_mut();
            inner.add_std_lib(r.inner.clone());
        }
        stdlib::add_math_lib(&r);
        r
    }
    pub fn create_instance(&self) -> Rc<Instance> {
        self.inner.borrow_mut().create_instance()
    }
    pub fn create_module(&self) -> Module {
        let gc = self.inner.borrow().gc.clone();
        Module {
            runtime: self.inner.clone(),
            module: Value {
                data: ValueData::Table(gc.allocate(RefCell::new(Table::new()))),
                metatable: None,
            },
        }
    }
    pub fn add_module(&self, name: String, module: Module) {
        self.inner.borrow_mut().add_module(name, module)
    }
}

impl RuntimeInner {
    pub fn add_module(&mut self, name: String, module: Module) {
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(
            Value {
                data: ValueData::String(self.gc.allocate(Managed { data: name })),
                metatable: None,
            },
            module.module,
        );
    }
    fn add_function<'a, F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(
        &'a mut self,
        name: String,
        f: F,
    ) {
        self.add_callable(name, Box::new(NativeFunction::new(f)))
    }
    fn add_callable<'a>(&'a mut self, name: String, callable: Box<dyn Callable>)
    /*->Option<Local<'a>>*/
    {
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(
            Value {
                data: ValueData::String(self.gc.allocate(Managed { data: name })),
                metatable: None,
            },
            Value {
                data: ValueData::Callable(self.gc.allocate(callable)),
                metatable: None,
            },
        );
        // Some(Local{
        //     value:v,
        //     phantom:PhantomData{},
        // })
    }
    fn create_instance(&mut self) -> Rc<Instance> {
        let instance = Rc::new(Instance {
            gc: self.gc.clone(),
            state: State {
                gc: self.gc.clone(),
                globals: self.globals,
                frames: RefCell::new(Stack::new()),
                eval_stack: RefCell::new(vec![]),
            },
        });
        self.instances.push(instance.clone());
        instance
    }
    fn add_std_lib(&mut self, pself: Rc<RefCell<RuntimeInner>>) {
        self.add_function("print".into(), |ctx| {
            for i in 0..ctx.get_arg_count() {
                let arg = ctx.arg(i).unwrap();
                if i > 0 {
                    print!(" ");
                }
                print!("{}", arg.print());
            }
            print!("\n");
            Ok(())
        });
        self.add_function("assert".into(), |ctx| {
            let v = ctx.arg(0).unwrap();
            assert!(v.to_bool());
            Ok(())
        });
        self.add_function("type".into(), |ctx| {
            let v = ctx.arg(0).unwrap();
            ctx.ret(0, ctx.state.create_string(String::from(v.type_of())));
            Ok(())
        });
        // let pself = self as *mut RuntimeInner;
        let gc = self.gc.clone();
        self.add_function("collectgarbage".into(), move |_ctx| {
            let self_ = pself.borrow();
            gc.start_trace();
            gc.trace(&*self_);
            gc.end_trace();
            gc.collect();
            Ok(())
        });
    }
    fn new() -> Self {
        let gc = Rc::new(GcState::new());
        let runtime = Self {
            gc: gc.clone(),
            globals: Value {
                data: ValueData::Table(gc.allocate(RefCell::new(Table::new()))),
                metatable: None,
            },
            instances: vec![],
        };
        runtime
    }
}
impl Traceable for RuntimeInner {
    fn trace(&self, gc: &GcState) {
        gc.trace(&self.globals);
        for instance in &self.instances {
            gc.trace(instance.as_ref());
        }
    }
}
