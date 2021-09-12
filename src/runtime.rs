use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{Stack, closure::{Callable, NativeFunction}, gc::{Gc, Traceable}, state::{CallContext, State}, table::Table, value::{Managed, ManagedCell, Value, ValueData}, vm::Instance};


#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    TypeError,
    ArithmeticError,
    NameError,
}
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    pub msg: String,
}
// struct Locals{
//     locals:RefCell<Vec<Rc<RefCell<Value>>>>,
// }
pub struct Runtime {
    gc: Rc<Gc>,
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

impl Runtime {
    pub fn add_function<'a, F: Fn(&CallContext<'_>) -> () + 'static>(
        &'a mut self,
        name: String,
        f: F,
    ) {
        self.add_callable(name, Box::new(NativeFunction::new(f)))
    }
    pub fn add_callable<'a>(&'a mut self, name: String, callable: Box<dyn Callable>)
    /*->Option<Local<'a>>*/
    {
        let mut globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(
            Value {
                data: ValueData::String(self.gc.manage(Managed { data: name })),
                metatable: std::ptr::null(),
            },
            Value {
                data: ValueData::Callable(self.gc.manage(Managed { data: callable })),
                metatable: std::ptr::null(),
            },
        );
        // Some(Local{
        //     value:v,
        //     phantom:PhantomData{},
        // })
    }
    pub fn create_instance(&mut self) -> Rc<Instance> {
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
    fn add_std_lib(&mut self) {
        self.add_function("print".into(), |ctx| {
            for i in 0..ctx.get_arg_count() {
                let arg = ctx.arg(i);
                if i > 0 {
                    print!(" ");
                }
                print!("{}", arg.print());
            }
            print!("\n");
        });
        self.add_function("assert".into(), |ctx| {
            let v = ctx.arg(0);
            assert!(v.as_bool());
        });
        self.add_function("type".into(), |ctx| {
            let v = ctx.arg(0);
            ctx.ret(0, ctx.state.create_string(String::from(v.type_of())));
        });
    }
    pub fn new() -> Self {
        let gc = Rc::new(Gc::new());
        let mut runtime = Self {
            gc: gc.clone(),
            globals: Value {
                data: ValueData::Table(gc.manage(ManagedCell {
                    data: RefCell::new(Table::new()),
                })),
                metatable: std::ptr::null(),
            },
            instances: vec![],
        };
        runtime.add_std_lib();
        runtime
    }
}
