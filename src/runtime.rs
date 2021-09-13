use std::{
    any::TypeId,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    marker::PhantomData,
    mem::size_of,
    rc::Rc,
};

use crate::{
    closure::{Callable, NativeFunction},
    gc::{GcState, Traceable},
    state::{CallContext, State},
    stdlib,
    table::Table,
    value::{Managed, ManagedCell, UserData, Value, ValueData},
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

struct RuntimeInner {
    gc: Rc<GcState>,
    globals: Value,
    instances: Vec<Rc<Instance>>,
}
pub struct ValueRef<'a> {
    pub(crate) value: Value,
    pub(crate) phantom: PhantomData<&'a u32>,
}
fn dummy_convert_ref<T: 'static, U: 'static>(x: &T) -> &U {
    if TypeId::of::<T>() == TypeId::of::<U>() {
        unsafe { std::mem::transmute(x) }
    } else {
        unreachable!()
    }
}
fn dummy_convert<T: 'static + Sized, U: 'static + Sized>(x: T) -> U {
    if TypeId::of::<T>() == TypeId::of::<U>() {
        unsafe {
            debug_assert!(size_of::<T>() == size_of::<U>());
            let y = std::mem::transmute_copy(&x);
            std::mem::forget(x);
            y
        }
    } else {
        unreachable!()
    }
}
macro_rules! arg_f64 {
    ($self:expr) => {{
        match $self.as_f64() {
            Some(x) => Ok(x),
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("expected  type 'number' but is {}", $self.type_of()),
                })
            }
        }
    }};
}
macro_rules! arg_user {
    ($self:expr,$t:ty) => {{
        match $self.as_userdata() {
            Some(userdata) => {
                let any = userdata.as_any();
                if let Some(x) = any.downcast_ref::<$t>() {
                    Ok(x)
                } else {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!("expected type 'userdata' but is {}", userdata.type_name()),
                    });
                }
            }
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!(
                        "expected 'userdata' {} but is {}",
                        std::any::type_name::<T>(),
                        $self.type_of()
                    ),
                })
            }
        }
    }};
}
macro_rules! arg_string {
    ($self:expr) => {{
        match $self.as_string() {
            Some(x) => Ok(x),
            None => {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("expected 'string' but is {}", $self.type_of()),
                })
            }
        }
    }};
}

impl<'a> ValueRef<'a> {
    pub fn to_number(&'a self) -> Option<f64> {
        self.value.number()
    }
    pub fn as_f64(&'a self) -> Option<&'a f64> {
        self.value.as_f64()
    }
    pub fn as_bool(&'a self) -> Option<&'a bool> {
        self.value.as_bool()
    }
    pub fn as_string(&'a self) -> Option<&'a String> {
        self.value.as_string()
    }
    pub fn as_userdata(&'a self) -> Option<&'a dyn UserData> {
        self.value.as_userdata()
    }
    pub fn to_bool(&self) -> bool {
        self.value.to_bool()
    }
    pub fn type_of(&self) -> &'static str {
        self.value.type_of()
    }
    pub fn cast<T: 'static>(&self) -> Result<T, RuntimeError> {
        if TypeId::of::<f64>() == TypeId::of::<T>() {
            let x = match self.to_number() {
                Some(x) => Ok(x),
                None => Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("expected  type 'number' but is {}", self.type_of()),
                }),
            }?;
            Ok(dummy_convert::<f64, T>(x))
        } else if TypeId::of::<String>() == TypeId::of::<T>() {
            let x = match self.as_string() {
                Some(x) => Ok(x.clone()),
                None => Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("expected  type 'string' but is {}", self.type_of()),
                }),
            }?;
            Ok(dummy_convert::<String, T>(x))
        } else {
            panic!("attempt to cast a userdata, use cast_ref instead!");
        }
    }
    pub fn cast_ref<T: 'static>(&self) -> Result<&T, RuntimeError> {
        if TypeId::of::<f64>() == TypeId::of::<T>() {
            let x = arg_f64!(self)?;
            Ok(dummy_convert_ref::<f64, T>(x))
        } else if TypeId::of::<String>() == TypeId::of::<T>() {
            let x = arg_string!(self)?;
            Ok(dummy_convert_ref::<String, T>(x))
        } else {
            let x = arg_user!(self, T)?;
            Ok(dummy_convert_ref::<T, T>(x))
        }
    }
}

struct ValueBox {
    next: Cell<*const ValueBox>,
    prev: Cell<*const ValueBox>,
    value: RefCell<Value>,
}
/*
Safe wrapper for Value
Local are gc roots
*/
pub struct RootValue {
    inner: Box<ValueBox>,
}
pub struct RootBorrowMut<'a> {
    inner: &'a ValueBox,
    ref_: RefMut<'a, Value>,
    value: ValueRef<'a>,
}

pub struct RootBorrow<'a> {
    inner: &'a ValueBox,
    ref_: Ref<'a, Value>,
    value: ValueRef<'a>,
}
impl RootValue {
    pub fn borrow_mut<'a>(&'a self) -> RootBorrowMut<'a> {
        let b = self.inner.value.borrow_mut();
        RootBorrowMut {
            inner: self.inner.as_ref(),
            value: ValueRef {
                value: *b,
                phantom: PhantomData {},
            },
            ref_: b,
        }
    }
    fn borrow<'a>(&'a self) -> RootBorrow<'a> {
        let b = self.inner.value.borrow();
        RootBorrow {
            inner: self.inner.as_ref(),
            value: ValueRef {
                value: *b,
                phantom: PhantomData {},
            },
            ref_: b,
        }
    }
}
impl<'a> Drop for RootBorrowMut<'a> {
    fn drop(&mut self) {
        *self.ref_ = self.value.value;
    }
}

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
                print!("{}", arg.value.print());
            }
            print!("\n");
            Ok(())
        });
        self.add_function("assert".into(), |ctx| {
            let v = ctx.arg(0).unwrap();
            assert!(v.value.to_bool());
            Ok(())
        });
        self.add_function("type".into(), |ctx| {
            let v = ctx.arg(0).unwrap();
            ctx.ret(0, ctx.create_string(String::from(v.type_of())));
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
