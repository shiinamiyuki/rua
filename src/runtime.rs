use std::{
    any::TypeId,
    cell::{Cell, Ref, RefCell, RefMut, UnsafeCell},
    collections::HashMap,
    marker::PhantomData,
    rc::{Rc, Weak},
};

use ordered_float::OrderedFloat;

use crate::{
    bytecode::ByteCodeModule,
    closure::{Callable, NativeFunction},
    compile::{compile, CompileError},
    dummy_convert_ref,
    gc::{GcState, Traceable},
    parse::{parse_impl, tokenize},
    state::{CallContext, State},
    stdlib,
    table::Table,
    value::{Managed, UserData, Value},
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
    CompileError,
    KeyError,
}
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    pub msg: String,
}

pub(crate) struct RuntimeInner {
    pub(crate) gc: Rc<GcState>,
    pub(crate) globals: Value,
    pub(crate) constants: Rc<Vec<Value>>,
    pub(crate) instances: Vec<Rc<Instance>>,
    pub(crate) string_pool: HashMap<String, Value>,
}
pub(crate) enum ConstantsIndex {
    MtNumber,
    MtString,
    MtBool,
    MtClosure,
    MtKeyIndex,
    MtKeyAdd,
    MtKeySub,
    MtKeyMul,
    MtKeyDiv,
    MtKeyMod,
    MtKeyEq,
    NumConstants,
}
// pub(crate) const MT_KEY_INDEX:usize=0;
// const MT_KEY_INDEX:usize=0;
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum RustPrimitive {
    Unit,
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
}

impl From<i8> for RustPrimitive {
    fn from(x: i8) -> Self {
        RustPrimitive::I8(x)
    }
}
impl From<u8> for RustPrimitive {
    fn from(x: u8) -> Self {
        RustPrimitive::U8(x)
    }
}
impl From<i16> for RustPrimitive {
    fn from(x: i16) -> Self {
        RustPrimitive::I16(x)
    }
}
impl From<u16> for RustPrimitive {
    fn from(x: u16) -> Self {
        RustPrimitive::U16(x)
    }
}
impl From<i32> for RustPrimitive {
    fn from(x: i32) -> Self {
        RustPrimitive::I32(x)
    }
}
impl From<u32> for RustPrimitive {
    fn from(x: u32) -> Self {
        RustPrimitive::U32(x)
    }
}
impl From<f32> for RustPrimitive {
    fn from(x: f32) -> Self {
        RustPrimitive::F32(OrderedFloat(x))
    }
}
impl From<f64> for RustPrimitive {
    fn from(x: f64) -> Self {
        RustPrimitive::F64(OrderedFloat(x))
    }
}
pub struct ValueRef<'a> {
    pub(crate) value: Value,
    pub(crate) phantom: PhantomData<&'a u32>,
    pub(crate) prim: UnsafeCell<RustPrimitive>,
}
impl<'a> ValueRef<'a> {
    pub(crate) fn new(v: Value) -> Self {
        Self {
            value: v,
            phantom: PhantomData {},
            prim: UnsafeCell::new(RustPrimitive::Unit),
        }
    }
}
macro_rules! impl_from_prim {
    ($t:ty) => {
        impl<'a> From<$t> for ValueRef<'a> {
            fn from(x: $t) -> Self {
                Self::new(Value::from_number(x as f64))
            }
        }
    };
}

impl_from_prim!(i8);
impl_from_prim!(u8);
impl_from_prim!(i16);
impl_from_prim!(u16);
impl_from_prim!(i32);
impl_from_prim!(u32);
impl_from_prim!(f32);
impl_from_prim!(f64);

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
macro_rules! arg_prim {
    ($self:expr, $t:ty) => {
       { let x = arg_f64!($self)?;
        unsafe{
            let prim = &mut *$self.prim.get();
            if *prim != RustPrimitive::Unit {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: "this is an internal limitation, try let another = this_var; another.cast::<T>()".into(),
                });
            }
            *prim = RustPrimitive::from(*x as $t);
        }
        x
    }
    };
}
macro_rules! arg_prim_full {
    ($self:expr, $t:ty,$v:ident) => {
       { let x = arg_f64!($self)?;
        unsafe{
            let prim = &mut *$self.prim.get();
            if *prim != RustPrimitive::Unit {
                return Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: "this is an internal limitation, try let another = this_var; another.cast::<T>()".into(),
                });
            }
            *prim = RustPrimitive::from(*x as $t);
            let prim = & *$self.prim.get();
            let x = match prim {
                RustPrimitive::$v(x)=>x,
                _=>unreachable!(),
            };
            Ok(dummy_convert_ref::<$t, T>(x))
        }
    }
    };
}
impl<'a> ValueRef<'a> {
    pub fn is_nil(&self) -> bool {
        self.value.is_nil()
    }
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
    // pub fn cast<T: 'static>(&self) -> Result<T, RuntimeError> {
    //     if TypeId::of::<f64>() == TypeId::of::<T>() {
    //         let x = match self.to_number() {
    //             Some(x) => Ok(x),
    //             None => Err(RuntimeError {
    //                 kind: ErrorKind::TypeError,
    //                 msg: format!("expected  type 'number' but is {}", self.type_of()),
    //             }),
    //         }?;
    //         Ok(dummy_convert::<f64, T>(x))
    //     } else if TypeId::of::<bool>() == TypeId::of::<T>() {
    //         let x = self.to_bool();
    //         Ok(dummy_convert::<bool, T>(x))
    //     } else if TypeId::of::<String>() == TypeId::of::<T>() {
    //         let x = match self.as_string() {
    //             Some(x) => Ok(x.clone()),
    //             None => Err(RuntimeError {
    //                 kind: ErrorKind::TypeError,
    //                 msg: format!("expected  type 'string' but is {}", self.type_of()),
    //             }),
    //         }?;
    //         Ok(dummy_convert::<String, T>(x))
    //     } else {
    //         panic!("attempt to cast a userdata, use cast_ref instead!");
    //     }
    // }
    pub fn cast<T: 'static>(&self) -> Result<&T, RuntimeError> {
        if TypeId::of::<f64>() == TypeId::of::<T>() {
            let x = arg_f64!(self)?;
            Ok(dummy_convert_ref::<f64, T>(x))
        }
        /*else if TypeId::of::<f32>() == TypeId::of::<T>() {
            let x = arg_prim!(self, f32);
            unsafe {
                let prim = &*self.prim.get();
                let x = match prim {
                    RustPrimitive::F32(x) => &x.0,
                    _ => unreachable!(),
                };
                Ok(dummy_convert_ref::<f32, T>(x))
            }
        } else if TypeId::of::<i32>() == TypeId::of::<T>() {
            arg_prim_full!(self, i32, I32)
        } else if TypeId::of::<u32>() == TypeId::of::<T>() {
            arg_prim_full!(self, u32, U32)
        } else if TypeId::of::<i16>() == TypeId::of::<T>() {
            arg_prim_full!(self, i16, I16)
        } else if TypeId::of::<u16>() == TypeId::of::<T>() {
            arg_prim_full!(self, u16, U16)
        } else if TypeId::of::<i8>() == TypeId::of::<T>() {
            arg_prim_full!(self, i8, I8)
        } else if TypeId::of::<u8>() == TypeId::of::<T>() {
            arg_prim_full!(self, u8, U8)
        }*/
        else if TypeId::of::<bool>() == TypeId::of::<T>() {
            let x = match self.as_bool() {
                Some(x) => Ok(x),
                None => Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("expected  type 'boolean' but is {}", self.type_of()),
                }),
            }?;
            Ok(dummy_convert_ref::<bool, T>(x))
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
                prim: UnsafeCell::new(RustPrimitive::Unit),
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
                prim: UnsafeCell::new(RustPrimitive::Unit),
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
                Value::String(gc.allocate(Managed::new(name))),
                Value::Callable(gc.allocate(Box::new(NativeFunction::new(f)))),
            );
        }
        self
    }
    pub fn submodule(&mut self, name: String, module: Module) -> &mut Module {
        {
            let gc = self.runtime.borrow().gc.clone();
            let mut table = self.module.as_table().unwrap().borrow_mut();
            table.set(
                Value::String(gc.allocate(Managed::new(name))),
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
        self.inner.borrow_mut().create_instance(self.inner.clone())
    }
    pub fn create_module(&self) -> Module {
        let gc = self.inner.borrow().gc.clone();
        Module {
            runtime: self.inner.clone(),
            module: Value::Table(gc.allocate(RefCell::new(Table::new()))),
        }
    }
    pub fn add_module(&self, name: String, module: Module) {
        self.inner.borrow_mut().add_module(name, module)
    }
    pub fn compile(&self, filename: &str, source: &str) -> Result<ByteCodeModule, CompileError> {
        let show_ast = std::env::var("PRINT_AST").map_or(false, |x| x == "1");
        let show_module = std::env::var("PRINT_BYTECODE").map_or(false, |x| x == "1");
        let tokens = tokenize(filename, &source);
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                return Err(CompileError {
                    loc: err.loc.clone(),
                    kind: crate::compile::ErrorKind::ParseOrTokenizeError,
                    msg: format!(
                        "error: {}\n  --> {}:{}:{}",
                        err.msg, *err.loc.file, err.loc.line, err.loc.col
                    ),
                })
            }
        };

        let expr = parse_impl(tokens);
        let expr = match expr {
            Ok(expr) => expr,
            Err(err) => {
                return Err(CompileError {
                    loc: err.loc.clone(),
                    kind: crate::compile::ErrorKind::ParseOrTokenizeError,
                    msg: format!(
                        "error: {}\n  --> {}:{}:{}",
                        err.msg, *err.loc.file, err.loc.line, err.loc.col
                    ),
                });
            }
        };
        if show_ast {
            println!("{:#?}", expr);
        }

        let module = compile(expr)?;
        if show_module {
            println!("{:#?}", module);
        }
        Ok(module)
    }
}

impl RuntimeInner {
    pub fn add_module(&mut self, name: String, module: Module) {
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(
            Value::String(self.gc.allocate(Managed { data: name })),
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
            Value::String(self.gc.allocate(Managed { data: name })),
            Value::Callable(self.gc.allocate(callable)),
        );
        // Some(Local{
        //     value:v,
        //     phantom:PhantomData{},
        // })
    }
    fn create_instance(&mut self, pself: Rc<RefCell<RuntimeInner>>) -> Rc<Instance> {
        let mut instance = Rc::new(Instance {
            gc: self.gc.clone(),
            runtime: pself.clone(),
            state: State {
                gc: self.gc.clone(),
                globals: self.globals,
                frames: RefCell::new(Stack::new()),
                eval_stack: RefCell::new(vec![]),
                constants: self.constants.clone(),
                instance: Weak::new(),
            },
        });
        {
            let p = Rc::downgrade(&instance);
            unsafe {
                let i = Rc::as_ptr(&instance) as *mut Instance;
                (*i).state.instance = p;
            }
        }
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
        self.add_function("getmetatable".into(), |ctx| {
            let t = ctx.arg(0)?;
            let mt = match &t.value {
                Value::Table(t) => {
                    let t = t.borrow();
                    t.metatable
                }
                _ => Value::Nil,
            };
            ctx.ret(0, ValueRef::new(mt));
            Ok(())
        });
        self.add_function("setmetatable".into(), |ctx| {
            let t = ctx.arg(0)?;
            let mt = ctx.arg_or_nil(1);
            let _mt = mt.value.as_table().map_or(
                Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!(
                        "setmetatable expect table at second argument but found {}",
                        mt.type_of()
                    ),
                }),
                |mt| Ok(mt),
            )?;
            match &t.value {
                // Value::String(_)=>{
                //     // t.value.metatable
                // }
                Value::Table(table) => {
                    let mut table = table.borrow_mut();
                    table.metatable = mt.value;
                }
                _ => {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!("attemp to setmetatble for {}", t.type_of()),
                    });
                }
            }
            ctx.ret(0, t);
            Ok(())
        });
        self.add_function("type".into(), |ctx| {
            let v = ctx.arg(0).unwrap();
            ctx.ret(0, ctx.create_string(String::from(v.type_of())));
            Ok(())
        });
        self.add_function("next".into(), |ctx| {
            let v = ctx.arg(0)?;
            let key = ctx.arg_or_nil(1);
            let table = v.value.as_table().map_or(
                Err(RuntimeError {
                    kind: ErrorKind::TypeError,
                    msg: format!("table expected in 'next' but found {}", v.value.type_of()),
                }),
                |x| Ok(x),
            )?;
            let table = table.borrow();
            let next = table.next(key.value)?;
            ctx.ret(
                0,
                ValueRef {
                    value: next,
                    phantom: PhantomData {},
                    prim: UnsafeCell::new(RustPrimitive::Unit),
                },
            );
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
    pub(crate) fn create_pooled_string(&mut self, s: &String) -> Value {
        if let Some(v) = self.string_pool.get(s) {
            *v
        } else {
            let v = Value::String(self.gc.allocate(Managed::new(s.clone())));
            self.string_pool.insert(s.clone(), v);
            v
        }
    }
    fn new() -> Self {
        let gc = Rc::new(GcState::new());
        let mut runtime = Self {
            gc: gc.clone(),
            string_pool: HashMap::new(),
            globals: Value::Table(gc.allocate(RefCell::new(Table::new()))),
            instances: vec![],
            constants: Rc::new(vec![]),
        };
        let mut constants = vec![Value::Nil; ConstantsIndex::NumConstants as usize];
        constants[ConstantsIndex::MtBool as usize] =
            Value::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtClosure as usize] =
            Value::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtNumber as usize] =
            Value::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtString as usize] =
            Value::Table(gc.allocate(RefCell::new(Table::new())));

        constants[ConstantsIndex::MtKeyIndex as usize] =
            runtime.create_pooled_string(&String::from("__index"));
        constants[ConstantsIndex::MtKeyAdd as usize] =
            runtime.create_pooled_string(&String::from("__add"));
        constants[ConstantsIndex::MtKeySub as usize] =
            runtime.create_pooled_string(&String::from("__sub"));
        constants[ConstantsIndex::MtKeyMul as usize] =
            runtime.create_pooled_string(&String::from("__mul"));
        constants[ConstantsIndex::MtKeyDiv as usize] =
            runtime.create_pooled_string(&String::from("__div"));
        constants[ConstantsIndex::MtKeyMod as usize] =
            runtime.create_pooled_string(&String::from("__mod"));
        runtime.constants = Rc::new(constants);
        runtime
    }
}
impl Traceable for RuntimeInner {
    fn trace(&self, gc: &GcState) {
        gc.trace(&self.globals);
        for c in &*self.constants {
            gc.trace(c);
        }
        for instance in &self.instances {
            gc.trace(instance.as_ref());
        }
        for (_, s) in &self.string_pool {
            gc.trace(s);
        }
    }
}
