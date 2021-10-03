use std::{
    any::TypeId,
    cell::{Cell, Ref, RefCell, RefMut, UnsafeCell},
    collections::HashMap,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
};

use ordered_float::OrderedFloat;

use crate::{api::StateApi, runtime::ErrorKind::ExternalError, CloneCell};
use crate::{
    api::{BaseApi, CallApi},
    bytecode::ByteCodeModule,
    closure::{Callable, NativeFunction},
    compile::{compile, CompileError},
    dummy_convert_ref,
    gc::{GcState, Traceable},
    parse::{parse_impl, tokenize},
    state::{CallContext, State},
    stdlib,
    table::Table,
    value::{Managed, RawValue, UserData},
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

pub(crate) struct GcValueList {
    pub(crate) head: Box<ValueBox>,
    pub(crate) tail: Box<ValueBox>,
}

impl GcValueList {
    pub(crate) fn new() -> Self {
        let head = Box::new(ValueBox {
            prev: Cell::new(std::ptr::null()),
            next: Cell::new(std::ptr::null()),
            value: RefCell::new(RawValue::Nil),
            _runtime: None,
        });
        let tail = Box::new(ValueBox {
            prev: Cell::new(std::ptr::null()),
            next: Cell::new(std::ptr::null()),
            value: RefCell::new(RawValue::Nil),
            _runtime: None,
        });
        {
            head.next.set(tail.as_ref() as *const ValueBox);
            tail.prev.set(head.as_ref() as *const ValueBox);
        }
        Self { head, tail }
    }
}

pub(crate) struct GlobalState {
    pub(crate) constants: Vec<CloneCell<RawValue>>,
}

pub(crate) struct RuntimeInner {
    pub(crate) gc: Rc<GcState>,
    pub(crate) globals: RawValue,
    pub(crate) global_state: Option<Rc<GlobalState>>,
    pub(crate) instances: Vec<Weak<Instance>>,
    pub(crate) string_pool: HashMap<String, RawValue>,
    pub(crate) gc_value_list: GcValueList,
}

pub(crate) enum ConstantsIndex {
    MtNumber,
    MtString,
    MtBool,
    MtClosure,
    MtKeyIndex,
    MtKeyNewIndex,
    MtKeyAdd,
    MtKeySub,
    MtKeyMul,
    MtKeyDiv,
    MtKeyIDiv,
    MtKeyMod,
    MtKeyPow,
    MtKeyConcat,
    MtKeyEq,
    MtKeyLt,
    MtKeyLe,
    MtKeyLen,
    MtKeyNeg,
    MtKeyCall,
    MtKeyToString,
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

#[derive(Clone)]
pub struct Value<'a> {
    pub(crate) value: RawValue,
    pub(crate) phantom: PhantomData<&'a u32>,
    pub(crate) arg_idx: Option<usize>,
    // pub(crate) prim: UnsafeCell<RustPrimitive>,
}

impl<'a> Value<'a> {
    pub(crate) fn new_arg(v: RawValue, idx: usize) -> Self {
        Self {
            value: v,
            phantom: PhantomData {},
            arg_idx: Some(idx), // prim: UnsafeCell::new(RustPrimitive::Unit),
        }
    }
    pub(crate) fn new(v: RawValue) -> Self {
        Self {
            value: v,
            phantom: PhantomData {},
            arg_idx: None, // prim: UnsafeCell::new(RustPrimitive::Unit),
        }
    }
}
macro_rules! impl_from_prim {
    ($t:ty) => {
        impl<'a> From<$t> for Value<'a> {
            fn from(x: $t) -> Self {
                Self::new(RawValue::from_number(x as f64))
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
                if $self.arg_idx.is_some() {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!(
                            "bad argument #{}, expected  type 'number' but is {}",
                            $self.arg_idx.unwrap() + 1,
                            $self.type_of()
                        ),
                    });
                } else {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!("expected  type 'number' but is {}", $self.type_of()),
                    });
                }
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
                    if $self.arg_idx.is_some() {
                        return Err(RuntimeError {
                            kind: ErrorKind::TypeError,
                            msg: format!(
                                "bad argument #{}, expected type 'userdata' but is {}",
                                $self.arg_idx.unwrap() + 1,
                                $self.type_of()
                            ),
                        });
                    } else {
                        return Err(RuntimeError {
                            kind: ErrorKind::TypeError,
                            msg: format!(
                                "expected type 'userdata' {} but is {}",
                                userdata.type_name(),
                                std::any::type_name::<$t>()
                            ),
                        });
                    }
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
                if $self.arg_idx.is_some() {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!(
                            "bad argument #{}, expected type 'string' but is {}",
                            $self.arg_idx.unwrap() + 1,
                            $self.type_of()
                        ),
                    });
                } else {
                    return Err(RuntimeError {
                        kind: ErrorKind::TypeError,
                        msg: format!("expected 'string' but is {}", $self.type_of()),
                    });
                }
            }
        }
    }};
}
// macro_rules! arg_prim {
//     ($self:expr, $t:ty) => {
//        { let x = arg_f64!($self)?;
//         unsafe{
//             let prim = &mut *$self.prim.get();
//             if *prim != RustPrimitive::Unit {
//                 return Err(RuntimeError {
//                     kind: ErrorKind::TypeError,
//                     msg: "this is an internal limitation, try let another = this_var; another.cast::<T>()".into(),
//                 });
//             }
//             *prim = RustPrimitive::from(*x as $t);
//         }
//         x
//     }
//     };
// }
// macro_rules! arg_prim_full {
//     ($self:expr, $t:ty,$v:ident) => {
//        { let x = arg_f64!($self)?;
//         unsafe{
//             let prim = &mut *$self.prim.get();
//             if *prim != RustPrimitive::Unit {
//                 return Err(RuntimeError {
//                     kind: ErrorKind::TypeError,
//                     msg: "this is an internal limitation, try let another = this_var; another.cast::<T>()".into(),
//                 });
//             }
//             *prim = RustPrimitive::from(*x as $t);
//             let prim = & *$self.prim.get();
//             let x = match prim {
//                 RustPrimitive::$v(x)=>x,
//                 _=>unreachable!(),
//             };
//             Ok(dummy_convert_ref::<$t, T>(x))
//         }
//     }
//     };
// }
impl<'a> Value<'a> {
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
    pub fn cast<T: 'static>(&self) -> Result<&'a T, RuntimeError> {
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

pub(crate) struct ValueBox {
    next: Cell<*const ValueBox>,
    prev: Cell<*const ValueBox>,
    value: RefCell<RawValue>,
    _runtime: Option<Rc<RefCell<RuntimeInner>>>,
}
/*
Safe wrapper for Value
Represent a gc root
a Root is never collected as long as it is not dropped
*/
pub struct GcValue {
    inner: Box<ValueBox>,
}

pub struct RootBorrowMut<'a> {
    // inner: &'a ValueBox,
    _ref: RefMut<'a, RawValue>,
    value: Value<'a>,
}

pub struct RootBorrow<'a> {
    // inner: &'a ValueBox,
    _ref: Ref<'a, RawValue>,
    value: Value<'a>,
}

impl<'a> Deref for RootBorrow<'a> {
    type Target = Value<'a>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a> Deref for RootBorrowMut<'a> {
    type Target = Value<'a>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a> DerefMut for RootBorrowMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl GcValue {
    pub fn borrow_mut<'a>(&'a self) -> RootBorrowMut<'a> {
        let b = self.inner.value.borrow_mut();
        RootBorrowMut {
            // inner: self.inner.as_ref(),
            value: Value::new((*b).clone()),
            _ref: b,
        }
    }
    pub fn borrow<'a>(&'a self) -> RootBorrow<'a> {
        let b = self.inner.value.borrow();
        RootBorrow {
            // inner: self.inner.as_ref(),
            value: Value::new((*b).clone()),
            _ref: b,
        }
    }
}

impl Drop for GcValue {
    fn drop(&mut self) {
        unsafe {
            let prev = self.inner.prev.get().as_ref().unwrap();
            let next = self.inner.next.get().as_ref().unwrap();
            prev.next.set(next as *const ValueBox);
            next.prev.set(prev as *const ValueBox);
        }
    }
}

impl<'a> Drop for RootBorrowMut<'a> {
    fn drop(&mut self) {
        *self._ref = self.value.value.clone();
    }
}
// pub struct LuaClass {
//     runtime: Rc<RefCell<RuntimeInner>>,
// }
// impl LuaClass {
//     pub fn function<'a, F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(
//         &'a mut self,
//         name: String,
//         f: F,
//     ) -> &mut Module {
//         {
//             let gc = self.runtime.borrow().gc.clone();
//             let mut table = self.module.as_table().unwrap().borrow_mut();
//             table.set(
//                 Value::String(gc.allocate(Managed::new(name))),
//                 Value::Callable(gc.allocate(Box::new(NativeFunction::new(f)))),
//             );
//         }
//         self
//     }
// }
pub struct Module {
    runtime: Rc<RefCell<RuntimeInner>>,
    pub(crate) module: RawValue,
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
                RawValue::String(gc.allocate(Managed::new(name))),
                RawValue::Callable(gc.allocate(Box::new(NativeFunction::new(f)))),
            );
        }
        self
    }
    pub fn submodule(&mut self, name: String, module: Module) -> &mut Module {
        {
            let gc = self.runtime.borrow().gc.clone();
            let mut table = self.module.as_table().unwrap().borrow_mut();
            table.set(
                RawValue::String(gc.allocate(Managed::new(name))),
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
            inner.add_std_lib(Rc::downgrade(&r.inner));
        }
        stdlib::add_math_lib(&r);
        stdlib::add_string_lib(&r);
        stdlib::add_table_lib(&r);
        #[cfg(feature = "complete")]
        crate::na_bind::add_na_lib(&r);
        {
            let std_src = include_str!("stdlib.lua");
            let instance = r.create_instance();
            let module = compile_src("stdlib.lua", std_src).unwrap();
            instance.exec(module).unwrap();
        }
        r
    }
    pub fn create_instance(&self) -> Rc<Instance> {
        self.inner.borrow_mut().create_instance(self.inner.clone())
    }
    pub fn create_module(&self) -> Module {
        let gc = self.inner.borrow().gc.clone();
        Module {
            runtime: self.inner.clone(),
            module: RawValue::Table(gc.allocate(RefCell::new(Table::new()))),
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
                });
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

pub(crate) fn compile_src(filename: &str, src: &str) -> Result<ByteCodeModule, RuntimeError> {
    let tokens = tokenize(filename, src);
    let tokens = tokens.map_err(|err| RuntimeError {
        kind: ErrorKind::CompileError,
        msg: format!(
            "error: {}\n  --> {}:{}:{}",
            err.msg, *err.loc.file, err.loc.line, err.loc.col
        ),
    })?;

    let expr = parse_impl(tokens);
    let expr = expr.map_err(|err| RuntimeError {
        kind: ErrorKind::CompileError,
        msg: format!(
            "error: {}\n  --> {}:{}:{}",
            err.msg, *err.loc.file, err.loc.line, err.loc.col
        ),
    })?;

    let module = compile(expr).map_err(|err| RuntimeError {
        kind: ErrorKind::CompileError,
        msg: format!(
            "error: {}\n  --> {}:{}:{}",
            err.msg, *err.loc.file, err.loc.line, err.loc.col
        ),
    })?;
    Ok(module)
}
pub(crate) fn compile_file(path: &str) -> Result<ByteCodeModule, RuntimeError> {
    let src = std::fs::read_to_string(path).unwrap();
    compile_src(path, &src)
}

impl RuntimeInner {
    pub fn add_module(&mut self, name: String, module: Module) {
        let name = self.create_pooled_string(&name);
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(name, module.module);
    }
    pub fn add_var(&mut self, name: String, var: RawValue) {
        let name = self.create_pooled_string(&name);
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();

        globals.set(name, var);
    }
    fn add_function<F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(
        &mut self,
        name: String,
        f: F,
    ) {
        self.add_callable(name, Box::new(NativeFunction::new(f)))
    }
    fn add_callable<'a>(&'a mut self, name: String, callable: Box<dyn Callable>)
    /*->Option<Local<'a>>*/
    {
        let name = self.create_pooled_string(&name);
        let globals = self.globals.as_table().unwrap();
        let mut globals = globals.borrow_mut();
        globals.set(name, RawValue::Callable(self.gc.allocate(callable)));
        // Some(Local{
        //     value:v,
        //     phantom:PhantomData{},
        // })
    }
    fn create_instance(&mut self, pself: Rc<RefCell<RuntimeInner>>) -> Rc<Instance> {
        let instance = Rc::new(Instance {
            gc: self.gc.clone(),
            runtime: pself.clone(),
            state: State {
                gc: self.gc.clone(),
                globals: self.globals.clone(),
                frames: RefCell::new(Stack::new()),
                eval_stack: RefCell::new(vec![]),
                global_state: self.global_state.clone().unwrap(),
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
        self.instances.push(Rc::downgrade(&instance));
        instance
    }
    pub(crate) fn collectgarbage(&self) {
        self.gc.start_trace();
        self.gc.trace(self);
        self.gc.end_trace();
    }
    fn add_std_lib(&mut self, pself: Weak<RefCell<RuntimeInner>>) {
        {
            let pself = pself.clone();
            self.add_function("tostring".into(), move |ctx| {
                let v = ctx.arg(0)?;
                let s = {
                    let mt = ctx.get_metatable(v.clone());
                    if !mt.value.is_nil() {
                        let tostring = ctx.table_get(
                            mt,
                            Value::new(
                                pself
                                    .upgrade()
                                    .unwrap()
                                    .borrow()
                                    .global_state
                                    .as_ref()
                                    .unwrap()
                                    .constants
                                    [ConstantsIndex::MtKeyToString as usize]
                                    .get(),
                            ),
                        )?;
                        if tostring.value.is_nil() {
                            ctx.create_string(v.value.print())
                        } else {
                            ctx.call(tostring, &[v])?
                        }
                    } else {
                        ctx.create_string(v.value.print())
                    }
                };
                ctx.ret(0, s);
                Ok(())
            });
        }
        self.add_function("print".into(), |ctx| {
            for i in 0..ctx.arg_count() {
                let arg = ctx.arg(i).unwrap();
                if i > 0 {
                    print!(" ");
                }
                let env = ctx.get_global_env();
                let tostring = ctx.table_get(env, ctx.create_string("tostring".into()))?;
                let s = ctx.call(tostring, &[arg])?;
                print!("{}", s.value.print());
            }
            print!("\n");
            Ok(())
        });
        {
            let ipairs_iter: Box<dyn Callable> = Box::new(NativeFunction::new(|ctx| {
                let a = ctx.arg(0)?;
                let i = ctx.arg(1)?;
                let i = Value::new(ctx.state.add(&i.value, &RawValue::from_number(1.0))?);
                let v = ctx.table_get(a, i.clone())?;
                if v.to_bool() {
                    ctx.ret(0, i);
                    ctx.ret(1, v);
                }
                Ok(())
            }));
            let ipairs_iter = RawValue::Callable(self.gc.allocate(ipairs_iter));
            let ipairs_iter = pself.upgrade().unwrap().upgrade(Value::new(ipairs_iter));
            self.add_function("ipairs".into(), move |ctx| {
                let it = ipairs_iter.borrow();
                ctx.ret(0, (*it).clone());
                ctx.ret(1, ctx.arg(0)?);
                ctx.ret(2, Value::from(0));
                Ok(())
            });
        }
        self.add_function("assert".into(), |ctx| {
            let v = ctx.arg(0)?;
            assert!(v.value.to_bool());
            Ok(())
        });
        {
            let pself = pself.clone();
            self.add_function("require".into(), move |ctx| {
                let v = ctx.arg(0)?;
                let path = v.cast::<String>()?;
                let module = compile_file(path)?;
                let pself = pself.upgrade().unwrap();
                let instance = {
                    let mut inner = pself.borrow_mut();
                    inner.create_instance(pself.clone())
                };
                let ret = instance.exec(module)?;
                ctx.ret(0, ret);
                Ok(())
            });
        }
        self.add_function("rawget".into(), |ctx| {
            let table = ctx.arg_or_nil(0);
            let key = ctx.arg_or_nil(1);
            ctx.ret(
                0,
                Value::new(ctx.state.table_rawget(&table.value, &key.value)?),
            );
            Ok(())
        });
        self.add_function("rawset".into(), |ctx| {
            let table = ctx.arg_or_nil(0);
            let key = ctx.arg_or_nil(1);
            let value = ctx.arg_or_nil(2);
            ctx.state
                .table_rawset(&table.value, &key.value, &value.value)?;
            Ok(())
        });
        self.add_function("rawequal".into(), |ctx| {
            let a = ctx.arg_or_nil(0);
            let b = ctx.arg_or_nil(1);
            ctx.ret(0, Value::new(RawValue::Bool(a.value == b.value)));
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
                RawValue::Table(t) => {
                    let t = t.borrow();
                    t.metatable.clone()
                }
                _ => RawValue::Nil,
            };
            ctx.ret(0, Value::new(mt));
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
                RawValue::Table(table) => {
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
            let value = if !next.is_nil() {
                table.get(&next)
            } else {
                RawValue::Nil
            };
            ctx.ret(0, Value::new(next));
            ctx.ret(1, Value::new(value));
            Ok(())
        });
        // let pself = self as *mut RuntimeInner;
        let gc = self.gc.clone();
        self.add_function("collectgarbage".into(), move |_ctx| {
            let pself = pself.upgrade().unwrap();
            let self_ = pself.borrow();
            gc.start_trace();
            gc.trace(&*self_);
            gc.end_trace();
            gc.collect(true);
            Ok(())
        });
    }
    pub(crate) fn create_pooled_string(&mut self, s: &String) -> RawValue {
        if let Some(v) = self.string_pool.get(s) {
            v.clone()
        } else {
            let v = RawValue::String(self.gc.allocate(Managed::new(s.clone())));
            self.string_pool.insert(s.clone(), v.clone());
            v
        }
    }
    fn new() -> Self {
        let gc = Rc::new(GcState::new());
        let mut runtime = Self {
            gc: gc.clone(),
            string_pool: HashMap::new(),
            globals: RawValue::Table(gc.allocate(RefCell::new(Table::new()))),
            instances: vec![],
            global_state: None,
            gc_value_list: GcValueList::new(),
        };
        let mut constants = vec![RawValue::Nil; ConstantsIndex::NumConstants as usize];
        constants[ConstantsIndex::MtBool as usize] =
            RawValue::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtClosure as usize] =
            RawValue::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtNumber as usize] =
            RawValue::Table(gc.allocate(RefCell::new(Table::new())));
        constants[ConstantsIndex::MtString as usize] =
            RawValue::Table(gc.allocate(RefCell::new(Table::new())));

        constants[ConstantsIndex::MtKeyIndex as usize] =
            runtime.create_pooled_string(&String::from("__index"));
        constants[ConstantsIndex::MtKeyNewIndex as usize] =
            runtime.create_pooled_string(&String::from("__newindex"));
        constants[ConstantsIndex::MtKeyAdd as usize] =
            runtime.create_pooled_string(&String::from("__add"));
        constants[ConstantsIndex::MtKeySub as usize] =
            runtime.create_pooled_string(&String::from("__sub"));
        constants[ConstantsIndex::MtKeyMul as usize] =
            runtime.create_pooled_string(&String::from("__mul"));
        constants[ConstantsIndex::MtKeyDiv as usize] =
            runtime.create_pooled_string(&String::from("__div"));
        constants[ConstantsIndex::MtKeyIDiv as usize] =
            runtime.create_pooled_string(&String::from("__idiv"));
        constants[ConstantsIndex::MtKeyMod as usize] =
            runtime.create_pooled_string(&String::from("__mod"));
        constants[ConstantsIndex::MtKeyPow as usize] =
            runtime.create_pooled_string(&String::from("__pow"));
        constants[ConstantsIndex::MtKeyEq as usize] =
            runtime.create_pooled_string(&String::from("__eq"));
        constants[ConstantsIndex::MtKeyLt as usize] =
            runtime.create_pooled_string(&String::from("__lt"));
        constants[ConstantsIndex::MtKeyLe as usize] =
            runtime.create_pooled_string(&String::from("__le"));
        constants[ConstantsIndex::MtKeyCall as usize] =
            runtime.create_pooled_string(&String::from("__call"));
        constants[ConstantsIndex::MtKeyConcat as usize] =
            runtime.create_pooled_string(&String::from("__concat"));
        constants[ConstantsIndex::MtKeyLen as usize] =
            runtime.create_pooled_string(&String::from("__len"));
        constants[ConstantsIndex::MtKeyNeg as usize] =
            runtime.create_pooled_string(&String::from("__unm"));
        constants[ConstantsIndex::MtKeyToString as usize] =
            runtime.create_pooled_string(&String::from("__tostring"));
        for v in &constants {
            assert!(!v.is_nil());
        }
        let global_state = GlobalState {
            constants: constants.into_iter().map(|x| CloneCell::new(x)).collect(),
        };
        runtime.global_state = Some(Rc::new(global_state));
        runtime.add_var("_G".into(), runtime.globals.clone());
        runtime
    }
}

impl GlobalState {
    pub(crate) fn get_metatable(&self, v: &RawValue) -> RawValue {
        match v {
            RawValue::Nil => RawValue::Nil,
            RawValue::Bool(_) => self.constants[ConstantsIndex::MtBool as usize].get(),
            RawValue::Number(_) => self.constants[ConstantsIndex::MtNumber as usize].get(),
            RawValue::Table(t) => {
                let t = t.borrow();
                t.metatable.clone()
            }
            RawValue::String(_) => self.constants[ConstantsIndex::MtString as usize].get(),
            RawValue::Closure(_) => RawValue::Nil,
            RawValue::Callable(_) => RawValue::Nil,
            RawValue::Tuple(t) => t.metatable.get(),
            RawValue::UserData(p) => p.get_metatable().value,
        }
    }
    pub(crate) fn set_metatable(&self, v: &RawValue, mt: &RawValue) {
        match v {
            RawValue::Nil => {}
            RawValue::Bool(_) => self.constants[ConstantsIndex::MtBool as usize].set(mt.clone()),
            RawValue::Number(_) => {
                self.constants[ConstantsIndex::MtNumber as usize].set(mt.clone())
            }
            RawValue::Table(t) => {
                let mut t = t.borrow_mut();
                t.metatable = mt.clone();
            }
            RawValue::String(_) => {
                self.constants[ConstantsIndex::MtString as usize].set(mt.clone())
            }
            RawValue::Closure(_) => {}
            RawValue::Callable(_) => {}
            RawValue::Tuple(t) => {
                t.metatable.set(mt.clone());
            }
            RawValue::UserData(p) => p.set_metatable(Value::new(mt.clone())),
        }
    }
}

impl Traceable for RuntimeInner {
    fn trace(&self, gc: &GcState) {
        gc.trace(&self.globals);
        for c in self.global_state.as_ref().unwrap().constants.iter() {
            gc.trace(&c.get());
        }
        for instance in &self.instances {
            if let Some(instance) = instance.upgrade() {
                gc.trace(instance.as_ref());
            }
        }
        for (_, s) in &self.string_pool {
            gc.trace(s);
        }
        unsafe {
            let head = self.gc_value_list.head.as_ref() as *const ValueBox;
            let tail = self.gc_value_list.tail.as_ref() as *const ValueBox;
            let mut p = head;
            while tail != p {
                let v = p.as_ref().unwrap();
                if p != head {
                    let v = v.value.borrow();
                    gc.trace(&*v);
                }
                p = v.next.get();
            }
        }
    }
}

impl BaseApi for Runtime {
    fn create_number<'a>(&'a self, x: f64) -> Value<'a> {
        self.inner.create_number(x)
    }

    fn create_bool<'a>(&self, x: bool) -> Value<'a> {
        self.inner.create_bool(x)
    }

    fn create_userdata<'a, T: UserData + Traceable>(&self, userdata: T) -> Value<'a> {
        self.inner.create_userdata(userdata)
    }

    fn create_string<'a>(&self, s: String) -> Value<'a> {
        self.inner.create_string(s)
    }

    fn upgrade<'a>(&'a self, v: Value<'_>) -> GcValue {
        self.inner.upgrade(v)
    }

    fn create_closure<'a>(&self, closure: Box<dyn Callable>) -> Value<'a> {
        self.inner.create_closure(closure)
    }

    fn create_table<'a>(&self) -> Value<'a> {
        self.inner.create_table()
    }

    fn set_metatable<'a>(&self, v: Value<'a>, mt: Value<'a>) {
        self.inner.set_metatable(v, mt)
    }

    fn get_metatable<'a>(&self, v: Value<'a>) -> Value<'a> {
        self.inner.get_metatable(v)
    }

    fn table_rawset<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError> {
        self.inner.table_rawset(table, key, value)
    }

    fn table_rawget<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        self.inner.table_rawget(table, key)
    }

    fn get_global_env<'a>(&self) -> Value<'a> {
        self.inner.get_global_env()
    }
}

impl BaseApi for Rc<RefCell<RuntimeInner>> {
    fn get_global_env<'a>(&self) -> Value<'a> {
        Value::new(self.borrow().globals.clone())
    }
    fn create_number<'a>(&'a self, x: f64) -> Value<'a> {
        Value::new(RawValue::from_number(x))
    }

    fn create_bool<'a>(&self, x: bool) -> Value<'a> {
        Value::new(RawValue::from_bool(x))
    }

    fn create_userdata<'a, T: UserData + Traceable>(&self, userdata: T) -> Value<'a> {
        let p: Box<dyn UserData> = Box::new(userdata);
        let inner = self.borrow();
        let gc = &inner.gc;
        let s = gc.allocate(p);
        Value::new(RawValue::UserData(s))
    }

    fn create_string<'a>(&self, s: String) -> Value<'a> {
        let inner = self.borrow();
        let gc = &inner.gc;
        let s = gc.allocate(Managed::new(s));
        Value::new(RawValue::String(s))
    }

    fn upgrade<'a>(&'a self, v: Value<'_>) -> GcValue {
        let inner =  (**self).as_ptr();
        let inner = unsafe{&*inner};
        let head = inner.gc_value_list.head.as_ref();
        let head_next = head.next.get();
        // let tail = inner.gc_value_list.tail.as_ref();

        let value_box = Box::new(ValueBox {
            prev: Cell::new(head as *const ValueBox),
            next: Cell::new(head_next),
            value: RefCell::new(v.value),
            _runtime: Some(self.clone()),
        });
        unsafe {
            let head_next = head_next.as_ref().unwrap();
            head_next.prev.set(value_box.as_ref() as *const ValueBox);
            head.next.set(value_box.as_ref() as *const ValueBox);
        }
        GcValue { inner: value_box }
    }

    fn create_closure<'a>(&self, closure: Box<dyn Callable>) -> Value<'a> {
        let inner = self.borrow();
        let gc = &inner.gc;
        Value::new(RawValue::Callable(gc.allocate(closure)))
    }

    fn create_table<'a>(&self) -> Value<'a> {
        let inner = self.borrow();
        let gc = &inner.gc;
        Value::new(RawValue::Table(gc.allocate(RefCell::new(Table::new()))))
    }

    fn set_metatable<'a>(&self, v: Value<'a>, mt: Value<'a>) {
        let inner = self.borrow();
        inner
            .global_state
            .as_ref()
            .unwrap()
            .set_metatable(&v.value, &mt.value)
    }

    fn get_metatable<'a>(&self, v: Value<'a>) -> Value<'a> {
        let inner = self.borrow();
        Value::new(inner.global_state.as_ref().unwrap().get_metatable(&v.value))
    }

    fn table_rawset<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError> {
        if let Some(table) = table.value.as_table() {
            table.borrow_mut().set(key.value, value.value);
            Ok(())
        } else {
            Err(RuntimeError {
                kind: ErrorKind::TypeError,
                msg: format!("attempt to call rawset on a {} value", table.type_of()),
            })
        }
    }

    fn table_rawget<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        if let Some(table) = table.value.as_table() {
            Ok(Value::new(table.borrow().get(&key.value)))
        } else {
            Err(RuntimeError {
                kind: ErrorKind::TypeError,
                msg: format!("attempt to call rawget on a {} value", table.type_of()),
            })
        }
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        for instance in self.inner.borrow().instances.iter() {
            assert!(
                instance.upgrade().is_none(),
                "Runtime dropped before instances"
            );
        }
        // println!(
        //     "total alloc {}",
        //     self.inner.borrow().gc.inner.borrow().alloc_count
        // );
    }
}
