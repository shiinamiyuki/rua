use std::{
    cell::{Ref, RefCell},
    cmp::Ordering,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use ordered_float::OrderedFloat;
use parking_lot::RwLock;

use crate::{
    closure::Closure,
    context::Context,
    error::Error,
    gc::{Gc, Heap, TraceContext, Traceable},
    table::Table,
};

unsafe impl Traceable for String {
    fn trace(&self, _ctx: &mut TraceContext) {}
}
pub(crate) trait Callable: Traceable {
    fn call(&self, ctx: &Context, args: &[Value]) -> Result<Value, Error>;
    fn is_method(&self) -> bool;
}
pub(crate) struct NativeFunction<F: Fn(&Context, &[Value]) -> Result<Value, Error>> {
    f: F,
}
impl<F> NativeFunction<F>
where
    F: Fn(&Context, &[Value]) -> Result<Value, Error>,
{
    pub(crate) fn new(f: F) -> Self {
        Self { f }
    }
}
unsafe impl<F> Traceable for NativeFunction<F>
where
    F: Fn(&Context, &[Value]) -> Result<Value, Error> + Send + Sync,
{
    fn trace(&self, _: &mut TraceContext) {}
}
impl<F> Callable for NativeFunction<F>
where
    F: Fn(&Context, &[Value]) -> Result<Value, Error> + Send + Sync,
{
    fn call(&self, ctx: &Context, args: &[Value]) -> Result<Value, Error> {
        (self.f)(ctx, args)
    }
    fn is_method(&self) -> bool {
        false
    }
}

// impl Traceable for dyn Callable {
//     fn trace(&self, _gc: &crate::gc::GcState) {}
// }

pub trait UserData: Traceable {
    fn typename(&self) -> &str;
    // fn method(&self, method:&str, ctx: &Context, args: &[Value]) -> Result<Value, Error>;
}

unsafe impl Traceable for Vec<Value> {
    fn trace(&self, ctx: &mut TraceContext) {
        for i in self {
            ctx.trace(i);
        }
    }
}
unsafe impl Traceable for Box<dyn Callable> {
    fn trace(&self, ctx: &mut TraceContext) {
        self.as_ref().trace(ctx)
    }
}
unsafe impl Traceable for Box<dyn UserData> {
    fn trace(&self, ctx: &mut TraceContext) {
        self.as_ref().trace(ctx)
    }
}


pub(crate) struct StringKey(pub(crate) Gc<String>);
impl Hash for StringKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let s: &String = &self.0;
        s.hash(state)
    }
}
impl PartialEq for StringKey {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == other.0 {
            return true;
        }
        let a = &*self.0;
        let b = &*other.0;
        a == b
    }
}
impl Eq for StringKey {}

#[derive(Clone)]
#[repr(C)]
pub(crate) enum Value {
    Nil,
    Bool(bool),
    Number(OrderedFloat<f64>),
    String(Gc<String>),
    Table(Gc<RwLock<Table>>),
    Callable(Gc<Box<dyn Callable>>),
    Closure(Gc<Closure>),
    UserData(Gc<Box<dyn UserData>>),
}

mod test {
    #[test]
    fn test_size() {
        assert_eq!(std::mem::size_of::<super::Value>(), 16);
    }
}
impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Bool(x) => x.to_string(),
            Value::Number(x) => x.to_string(),
            Value::String(s) => (**s).clone(),
            Value::Table(p) => {
                format!("table at {:?}", p.as_ptr())
            }
            Value::Callable(p) => {
                format!("callable at {:?}", p.as_ptr())
            }
            Value::UserData(p) => {
                format!("userdata at {:?}", p.as_ptr())
            }
            Value::Closure(p) => {
                format!("function at {:?}", p.as_ptr())
            }
        }
    }
}
impl Value {
    pub(crate) fn to_debug_string(&self) -> String {
        match self {
            Value::Nil => "null".into(),
            Value::Bool(x) => x.to_string(),
            Value::Number(x) => x.to_string(),
            Value::String(s) => format!("string at {:?} '{}'", s.as_ptr(), (**s).clone()),
            Value::Table(p) => {
                format!("table at {:?}", p.as_ptr())
            }
            Value::Callable(p) => {
                format!("callable at {:?}", p.as_ptr())
            }
            Value::UserData(p) => {
                format!("userdata at {:?}", p.as_ptr())
            }
            Value::Closure(p) => {
                format!("function at {:?}", p.as_ptr())
            }
        }
    }
}
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_debug_string())
    }
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => {
                if a == b {
                    return Some(Ordering::Equal);
                }
                let a = &(**a);
                let b = &(**b);
                a.partial_cmp(b)
            }
            (Value::Table(a), Value::Table(b)) => a.as_ptr().partial_cmp(&b.as_ptr()),
            _ => None,
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self.partial_cmp(other) {
            Some(ord) => ord == Ordering::Equal,
            None => false,
        }
    }
}
impl Eq for Value {}
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => ().hash(state),
            Value::Number(x) => x.hash(state),
            Value::Bool(x) => x.hash(state),
            Value::String(x) => {
                x.hash(state);
                let s: &String = x;
                s.hash(state)
            }
            Value::Table(x) => x.hash(state),
            Value::Callable(x) => x.hash(state),
            Value::Closure(x) => x.hash(state),
            Value::UserData(x) => x.hash(state),
        }
    }
}
impl Value {
    pub(crate) fn nil() -> Self {
        Value::Nil
    }
    pub(crate) fn is_nil(self) -> bool {
        match self {
            Self::Nil => true,
            _ => false,
        }
    }
    pub(crate) fn as_int(self) -> Option<i64> {
        match self {
            Self::Number(x) => Some(x.0 as i64),
            _ => None,
        }
    }
    pub(crate) fn as_int_exact(self) -> Option<i64> {
        let x = self.as_float()?;
        let fract = x.fract();
        if fract == 0.0 {
            Some(x.trunc() as i64)
        } else {
            None
        }
    }
    pub(crate) fn as_float(self) -> Option<f64> {
        match self {
            Self::Number(x) => Some(x.0),
            _ => None,
        }
    }
    pub(crate) fn as_string<'a>(&'a self) -> Option<&'a String> {
        match self {
            Self::String(x) => Some(&**x),
            _ => None,
        }
    }
    pub(crate) fn as_table<'a>(&'a self) -> Option<&'a RwLock<Table>> {
        match self {
            Self::Table(x) => Some(&**x),
            _ => None,
        }
    }
    pub(crate) fn type_of(&self) -> &'static str {
        match self {
            Value::Nil => "null",
            Value::Number(_) => "number",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Table(_) => "table",
            Value::Callable(_) => "function",
            Value::Closure(_) => "function",
            Value::UserData(_) => "userdata",
        }
    }
}
impl From<i64> for Value {
    fn from(x: i64) -> Self {
        Value::Number(OrderedFloat(x as f64))
    }
}
impl From<bool> for Value {
    fn from(x: bool) -> Self {
        Value::Bool(x)
    }
}
impl From<f64> for Value {
    fn from(x: f64) -> Self {
        Value::Number(OrderedFloat(x))
    }
}
impl From<Gc<String>> for Value {
    fn from(x: Gc<String>) -> Self {
        Value::String(x)
    }
}

impl Copy for Value {}
unsafe impl Traceable for Value {
    fn trace(&self, ctx: &mut TraceContext) {
        match self {
            // Value::Callable(c) => gc.trace_ptr(*c),
            // Value::UserData(u) => gc.trace_ptr(*u),
            // Value::Closure(c) => gc.trace_ptr(*c),
            // Value::Object(ObjectRef { object: o, .. }) => gc.trace_ptr(*o),
            Value::Callable(x) => {
                ctx.trace_ptr(*x);
            }
            Value::UserData(x) => {
                ctx.trace_ptr(*x);
            }
            // ValueType::Closure => {
            //     gc.trace_ptr(self.inner.closure);
            // }
            _ => {}
        }
    }
}

unsafe impl Traceable for HashMap<Value, Value> {
    fn trace(&self, ctx: &mut TraceContext) {
        for (k, v) in self {
            ctx.trace(k);
            ctx.trace(v);
        }
    }
}
