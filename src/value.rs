use std::{
    any::Any,
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::Hash,
    rc::Rc,
};

use ordered_float::OrderedFloat;
use smallvec::SmallVec;

use crate::{
    bytecode::ByteCodeModule,
    closure::{Callable, Closure},
    gc::{Gc, GcState, Traceable},
    runtime::Value,
    state::State,
    table::Table,
};

pub struct Managed<T: 'static + Any> {
    pub data: T,
    metatable: Cell<RawValue>,
}
pub type LightUserData<T> = Managed<T>;
impl<T> Managed<T> {
    pub fn new(data: T) -> Self {
        Self {
            data,
            metatable: Cell::new(RawValue::Nil),
        }
    }
}
impl<T> Traceable for Managed<T> {
    fn trace(&self, gc: &GcState) {
        gc.trace(&self.metatable.get());
    }
}

pub type ManagedCell<T> = Managed<RefCell<T>>;

pub trait UserData: Traceable + Any {
    fn as_traceable(&self) -> &dyn Traceable;
    fn as_any(&self) -> &dyn Any;
    fn type_name(&self) -> &'static str;
    fn set_metatable(&self, v: Value<'_>);
    fn get_metatable<'a>(&'a self) -> Value<'a>;
}
impl<T> UserData for Managed<T>
where
    T: Any,
{
    fn as_traceable(&self) -> &dyn Traceable {
        self
    }
    fn as_any(&self) -> &dyn Any {
        &self.data
    }
    fn type_name(&self) -> &'static str {
        std::any::type_name::<T>()
    }

    fn set_metatable(&self, v: Value<'_>) {
        self.metatable.set(v.value);
    }

    fn get_metatable<'a>(&'a self) -> Value<'a> {
        Value::new(self.metatable.get())
        // ValueRef::new(Value::Nil)
    }
}
impl Traceable for Box<dyn UserData> {
    fn trace(&self, gc: &GcState) {
        self.as_ref().trace(gc)
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum TupleFlag {
    Empty,
    VarArgs,
}

pub struct Tuple {
    pub(crate) values: RefCell<SmallVec<[RawValue; 8]>>,
    pub(crate) metatable: Cell<RawValue>,
    pub(crate) flag: TupleFlag,
}
impl Traceable for Tuple {
    fn trace(&self, gc: &GcState) {
        let values = self.values.borrow();
        for v in values.iter() {
            gc.trace(v);
        }
        gc.trace(&self.metatable.get());
    }
}

pub(crate) enum RawValue {
    Nil,
    Bool(bool),
    Number(OrderedFloat<f64>),
    Table(Gc<RefCell<Table>>),
    String(Gc<Managed<String>>),
    Closure(Gc<Closure>),
    Callable(Gc<Box<dyn Callable>>),
    Tuple(Gc<Tuple>),
    UserData(Gc<Box<dyn UserData>>),
}
impl Copy for RawValue {}
impl Clone for RawValue {
    fn clone(&self) -> Self {
        *self
    }
}
impl PartialEq for RawValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RawValue::Nil, RawValue::Nil) => true,
            (RawValue::Bool(a), RawValue::Bool(b)) => a == b,
            (RawValue::Number(a), RawValue::Number(b)) => a == b,
            (RawValue::Table(a), RawValue::Table(b)) => a.ptr_eq(b),
            (RawValue::Callable(a), RawValue::Callable(b)) => a.ptr_eq(b),
            (RawValue::Closure(a), RawValue::Closure(b)) => a.ptr_eq(b),
            (RawValue::String(a), RawValue::String(b)) => {
                if a.ptr_eq(b) {
                    return true;
                }
                {
                    let a = &(*a).data;
                    let b = &(*b).data;
                    a == b
                }
            }
            _ => false,
        }
    }
}

impl Hash for RawValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            RawValue::Nil => ().hash(state),
            RawValue::Bool(x) => x.hash(state),
            RawValue::Number(x) => x.hash(state),
            RawValue::Table(x) => std::ptr::hash(x.as_ptr(), state),
            RawValue::String(x) => {
                let s = &(*x).data;
                s.hash(state)
            }
            RawValue::Closure(x) => std::ptr::hash(x.as_ptr(), state),
            RawValue::Callable(x) => std::ptr::hash(x.as_ptr(), state),
            RawValue::Tuple(x) => std::ptr::hash(x.as_ptr(), state),
            RawValue::UserData(x) => std::ptr::hash(x.as_ptr(), state),
        }
    }
}
impl Eq for RawValue {}
// impl Hash for Value {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.data.hash(state);
//         std::ptr::hash(self.metatable.as_ptr(), state);
//     }
// }
// impl PartialEq for Value {
//     fn eq(&self, other: &Self) -> bool {
//         self.data == other.data && self.metatable.ptr_eq(&other.metatable)
//     }
// }
// impl Eq for Value {}
impl Traceable for RawValue {
    fn trace(&self, gc: &GcState) {
        match self {
            RawValue::Table(x) => gc.trace_ptr(*x),
            RawValue::String(x) => gc.trace_ptr(*x),
            RawValue::Closure(x) => gc.trace_ptr(*x),
            RawValue::Callable(x) => gc.trace_ptr(*x),
            RawValue::Tuple(x) => gc.trace_ptr(*x),
            _ => {}
        }
    }
}

impl RawValue {
    pub(crate) fn is_nil(&self) -> bool {
        match self {
            RawValue::Nil => true,
            _ => false,
        }
    }
    pub(crate) fn nil() -> Self {
        Default::default()
    }
    pub(crate) fn from_bool(x: bool) -> Self {
        RawValue::Bool(x)
    }
    pub(crate) fn from_number(x: f64) -> Self {
        RawValue::Number(OrderedFloat(x))
    }
    pub(crate) fn number(&self) -> Option<f64> {
        match self {
            RawValue::Number(x) => Some(x.0),
            RawValue::String(s) => {
                if let Ok(x) = (*s).data.parse::<f64>() {
                    Some(x)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    pub(crate) fn as_f64(&self) -> Option<&f64> {
        match &self {
            RawValue::Number(x) => Some(&x.0),
            _ => None,
        }
    }
    pub(crate) fn as_bool(&self) -> Option<&bool> {
        match &self {
            RawValue::Bool(x) => Some(x),
            _ => None,
        }
    }
    pub(crate) fn to_bool(&self) -> bool {
        match self {
            RawValue::Number(x) => *x != 0.0,
            RawValue::Bool(x) => *x,
            RawValue::Nil => false,
            _ => true,
        }
    }
    pub(crate) fn as_string<'a>(&'a self) -> Option<&'a String> {
        match self {
            RawValue::String(s) => unsafe { Some(&(*s.as_ptr()).data) },
            _ => None,
        }
    }
    pub(crate) fn as_table<'a>(&'a self) -> Option<&'a RefCell<Table>> {
        match self {
            RawValue::Table(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_closure<'a>(&'a self) -> Option<&'a Closure> {
        match self {
            RawValue::Closure(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_callable<'a>(&'a self) -> Option<&'a Box<dyn Callable>> {
        match self {
            RawValue::Callable(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_userdata<'a>(&'a self) -> Option<&'a dyn UserData> {
        match self {
            RawValue::UserData(t) => unsafe { Some(&(**t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_i64(&self) -> Option<i64> {
        let x = self.number()?;
        let fract = x.fract();
        if fract == 0.0 {
            Some(x.trunc() as i64)
        } else {
            None
        }
    }

    pub(crate) fn type_of(&self) -> &'static str {
        match self {
            RawValue::Nil => "nil",
            RawValue::Bool(_) => "boolean",
            RawValue::Number(_) => "number",
            RawValue::Table(_) => "table",
            RawValue::String(_) => "string",
            RawValue::Closure(_) => "function",
            RawValue::Callable(_) => "function",
            RawValue::Tuple(_) => "tuple",
            RawValue::UserData(x) => x.type_name(),
        }
    }
    pub(crate) fn print(&self) -> String {
        match self {
            RawValue::Nil => String::from("nil"),
            RawValue::Bool(t) => {
                if *t {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            RawValue::Number(x) => x.to_string(),
            RawValue::Table(table) => {
                format!("table: 0x{:0x}", table.as_ptr() as u64)
            }
            RawValue::String(s) => (*s).data.clone(),
            RawValue::Closure(closure) => {
                format!("function: 0x{:0x}", closure.as_ptr() as u64)
            }
            RawValue::Callable(callable) => {
                format!("function: 0x{:0x}", callable.as_ptr() as u64)
            }
            RawValue::UserData(p) => {
                format!("userdata: 0x{:0x}", p.as_ptr() as u64)
            }
            RawValue::Tuple(tuple) => {
                let mut s = String::from("(");
                let values = tuple.values.borrow();
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        s.push(',');
                    }
                    s.push_str(&format!("{}", v.print()));
                }
                s.push(')');
                s
            }
        }
    }
}

impl Default for RawValue {
    fn default() -> Self {
        RawValue::Nil
    }
}
