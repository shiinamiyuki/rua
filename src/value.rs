use std::{
    any::Any,
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::Hash,
    rc::Rc,
};

use ordered_float::OrderedFloat;

use crate::{
    bytecode::ByteCodeModule,
    closure::{Callable, Closure},
    gc::{Gc, GcState, Traceable},
    state::State,
    table::Table,
};

pub struct Managed<T: ?Sized + 'static> {
    pub data: T,
}
pub type LightUserData<T> = Managed<T>;
impl<T> Managed<T> {
    pub fn new(data: T) -> Self {
        Self { data }
    }
}
impl<T> Traceable for Managed<T> {
    fn trace(&self, _gc: &GcState) {}
}

pub type ManagedCell<T> = Managed<RefCell<T>>;

pub trait UserData: Traceable + Any {
    fn as_traceable(&self) -> &dyn Traceable;
    fn as_any(&self) -> &dyn Any;
    fn type_name(&self) -> &'static str;
}
impl<T> UserData for Managed<T> {
    fn as_traceable(&self) -> &dyn Traceable {
        self
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn type_name(&self) -> &'static str {
        std::any::type_name::<T>()
    }
}
impl Traceable for Box<dyn UserData> {
    fn trace(&self, gc: &GcState) {
        self.as_ref().trace(gc)
    }
}

/*
function f(x) return 1,2 end
function g(x) return tuple(1,2) end

x,y = f(x) -- x:1, y:2
x,y = g(x) -- x:tuple(1,2), y:nil
x,y = tuple.unpack(f(x)) -- x:1,y:2
x,y = tuple.unpack(g(x)) -- x:1,y:2

x,y = tuple(1,2) -- x: tuple(1,2), y:nil
x,y= tuple.unpack(tuple(1, 2)) -- good
x = tuple.unpack(tuple(1,2)) --error

Exact unpack:
Upon assignment and paremeter passing, tuple is treated as in Python
tuple(args...) is always Exact
args, ... = tuple.unpack(values, ...) is always Exact

TruncateFill:
Upon assignment and paremeter passing, tuple is treated as in Lua
args... is always TruncateFill
args, ... = values, ... is always TruncateFill

unpack(tuple) does unpacking as if tuple were TruncateFill

tuple.unpack(tuple) does unpacking as if tuple were Exact

*/
#[derive(Clone, Copy)]
pub enum TupleUnpack {
    Exact,
    TruncateFill,
}
pub struct Tuple {
    pub(crate) values: Vec<Value>,
    pub(crate) unpack: TupleUnpack,
    pub(crate) metatable: Cell<Value>,
}
impl Traceable for Tuple {
    fn trace(&self, gc: &GcState) {
        for v in &self.values {
            gc.trace(v);
        }
        gc.trace(&self.metatable.get());
    }
}

pub(crate) enum Value {
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
impl Copy for Value {}
impl Clone for Value {
    fn clone(&self) -> Self {
        *self
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Table(a), Value::Table(b)) => a.ptr_eq(b),
            (Value::Callable(a), Value::Callable(b)) => a.ptr_eq(b),
            (Value::Closure(a), Value::Closure(b)) => a.ptr_eq(b),
            (Value::String(a), Value::String(b)) => {
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

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => ().hash(state),
            Value::Bool(x) => x.hash(state),
            Value::Number(x) => x.hash(state),
            Value::Table(x) => std::ptr::hash(x.as_ptr(), state),
            Value::String(x) => {
                let s = &(*x).data;
                s.hash(state)
            }
            Value::Closure(x) => std::ptr::hash(x.as_ptr(), state),
            Value::Callable(x) => std::ptr::hash(x.as_ptr(), state),
            Value::Tuple(x) => std::ptr::hash(x.as_ptr(), state),
            Value::UserData(x) => std::ptr::hash(x.as_ptr(), state),
        }
    }
}
impl Eq for Value {}
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
impl Traceable for Value {
    fn trace(&self, gc: &GcState) {
        match self {
            Value::Table(x) => gc.trace_ptr(*x),
            Value::String(x) => gc.trace_ptr(*x),
            Value::Closure(x) => gc.trace_ptr(*x),
            Value::Callable(x) => gc.trace_ptr(*x),
            Value::Tuple(x) => gc.trace_ptr(*x),
            _ => {}
        }
    }
}

impl Value {
    pub(crate) fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }
    pub(crate) fn nil() -> Self {
        Default::default()
    }
    pub(crate) fn from_bool(x: bool) -> Self {
        Value::Bool(x)
    }
    pub(crate) fn from_number(x: f64) -> Self {
        Value::Number(OrderedFloat(x))
    }
    pub(crate) fn number(&self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(x.0),
            Value::String(s) => {
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
            Value::Number(x) => Some(&x.0),
            _ => None,
        }
    }
    pub(crate) fn as_bool(&self) -> Option<&bool> {
        match &self {
            Value::Bool(x) => Some(x),
            _ => None,
        }
    }
    pub(crate) fn to_bool(&self) -> bool {
        match self {
            Value::Number(x) => *x != 0.0,
            Value::Bool(x) => *x,
            Value::Nil => false,
            _ => true,
        }
    }
    pub(crate) fn as_string<'a>(&'a self) -> Option<&'a String> {
        match self {
            Value::String(s) => unsafe { Some(&(*s.as_ptr()).data) },
            _ => None,
        }
    }
    pub(crate) fn as_table<'a>(&'a self) -> Option<&'a RefCell<Table>> {
        match self {
            Value::Table(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_closure<'a>(&'a self) -> Option<&'a Closure> {
        match self {
            Value::Closure(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_callable<'a>(&'a self) -> Option<&'a Box<dyn Callable>> {
        match self {
            Value::Callable(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_userdata<'a>(&'a self) -> Option<&'a dyn UserData> {
        match self {
            Value::UserData(t) => unsafe { Some(&(**t.as_ptr())) },
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
            Value::Nil => "nil",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::Table(_) => "table",
            Value::String(_) => "string",
            Value::Closure(_) => "function",
            Value::Callable(_) => "function",
            Value::Tuple(_) => "tuple",
            Value::UserData(x) => x.type_name(),
        }
    }
    pub(crate) fn print(&self) -> String {
        match self {
            Value::Nil => String::from("nil"),
            Value::Bool(t) => {
                if *t {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            Value::Number(x) => x.to_string(),
            Value::Table(table) => {
                format!("table: 0x{:0x}", table.as_ptr() as u64)
            }
            Value::String(s) => (*s).data.clone(),
            Value::Closure(closure) => {
                format!("function: 0x{:0x}", closure.as_ptr() as u64)
            }
            Value::Callable(callable) => {
                format!("function: 0x{:0x}", callable.as_ptr() as u64)
            }
            Value::UserData(p) => {
                format!("userdata: 0x{:0x}", p.as_ptr() as u64)
            }
            Value::Tuple(tuple) => {
                let mut s = String::from("(");
                for (i, v) in (*tuple).values.iter().enumerate() {
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

impl Default for Value {
    fn default() -> Self {
        Value::Nil
    }
}
