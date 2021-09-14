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
}
impl Traceable for Tuple {
    fn trace(&self, gc: &GcState) {
        for v in &self.values {
            gc.trace(v);
        }
    }
}
pub enum Number {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
}
pub enum ValueData {
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
impl Copy for ValueData {}
impl Clone for ValueData {
    fn clone(&self) -> Self {
        *self
    }
}
impl PartialEq for ValueData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueData::Nil, ValueData::Nil) => true,
            (ValueData::Bool(a), ValueData::Bool(b)) => a == b,
            (ValueData::Number(a), ValueData::Number(b)) => a == b,
            (ValueData::Table(a), ValueData::Table(b)) => a.ptr_eq(b),
            (ValueData::Callable(a), ValueData::Callable(b)) => a.ptr_eq(b),
            (ValueData::Closure(a), ValueData::Closure(b)) => a.ptr_eq(b),
            (ValueData::String(a), ValueData::String(b)) => {
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

impl Hash for ValueData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ValueData::Nil => ().hash(state),
            ValueData::Bool(x) => x.hash(state),
            ValueData::Number(x) => x.hash(state),
            ValueData::Table(x) => std::ptr::hash(x.as_ptr(), state),
            ValueData::String(x) => {
                let s = &(*x).data;
                s.hash(state)
            }
            ValueData::Closure(x) => std::ptr::hash(x.as_ptr(), state),
            ValueData::Callable(x) => std::ptr::hash(x.as_ptr(), state),
            ValueData::Tuple(x) => std::ptr::hash(x.as_ptr(), state),
            ValueData::UserData(x) => std::ptr::hash(x.as_ptr(), state),
        }
    }
}
impl Eq for ValueData {}
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct Value {
    pub(crate) data: ValueData,
    pub(crate) metatable: Option<Gc<RefCell<Table>>>,
}
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
        match self.data {
            ValueData::Table(x) => gc.trace_ptr(x),
            ValueData::String(x) => gc.trace_ptr(x),
            ValueData::Closure(x) => gc.trace_ptr(x),
            ValueData::Callable(x) => gc.trace_ptr(x),
            ValueData::Tuple(x) => gc.trace_ptr(x),
            _ => {}
        }
        if let Some(p) = self.metatable {
            gc.trace_ptr(p);
        }
    }
}

impl Value {
    pub(crate) fn is_nil(&self) -> bool {
        match self.data {
            ValueData::Nil => true,
            _ => false,
        }
    }
    pub(crate) fn nil() -> Self {
        Default::default()
    }
    pub(crate) fn from_bool(x: bool) -> Self {
        Self {
            data: ValueData::Bool(x),
            metatable: None,
        }
    }
    pub(crate) fn from_number(x: f64) -> Self {
        Self {
            data: ValueData::Number(OrderedFloat(x)),
            metatable: None,
        }
    }
    pub(crate) fn number(&self) -> Option<f64> {
        match self.data {
            ValueData::Number(x) => Some(x.0),
            ValueData::String(s) => {
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
        match &self.data {
            ValueData::Number(x) => Some(&x.0),
            _ => None,
        }
    }
    pub(crate) fn as_bool(&self) -> Option<&bool> {
        match &self.data {
            ValueData::Bool(x) => Some(x),
            _ => None,
        }
    }
    pub(crate) fn to_bool(&self) -> bool {
        match self.data {
            ValueData::Number(x) => x != 0.0,
            ValueData::Bool(x) => x,
            ValueData::Nil => false,
            _ => true,
        }
    }
    pub(crate) fn as_string<'a>(&'a self) -> Option<&'a String> {
        match self.data {
            ValueData::String(s) => unsafe { Some(&(*s.as_ptr()).data) },
            _ => None,
        }
    }
    pub(crate) fn as_table<'a>(&'a self) -> Option<&'a RefCell<Table>> {
        match self.data {
            ValueData::Table(t) => unsafe { Some(&(*t.as_ptr())) },
            _ => None,
        }
    }
    pub(crate) fn as_userdata<'a>(&'a self) -> Option<&'a dyn UserData> {
        match self.data {
            ValueData::UserData(t) => unsafe { Some(&(**t.as_ptr())) },
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
        match self.data {
            ValueData::Nil => "nil",
            ValueData::Bool(_) => "boolean",
            ValueData::Number(_) => "number",
            ValueData::Table(_) => "table",
            ValueData::String(_) => "string",
            ValueData::Closure(_) => "function",
            ValueData::Callable(_) => "function",
            ValueData::Tuple(_) => "tuple",
            ValueData::UserData(x) => x.type_name(),
        }
    }
    pub(crate) fn print(&self) -> String {
        match self.data {
            ValueData::Nil => String::from("nil"),
            ValueData::Bool(t) => {
                if t {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            ValueData::Number(x) => x.to_string(),
            ValueData::Table(table) => {
                format!("table: 0x{:0x}", table.as_ptr() as u64)
            }
            ValueData::String(s) => (*s).data.clone(),
            ValueData::Closure(closure) => {
                format!("function: 0x{:0x}", closure.as_ptr() as u64)
            }
            ValueData::Callable(callable) => {
                format!("function: 0x{:0x}", callable.as_ptr() as u64)
            }
            ValueData::UserData(p) => {
                format!("userdata: 0x{:0x}", p.as_ptr() as u64)
            }
            ValueData::Tuple(tuple) => {
                let mut s = String::from("(");
                for (i, v) in  (*tuple).values.iter().enumerate()  {
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
        Self {
            data: ValueData::Nil,
            metatable: None,
        }
    }
}
