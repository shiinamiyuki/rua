use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::Hash,
    rc::Rc,
};

use ordered_float::OrderedFloat;

use crate::{
    bytecode::ByteCodeModule,
    closure::{Callable, Closure},
    gc::{Gc, Traceable},
    state::State,
    table::Table,
};

pub trait UserData {}

pub struct Managed<T> {
    pub data: T,
}

impl<T> Traceable for Managed<T> {
    fn trace(&self, _gc: &Gc) {}
}
pub type ManagedCell<T> = Managed<RefCell<T>>;

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
    pub values: Vec<Value>,
    pub unpack: TupleUnpack,
}
#[derive(Clone, Copy)]
pub enum ValueData {
    Nil,
    Bool(bool),
    Number(OrderedFloat<f64>),
    Table(*const ManagedCell<Table>),
    String(*const Managed<String>),
    Closure(*const Managed<Closure>),
    Callable(*const Managed<Box<dyn Callable>>),
    Tuple(*const Managed<Tuple>),
}
impl PartialEq for ValueData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueData::Nil, ValueData::Nil) => true,
            (ValueData::Bool(a), ValueData::Bool(b)) => a == b,
            (ValueData::Number(a), ValueData::Number(b)) => a == b,
            (ValueData::Table(a), ValueData::Table(b)) => a == b,
            (ValueData::Callable(a), ValueData::Callable(b)) => a == b,
            (ValueData::Closure(a), ValueData::Closure(b)) => a == b,
            (ValueData::String(a), ValueData::String(b)) => {
                if a == b {
                    return true;
                }
                unsafe {
                    let a = &(**a).data;
                    let b = &(**b).data;
                    a == b
                }
            }
            _ => false,
        }
    }
}
impl Eq for ValueData {}
impl Hash for ValueData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ValueData::Nil => ().hash(state),
            ValueData::Bool(x) => x.hash(state),
            ValueData::Number(x) => x.hash(state),
            ValueData::Table(x) => std::ptr::hash(*x, state),
            ValueData::String(x) => unsafe {
                let s = &(**x).data;
                s.hash(state)
            },
            ValueData::Closure(x) => std::ptr::hash(*x, state),
            ValueData::Callable(x) => std::ptr::hash(*x, state),
            ValueData::Tuple(x) => std::ptr::hash(*x, state),
        }
    }
}
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Value {
    pub data: ValueData,
    pub metatable: *const ManagedCell<Table>,
}

impl Value {
    pub fn nil() -> Self {
        Default::default()
    }
    pub fn from_bool(x: bool) -> Self {
        Self {
            data: ValueData::Bool(x),
            metatable: std::ptr::null(),
        }
    }
    pub fn from_number(x: f64) -> Self {
        Self {
            data: ValueData::Number(OrderedFloat(x)),
            metatable: std::ptr::null(),
        }
    }
    pub fn number(&self) -> Option<f64> {
        match self.data {
            ValueData::Number(x) => Some(x.0),
            ValueData::String(s) => unsafe {
                if let Ok(x) = (*s).data.parse::<f64>() {
                    Some(x)
                } else {
                    None
                }
            },
            _ => None,
        }
    }
    pub(crate) fn as_string<'a>(&'a self) -> Option<&'a String> {
        match self.data {
            ValueData::String(s) => unsafe { Some(&(*s).data) },
            _ => None,
        }
    }
    pub(crate) fn as_table<'a>(&'a self) -> Option<&'a RefCell<Table>> {
        match self.data {
            ValueData::Table(t) => unsafe { Some(&(*t).data) },
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64> {
        let x = self.number()?;
        let fract = x.fract();
        if fract == 0.0 {
            Some(x.trunc() as i64)
        } else {
            None
        }
    }
    pub fn type_of(&self) -> &'static str {
        match self.data {
            ValueData::Nil => "nil",
            ValueData::Bool(_) => "boolean",
            ValueData::Number(_) => "number",
            ValueData::Table(_) => "table",
            ValueData::String(_) => "string",
            ValueData::Closure(_) => "function",
            ValueData::Callable(_) => "function",
            ValueData::Tuple(_) => "tuple",
        }
    }
    pub fn print(&self) -> String {
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
                format!("table: {:0x}", table as u64)
            }
            ValueData::String(s) => unsafe { (*s).data.clone() },
            ValueData::Closure(closure) => {
                format!("function: {:0x}", closure as u64)
            }
            ValueData::Callable(callable) => {
                format!("function: {:0x}", callable as u64)
            }
            ValueData::Tuple(tuple) => {
                let mut s = String::from("(");
                for (i, v) in unsafe { (*tuple).data.values.iter().enumerate() } {
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
            metatable: std::ptr::null(),
        }
    }
}
