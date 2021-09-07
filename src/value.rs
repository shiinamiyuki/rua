use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use ordered_float::OrderedFloat;

use crate::{
    bytecode::ByteCodeModule,
    gc::{Gc, Traceable},
    state::State,
};

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

pub trait UserData {}
pub trait Callable {
    fn call(&mut self, state: &State);
}
pub struct Closure {
    pub(crate) entry: usize,
    pub(crate) n_locals: usize,
    pub(crate) module: Rc<ByteCodeModule>,
    pub(crate) upvalues: RefCell<Vec<Value>>,
}

pub struct Managed<T> {
    data: T,
}

impl<T: Traceable + 'static> Traceable for Managed<T> {
    fn trace(&self, gc: &Gc) {}
}
pub type ManagedCell<T> = Managed<RefCell<T>>;
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum ValueData {
    Nil,
    Bool(bool),
    Number(OrderedFloat<f64>),
    Table(*const ManagedCell<Table>),
    String(*const Managed<String>),
    Closure(*const Managed<Closure>),
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
