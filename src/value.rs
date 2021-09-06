use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

use ordered_float::OrderedFloat;

use crate::{
    bytecode::ByteCode,
    gc::{Gc, Traceable},
};

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

pub trait UserData {}
pub struct Closure {
    code: Vec<ByteCode>,
}
pub enum GcObjectData {
    Table(RefCell<Table>),
    String(RefCell<String>),
    Closure(RefCell<Closure>),
    UserData(*const dyn UserData),
}
pub struct GcObject {
    data: GcObjectData,
    next: Cell<*const dyn Traceable>,
}

impl Traceable for GcObject {
    fn set_next(&self, next: *const dyn Traceable) {
        self.next.set(next)
    }

    fn next(&self) -> *const dyn Traceable {
        self.next.get()
    }

    fn trace(&self, gc: &Gc) {
        todo!()
    }
}
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum ValueData {
    Nil,
    Bool(bool),
    Number(OrderedFloat<f64>),
    Ref(*const GcObject),
}
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Value {
    pub data: ValueData,
    pub metatable: *const GcObject,
}

impl Default for Value {
    fn default() -> Self {
        Self {
            data: ValueData::Nil,
            metatable: std::ptr::null(),
        }
    }
}
