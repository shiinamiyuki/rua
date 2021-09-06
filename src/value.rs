use std::{collections::HashMap, sync::Arc};

use ordered_float::OrderedFloat;

pub struct Table {
    array: Vec<Value>,
    map: HashMap<Value, Value>,
}


#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum Value {
    Number(OrderedFloat<f64>),
    String(*mut String),
    Table(*mut Table),
}
