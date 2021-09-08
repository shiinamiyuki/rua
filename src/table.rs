use std::collections::HashMap;

use crate::value::Value;

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}
