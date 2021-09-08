use std::collections::HashMap;

use crate::value::{Value, ValueData};

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}
fn is_int(x: f64) -> bool {
    x.fract() == 0.0
}
impl Table {
    pub fn new() -> Self {
        Self {
            array: vec![],
            map: HashMap::new(),
        }
    }
    pub fn get(&self, key: Value) -> Option<Value> {
        match key.data {
            ValueData::Number(x) if is_int(x.0) => {
                let i = x.trunc() as i64;
                if i >= 1 && i <= self.array.len() as i64 {
                    return Some(self.array[(i - 1) as usize]);
                }
            }
            _ => {}
        }
        self.map.get(&key).map(|x| *x)
    }
    pub fn set(&mut self, key: Value, value: Value) {
        match key.data {
            ValueData::Number(x) if is_int(x.0) => {
                let i = x.trunc() as i64;
                if i >= 1 && i <= self.array.len() as i64 {
                    self.array[(i - 1) as usize] = value;
                    return;
                } else if i >= 1 && (i < self.array.len() as i64 + 4 || i <= 17) {
                    self.array.resize(i as usize, Value::nil());
                    self.array[(i - 1) as usize] = value;
                    return;
                }
            }
            _ => {}
        }
        self.map.insert(key, value);
    }
}
