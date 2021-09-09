use std::{
    collections::{
        hash_map::{DefaultHasher, RandomState},
        HashMap,
    },
    hash::{BuildHasher, Hash, Hasher},
};

use crate::value::{Value, ValueData};

#[derive(Clone, Copy)]
struct Entry {
    key: Value,
    value: Value,
    next: usize,
}

impl Default for Entry {
    fn default() -> Self {
        Entry {
            key: Value::nil(),
            value: Value::nil(),
            next: usize::MAX,
        }
    }
}

struct LinkedHashMap {
    table: Vec<Entry>,
    free_pos: usize,
    s: RandomState,
}

impl LinkedHashMap {
    pub fn new() -> Self {
        Self {
            table: vec![],
            free_pos: 0,
            s: RandomState::new(),
        }
    }
    fn init_table_clear(&mut self, len: usize) {
        self.table = vec![Default::default(); len];
        self.free_pos = self.table.len();
    }
    fn hash(&self, k: &Value) -> u64 {
        let mut hasher = self.s.build_hasher();
        k.hash(&mut hasher);
        hasher.finish()
    }
    fn get_free_pos(&mut self) -> Option<usize> {
        if self.free_pos == 0 {
            None
        } else {
            let i = self.free_pos - 1;
            self.free_pos -= 1;
            Some(i)
        }
    }
    fn get(&self, k: Value) -> Option<Value> {
        if self.table.is_empty() {
            return None;
        }
        let mp = (self.hash(&k) as usize) % self.table.len();
        let mut p = self.table[mp];
        while !p.value.is_nil() {
            if p.key == k {
                return Some(p.value);
            }
            if p.next != usize::MAX {
                p = self.table[p.next];
            }
        }
        None
    }
    fn rehash(&mut self) {
        let new_len = (self.table.len() as f64 * 1.8) as usize;
        let pairs: Vec<_> = self
            .table
            .iter()
            .filter(|e| !e.value.is_nil())
            .map(|e| (e.key, e.value))
            .collect();
        self.table.resize(new_len, Default::default());
        self.free_pos = self.table.len();
        for p in pairs {
            let suc = self.insert_impl(p.0, p.1);
            debug_assert!(suc);
        }
    }
    fn insert(&mut self, k: Value, v: Value) {
        loop {
            if self.insert_impl(k, v) {
                break;
            }
            self.rehash();
        }
    }
    fn insert_impl(&mut self, k: Value, v: Value) -> bool {
        if self.table.is_empty() {
            self.init_table_clear(16);
        }
        let mp = (self.hash(&k) as usize) % self.table.len();
        let entry = self.table[mp];
        if !entry.value.is_nil() {
            if self.hash(&entry.key) as usize == mp {
                if let Some(i) = self.get_free_pos() {
                    self.table[mp].next = i;
                    self.table[i] = Entry {
                        key: k,
                        value: v,
                        next: entry.next,
                    };
                    true
                } else {
                    false
                }
            } else {
                if let Some(i) = self.get_free_pos() {
                    self.table[i] = self.table[mp];
                    self.table[i] = Entry {
                        key: k,
                        value: v,
                        next: usize::MAX,
                    };
                    true
                } else {
                    false
                }
            }
        } else {
            self.table[mp] = Entry {
                key: k,
                value: v,
                next: usize::MAX,
            };
            true
        }
    }
}

pub struct Table {
    array: Vec<Value>,
    map: LinkedHashMap,
    free_pos: u32,
}
fn is_int(x: f64) -> bool {
    x.fract() == 0.0
}
impl Table {
    pub fn new() -> Self {
        Self {
            array: vec![],
            map: LinkedHashMap::new(),
            free_pos: 0,
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
        self.map.get(key)
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
