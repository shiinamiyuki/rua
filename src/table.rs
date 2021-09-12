use std::{
    collections::{
        hash_map::{DefaultHasher, RandomState},
        HashMap,
    },
    hash::{BuildHasher, Hash, Hasher},
};

use crate::{
    gc::Traceable,
    value::{Value, ValueData},
};

#[derive(Clone, Copy)]
struct Entry {
    key: Value,
    value: Value,
    prev: usize,
    next: usize,
}

impl Default for Entry {
    fn default() -> Self {
        Entry {
            key: Value::nil(),
            value: Value::nil(),
            prev: usize::MAX,
            next: usize::MAX,
        }
    }
}

pub(crate) struct LinkedHashMap {
    table: Vec<Entry>,
    len: usize,
    mod_mask: usize,
    s: RandomState,
    head: usize,
    during_rehash: bool,
    // tail:usize,
}

mod test {
    #[test]
    fn test_log() {
        use crate::log_2;
        for i in 0..62usize {
            assert_eq!(i as u32, log_2(1 << i).unwrap());
        }
    }
}
pub(crate) struct LinkedHashMapIter<'a> {
    map: &'a LinkedHashMap,
    i: usize,
}
impl<'a> Iterator for LinkedHashMapIter<'a> {
    type Item = (Value, Value);
    fn next(&mut self) -> Option<Self::Item> {
        if self.i == usize::MAX {
            None
        } else {
            let e = self.map.table[self.i];
            self.i = e.next;
            Some((e.key, e.value))
        }
    }
}

impl LinkedHashMap {
    pub(crate) fn iter<'a>(&'a self) -> LinkedHashMapIter<'a> {
        LinkedHashMapIter::<'a> {
            map: self,
            i: self.head,
        }
    }
    pub(crate) fn new() -> Self {
        Self {
            table: vec![],
            len: 0,
            s: RandomState::new(),
            head: usize::MAX,
            // tail:usize::MAX,
            mod_mask: 0,
            during_rehash: false,
        }
    }
    pub(crate) fn with_len(len: usize) -> Self {
        let mut m = Self::new();
        m.reset(len);
        m
    }
    fn reset(&mut self, len: usize) -> Vec<Entry> {
        assert!(len.is_power_of_two());
        self.len = 0;
        let old = std::mem::replace(&mut self.table, vec![Default::default(); len]);
        self.mod_mask = len - 1;
        // println!("{:0x} {:0x}", len, self.mod_mask);
        self.head = usize::MAX;
        old
    }
    fn rehash(&mut self, len: usize) {
        assert!(!self.during_rehash);
        self.during_rehash = true;
        assert!(self.len < len);
        let old = self.reset(len);
        for Entry { key, value, .. } in old {
            if !value.is_nil() {
                self.insert(key, value);
            }
        }
        self.during_rehash = false;
    }
    fn grow(&mut self) {
        // println!("grow");
        self.rehash(self.table.len() * 2);
    }
    fn hash(&self, k: &Value) -> u64 {
        let mut hasher = self.s.build_hasher();
        k.hash(&mut hasher);
        hasher.finish()
    }
    fn get_index(&self, k: &Value) -> Option<usize> {
        let hk = self.hash(k);
        for i in 0..self.table.len() {
            let h = (hk + i as u64 / 2 + (i * i) as u64 / 2) & self.mod_mask as u64;
            let entry = &self.table[h as usize];
            // println!("entry {} has {} {}", h, entry.key.print(), entry.value.print());
            if entry.value.is_nil() || entry.key == *k {
                return Some(h as usize);
            }
        }
        None
    }
    fn remove(&mut self, k: &Value) {
        let idx = self.get_index(k).unwrap();
        let entry = self.table[idx];
        let prev = entry.prev;
        let next = entry.next;
        if idx == self.head {
            self.head = next;
        }
        if prev != usize::MAX {
            self.table[prev].next = next;
        }
        if next != usize::MAX {
            self.table[next].prev = prev;
        }
        self.table[idx].value = Value::nil();
        self.len -= 1;
    }
    fn get(&self, key: Value) -> Value {
        // println!("get {}", key.print());
        if let Some(idx) = self.get_index(&key) {
            self.table[idx].value
        } else {
            Value::nil()
        }
    }
    fn insert(&mut self, k: Value, v: Value) {
        // println!("insert {} {}", k.print(), v.print());
        if self.table.is_empty() {
            self.reset(16);
        }
        if self.len * 2 > self.table.len() {
            self.grow();
        }
        if v.is_nil() {
            // effectively deleting the entry
            return self.remove(&k);
        } else {
            loop {
                if let Some(idx) = self.get_index(&k) {
                    let entry = Entry {
                        key: k,
                        value: v,
                        next: self.head,
                        ..Default::default()
                    };
                    if self.head != usize::MAX {
                        debug_assert!(!self.table[self.head].value.is_nil());
                        self.table[self.head].prev = idx;
                    }
                    self.head = idx;
                    self.table[idx] = entry;
                    self.len += 1;
                    // println!("insert to slot {}", idx);
                    break;
                } else {
                    self.grow();
                }
            }
        }
    }
}

pub struct Table {
    pub(crate) array: Vec<Value>,
    pub(crate) map: LinkedHashMap,
    largest_uint: u64,
    len: usize,
    need_recompute_len: bool,
}
fn is_int(x: f64) -> bool {
    x.fract() == 0.0
}
impl Table {
    pub fn new() -> Self {
        Self {
            array: vec![],
            map: LinkedHashMap::new(),
            largest_uint: 0,
            len: 0,
            need_recompute_len: false,
        }
    }
    pub fn new_with(array_part_len: usize, hash_part_len: usize) -> Self {
        Self {
            array: vec![Value::nil(); array_part_len],
            map: LinkedHashMap::with_len(hash_part_len),
            largest_uint: array_part_len as u64,
            len: array_part_len,
            need_recompute_len: false,
        }
    }
    pub fn len(&mut self) -> usize {
        if !self.need_recompute_len {
            self.len
        } else {
            self.need_recompute_len = false;
            for (i, v) in self.array.iter().enumerate() {
                if v.is_nil() {
                    self.len = i + 1;
                    return self.len;
                }
            }
            for i in (self.array.len() + 1) as u64..=self.largest_uint {
                let v = Value::from_number(i as f64);
                if self.get(v).is_nil() {
                    self.len = i as usize;
                    return self.len;
                }
            }
            self.len = self.largest_uint as usize;
            self.len
        }
    }
    pub fn get(&self, key: Value) -> Value {
        match key.data {
            ValueData::Number(x) if is_int(x.0) => {
                let i = x.trunc() as i64;
                if i >= 1 && i <= self.array.len() as i64 {
                    return self.array[(i - 1) as usize];
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
                if !value.is_nil() {
                    self.largest_uint = self.largest_uint.max(i as u64);
                }
                if i >= 1 && i <= self.array.len() as i64 {
                    if value.is_nil() {
                        self.need_recompute_len = true;
                    }
                    self.array[(i - 1) as usize] = value;
                    return;
                } else if i == 1 + (self.array.len() as i64) && !value.is_nil() {
                    self.array.push(value);
                    self.need_recompute_len = true;
                    return;
                } else {
                    self.need_recompute_len = true;
                }
            }
            _ => {}
        }
        self.map.insert(key, value);
    }
}

impl Traceable for Table {
    fn trace(&self, gc: &crate::gc::Gc) {
        for i in &self.array {
            gc.trace(i);
        }
        for (k, v) in self.map.iter() {
            gc.trace(&k);
            gc.trace(&v);
        }
    }
}
