use std::{
    collections::hash_map::RandomState,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
};

use parking_lot::RwLock;

use crate::{error::Error, gc::Traceable, value::Value};

#[derive(Clone, Copy)]
struct Entry {
    key: Value,
    value: Value,
    prev: usize,
    next: usize,
}
impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.prev, self.next)
    }
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
    // fn check_cycle(&self) -> bool {
    //     let mut fast = self.iter();
    //     let mut slow = self.iter();
    //     let mut prev = usize::MAX;
    //     let r = loop {
    //         fast.next();
    //         let prev= slow.i;
    //         let a = fast.next();
    //         let b = slow.next();

    //         if a.is_none() || b.is_none() {
    //             break false;
    //         }
    //         if self.table[slow.i].prev!= prev{
    //             println!("head={} slow.i={}", self.head, slow.i);
    //             for (i, e) in self.table.iter().enumerate() {
    //                 println!("{} {}", i, e);
    //             }
    //         }
    //         assert_eq!(self.table[slow.i].prev, prev);
    //         if prev != usize::MAX {
    //             assert_eq!(self.table[prev].next, slow.i);
    //         }
    //         if fast.i == slow.i {
    //             break true;
    //         }
    //     };
    //     if r {
    //         let mut it = self.iter();
    //         println!("head={}", self.head);
    //         for i in 0..10 {
    //             let i = it.i;
    //             println!("{}", i);
    //             if it.next().is_none() {
    //                 break;
    //             }
    //         }
    //     }
    //     r
    // }
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
        if self.table[idx].value.is_nil() {
            return;
        }
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
        self.table[idx] = Entry::default();
        self.len -= 1;
        // debug_assert!(!self.check_cycle());
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
            // debug_assert!(!self.check_cycle());
        }
        if self.len * 3 > self.table.len() {
            self.grow();
            // debug_assert!(!self.check_cycle());
        }
        if v.is_nil() {
            // effectively deleting the entry
            return self.remove(&k);
        } else {
            loop {
                if let Some(idx) = self.get_index(&k) {
                    // debug_assert!(!self.check_cycle());
                    if !self.table[idx].value.is_nil() {
                        self.table[idx].value = v;
                        // debug_assert!(!self.check_cycle());
                    } else {
                        let entry = Entry {
                            key: k,
                            value: v,
                            next: self.head,
                            prev: usize::MAX,
                        };
                        if self.head != usize::MAX {
                            // debug_assert!(!self.table[self.head].value.is_nil());
                            self.table[self.head].prev = idx;
                        }
                        self.head = idx;
                        self.table[idx] = entry;
                        self.len += 1;
                        // debug_assert!(!self.check_cycle());
                        // println!("insert to slot {}", idx);
                    }
                    break;
                } else {
                    self.grow();
                    // debug_assert!(!self.check_cycle());
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
    pub(crate) metatable: Value,
}
fn is_int(x: f64) -> bool {
    x.fract() == 0.0
}
impl Table {
    pub(crate) fn new() -> Self {
        Self {
            array: vec![],
            map: LinkedHashMap::new(),
            largest_uint: 0,
            len: 0,
            need_recompute_len: false,
            metatable: Value::Nil,
        }
    }
    pub(crate) fn next(&self, key: Value) -> Result<Value, Error> {
        if key.is_nil() {
            if !self.array.is_empty() {
                return Ok(Value::from(1));
            } else if !self.map.table.is_empty() {
                return Ok(self.map.table[self.map.head].key);
            } else {
                return Ok(Value::nil());
            }
        }
        match key {
            Value::Number(x) if is_int(x.0) => {
                let i = x.trunc() as i64;
                if i >= 1 && i < self.array.len() as i64 {
                    return Ok(Value::from(i + 1));
                }
            }
            _ => {}
        }
        if let Some(i) = self.map.get_index(&key) {
            let entry = self.map.table[i];
            if entry.value.is_nil() {
                Err(Error {
                    kind: crate::error::ErrorKind::TypeError,
                    msg: format!("invalid key to 'next', key:{}", key.to_debug_string()),
                })
            } else {
                if entry.next != usize::MAX {
                    Ok(self.map.table[entry.next].key)
                } else {
                    Ok(Value::nil())
                }
            }
        } else {
            Ok(Value::nil())
        }
    }
    pub(crate) fn new_with(array_part_len: usize, hash_part_len: usize) -> Self {
        Self {
            array: vec![Value::nil(); array_part_len],
            map: LinkedHashMap::with_len(hash_part_len),
            largest_uint: array_part_len as u64,
            len: array_part_len,
            need_recompute_len: false,
            metatable: Value::Nil,
        }
    }
    pub(crate) fn len(&mut self) -> usize {
        if !self.need_recompute_len {
            self.len
        } else {
            self.need_recompute_len = false;
            for (i, v) in self.array.iter().enumerate() {
                if v.is_nil() {
                    self.len = i;
                    return self.len;
                }
            }
            for i in (self.array.len() + 1) as u64..=self.largest_uint {
                let v = Value::from(i as i64);
                if self.get(v).is_nil() {
                    self.len = i as usize - 1;
                    return self.len;
                }
            }
            self.len = self.largest_uint as usize;
            self.len
        }
    }
    pub(crate) fn get(&self, key: Value) -> Value {
        match key {
            Value::Number(x) if is_int(x.0) => {
                let i = x.trunc() as i64;
                if i >= 1 && i <= self.array.len() as i64 {
                    return self.array[(i - 1) as usize];
                }
            }
            _ => {}
        }
        self.map.get(key)
    }
    pub(crate) fn set(&mut self, key: Value, value: Value) {
        match key {
            Value::Number(x) if is_int(x.0) => {
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

unsafe impl Traceable for Table {
    fn trace(&self, ctx: &mut crate::gc::TraceContext) {
        for i in &self.array {
            ctx.trace(i);
        }
        for (k, v) in self.map.iter() {
            ctx.trace(&k);
            ctx.trace(&v);
        }
    }
}

unsafe impl Traceable for RwLock<Table> {
    fn trace(&self, ctx: &mut crate::gc::TraceContext) {
        let t = self.read();
        ctx.trace(&*t);
    }
}
