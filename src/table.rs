//! Lua table: hybrid array + open-addressing hash map.

use crate::gc::{GcObjectKind, GcRef};
use crate::value::Value;

/// A hash table entry: key and value.
#[derive(Clone, Copy)]
pub(crate) struct HashEntry {
    pub(crate) key: Value,
    pub(crate) val: Value,
}

/// A Lua table: the primary data structure in Lua.
///
/// Combines an array part (for consecutive integer keys starting at 1)
/// with an open-addressing hash part (power-of-2 sizing, linear probing).
pub struct Table {
    /// Array part: consecutive integer keys \[1..n\]
    pub(crate) array: Vec<Value>,
    /// Hash part: open-addressing with power-of-2 sizing.
    /// None = empty slot, Some = occupied.
    pub(crate) hash: Vec<Option<HashEntry>>,
    /// Number of entries in the hash part.
    hash_used: usize,
    /// log2 of hash capacity (capacity = 1 << hash_log2). 0 means empty hash.
    hash_log2: u8,
    /// Metatable (if any)
    pub(crate) metatable: Option<GcRef>,
}

impl Table {
    /// Create a new empty table.
    pub fn new() -> Self {
        Table {
            array: Vec::new(),
            hash: Vec::new(),
            hash_used: 0,
            hash_log2: 0,
            metatable: None,
        }
    }

    /// Create a new table with pre-allocated capacity.
    pub fn with_capacity(array_cap: usize, hash_cap: usize) -> Self {
        let (hash, hash_log2) = if hash_cap > 0 {
            let log2 = hash_cap.next_power_of_two().trailing_zeros() as u8;
            let cap = 1usize << log2;
            (vec![None; cap], log2)
        } else {
            (Vec::new(), 0)
        };
        Table {
            array: Vec::with_capacity(array_cap),
            hash,
            hash_used: 0,
            hash_log2,
            metatable: None,
        }
    }

    /// Normalize a key: canonicalize float-integer keys, reject nil/NaN.
    fn normalize_key(key: Value) -> Option<Value> {
        match key {
            Value::Nil => None,
            Value::Float(f) if f.is_nan() => None,
            Value::Float(f) => {
                let i = f as i64;
                if i as f64 == f {
                    Some(Value::Integer(i))
                } else {
                    Some(Value::Float(f))
                }
            }
            other => Some(other),
        }
    }

    /// Hash a Value key to a usize.
    fn hash_value(key: &Value) -> u64 {
        match key {
            Value::Integer(i) => {
                // Fibonacci hashing for integers
                (*i as u64).wrapping_mul(11400714819323198485)
            }
            Value::Float(f) => {
                // Hash the raw bits
                let bits = f.to_bits();
                bits.wrapping_mul(11400714819323198485)
            }
            Value::Boolean(b) => {
                if *b { 1 } else { 0 }
            }
            Value::Object(r) => {
                // For strings, use the precomputed hash. For others, use pointer.
                match &r.as_object().kind {
                    GcObjectKind::String(s) => s.hash(),
                    _ => (r.ptr_value() as u64).wrapping_mul(11400714819323198485),
                }
            }
            Value::Nil => 0, // shouldn't happen (nil keys are rejected)
        }
    }

    // ── Hash part operations ───────────────────────────────────────

    /// Find the slot index for a key in the hash part, or the first empty slot.
    /// Returns (index, found) where found indicates if the key was found.
    fn hash_find(&self, key: &Value) -> Option<(usize, bool)> {
        if self.hash.is_empty() {
            return None;
        }
        let mask = (1usize << self.hash_log2) - 1;
        let h = Self::hash_value(key) as usize;
        let mut i = h & mask;
        let start = i;
        loop {
            match &self.hash[i] {
                Some(entry) => {
                    if entry.key == *key {
                        return Some((i, true));
                    }
                }
                None => {
                    return Some((i, false));
                }
            }
            i = (i + 1) & mask;
            if i == start {
                // Table is full (shouldn't happen with proper resizing)
                return None;
            }
        }
    }

    /// Get a value from the hash part.
    fn hash_get(&self, key: &Value) -> Value {
        match self.hash_find(key) {
            Some((idx, true)) => self.hash[idx].as_ref().unwrap().val,
            _ => Value::Nil,
        }
    }

    /// Set a value in the hash part. Handles insert, update, and delete (nil value).
    fn hash_set(&mut self, key: Value, value: Value) {
        // Delete case: setting to nil
        if value.is_nil() {
            self.hash_remove(&key);
            return;
        }

        // Check if we need to grow (load factor > 75%)
        if self.hash.is_empty() || self.hash_used * 4 >= self.hash.len() * 3 {
            self.hash_grow();
        }

        match self.hash_find(&key) {
            Some((idx, true)) => {
                // Update existing entry
                self.hash[idx].as_mut().unwrap().val = value;
            }
            Some((idx, false)) => {
                // Insert into empty slot
                self.hash[idx] = Some(HashEntry { key, val: value });
                self.hash_used += 1;
            }
            None => {
                // Table is full — shouldn't happen after grow, but safety fallback
                self.hash_grow();
                self.hash_set(key, value);
            }
        }
    }

    /// Remove a key from the hash part.
    fn hash_remove(&mut self, key: &Value) {
        if let Some((idx, true)) = self.hash_find(key) {
            // Remove the entry and re-insert displaced entries (backward shift deletion)
            self.hash[idx] = None;
            self.hash_used -= 1;
            let mask = (1usize << self.hash_log2) - 1;
            let mut j = (idx + 1) & mask;
            while let Some(entry) = self.hash[j] {
                self.hash[j] = None;
                self.hash_used -= 1;
                // Re-insert this displaced entry
                let target = Self::hash_value(&entry.key) as usize & mask;
                // Find the next empty slot from the entry's natural position
                let mut k = target;
                loop {
                    if self.hash[k].is_none() {
                        self.hash[k] = Some(entry);
                        self.hash_used += 1;
                        break;
                    }
                    k = (k + 1) & mask;
                }
                j = (j + 1) & mask;
                if j == idx {
                    break;
                }
            }
        }
    }

    /// Grow the hash part to double its current capacity.
    fn hash_grow(&mut self) {
        let new_log2 = if self.hash_log2 == 0 { 2 } else { self.hash_log2 + 1 };
        let new_cap = 1usize << new_log2;
        let old_hash = std::mem::replace(&mut self.hash, vec![None; new_cap]);
        let old_used = self.hash_used;
        self.hash_log2 = new_log2;
        self.hash_used = 0;

        for entry in old_hash.into_iter().flatten() {
            let mask = new_cap - 1;
            let h = Self::hash_value(&entry.key) as usize;
            let mut i = h & mask;
            loop {
                if self.hash[i].is_none() {
                    self.hash[i] = Some(entry);
                    self.hash_used += 1;
                    break;
                }
                i = (i + 1) & mask;
            }
        }
        debug_assert_eq!(self.hash_used, old_used);
    }

    // ── Public API ─────────────────────────────────────────────────

    /// Raw table get (no metamethods).
    pub fn raw_get(&self, key: &Value) -> Value {
        let key = match Self::normalize_key(*key) {
            Some(k) => k,
            None => return Value::Nil,
        };

        // Try array part for positive integers in range
        if let Value::Integer(i) = key {
            if i >= 1 && (i as usize) <= self.array.len() {
                return self.array[i as usize - 1];
            }
        }

        // Hash part
        self.hash_get(&key)
    }

    /// Raw table set (no metamethods).
    pub fn raw_set(&mut self, key: Value, value: Value) {
        let key = match Self::normalize_key(key) {
            Some(k) => k,
            None => return, // silently ignore nil/NaN keys
        };

        // Try array part for positive integers
        if let Value::Integer(i) = key {
            let idx = i as usize;
            if idx >= 1 && idx <= self.array.len() {
                self.array[idx - 1] = value;
                return;
            }
            // Extend array if this is the next consecutive index
            if idx == self.array.len() + 1 && !value.is_nil() {
                self.array.push(value);
                // Check if hash part has keys that should migrate to array
                self.migrate_hash_to_array();
                return;
            }
        }

        // Hash part
        self.hash_set(key, value);
    }

    /// After extending the array, check if sequential integer keys in the hash
    /// part should move into the array part.
    fn migrate_hash_to_array(&mut self) {
        loop {
            let next_idx = Value::Integer(self.array.len() as i64 + 1);
            let val = self.hash_get(&next_idx);
            if val.is_nil() {
                break;
            }
            self.array.push(val);
            self.hash_remove(&next_idx);
        }
    }

    /// Get the "length" of the table (# operator).
    pub fn length(&self) -> usize {
        self.array.len()
    }

    /// Get the next key-value pair after the given key (for `next()` / `pairs()`).
    pub fn next(&self, key: &Value) -> Option<(Value, Value)> {
        if key.is_nil() {
            // Start iteration: first non-nil array element, then hash
            for (i, v) in self.array.iter().enumerate() {
                if !v.is_nil() {
                    return Some((Value::Integer(i as i64 + 1), *v));
                }
            }
            // First occupied hash entry
            for entry in &self.hash {
                if let Some(e) = entry {
                    return Some((e.key, e.val));
                }
            }
            return None;
        }

        // Check if key is in array part
        let normalized = Self::normalize_key(*key)?;
        if let Value::Integer(i) = normalized {
            let idx = i as usize;
            if idx >= 1 && idx <= self.array.len() {
                // Next array element
                for j in idx..self.array.len() {
                    if !self.array[j].is_nil() {
                        return Some((Value::Integer(j as i64 + 1), self.array[j]));
                    }
                }
                // End of array, start of hash
                for entry in &self.hash {
                    if let Some(e) = entry {
                        return Some((e.key, e.val));
                    }
                }
                return None;
            }
        }

        // Key is in hash part: find it, then return the next occupied slot
        if let Some((idx, true)) = self.hash_find(&normalized) {
            for entry in &self.hash[idx + 1..] {
                if let Some(e) = entry {
                    return Some((e.key, e.val));
                }
            }
        }

        None
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}
