//! Lua table: hybrid array + hash map.
//!
//! Simplified implementation for M1.5. Full open-addressing hash map in M1.7.

use crate::gc::GcRef;
use crate::value::Value;

/// A Lua table: the primary data structure in Lua.
///
/// Combines an array part (for consecutive integer keys starting at 1)
/// with a hash part (linear-scan Vec for now, open-addressing in M1.7).
pub struct Table {
    /// Array part: consecutive integer keys \[1..n\]
    pub(crate) array: Vec<Value>,
    /// Hash part: key-value pairs (linear scan, replaced with open-addressing in M1.7)
    pub(crate) hash: Vec<(Value, Value)>,
    /// Metatable (if any)
    pub(crate) metatable: Option<GcRef>,
}

impl Table {
    /// Create a new empty table.
    pub fn new() -> Self {
        Table {
            array: Vec::new(),
            hash: Vec::new(),
            metatable: None,
        }
    }

    /// Create a new table with pre-allocated capacity.
    pub fn with_capacity(array_cap: usize, _hash_cap: usize) -> Self {
        Table {
            array: Vec::with_capacity(array_cap),
            hash: Vec::new(),
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

        // Hash part (linear scan)
        for (k, v) in &self.hash {
            if *k == key {
                return *v;
            }
        }
        Value::Nil
    }

    /// Raw table set (no metamethods). Returns Err for invalid keys (nil, NaN).
    pub fn raw_set(&mut self, key: Value, value: Value) {
        let key = match Self::normalize_key(key) {
            Some(k) => k,
            None => return, // silently ignore nil/NaN keys for now
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
                return;
            }
        }

        // Hash part: update existing entry
        for (k, v) in &mut self.hash {
            if *k == key {
                *v = value;
                return;
            }
        }

        // Insert new entry (unless value is nil)
        if !value.is_nil() {
            self.hash.push((key, value));
        }
    }

    /// Get the "length" of the table (# operator).
    /// For sequences (no holes in array part), this is the array length.
    pub fn length(&self) -> usize {
        self.array.len()
    }

    /// Get the next key-value pair after the given key (for `next()` / `pairs()`).
    pub fn next(&self, key: &Value) -> Option<(Value, Value)> {
        if key.is_nil() {
            // Start iteration: first array element, then hash
            if !self.array.is_empty() {
                return Some((Value::Integer(1), self.array[0]));
            }
            return self.hash.first().map(|(k, v)| (*k, *v));
        }

        // Check if key is in array part
        let normalized = Self::normalize_key(*key)?;
        if let Value::Integer(i) = normalized {
            let idx = i as usize;
            if idx >= 1 && idx <= self.array.len() {
                // Next array element
                if idx < self.array.len() {
                    return Some((Value::Integer(i + 1), self.array[idx]));
                }
                // End of array, start of hash
                return self.hash.first().map(|(k, v)| (*k, *v));
            }
        }

        // Find key in hash part and return the next entry
        for (i, (k, _)) in self.hash.iter().enumerate() {
            if *k == normalized {
                return self.hash.get(i + 1).map(|(k, v)| (*k, *v));
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
