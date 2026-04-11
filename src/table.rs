//! Lua table: hybrid array + hash map.
//!
//! Full implementation in M1.7. This is a minimal stub for M1.1.

use crate::gc::GcRef;
use crate::value::Value;

/// A Lua table: the primary data structure in Lua.
///
/// Combines an array part (for consecutive integer keys starting at 1)
/// with a hash part (for all other keys).
pub struct Table {
    /// Array part: consecutive integer keys \[1..n\]
    pub(crate) array: Vec<Value>,
    // Hash part: open-addressing hash map (M1.7)
    /// Metatable (if any)
    pub(crate) metatable: Option<GcRef>,
}

impl Table {
    /// Create a new empty table.
    pub fn new() -> Self {
        Table {
            array: Vec::new(),
            metatable: None,
        }
    }

    /// Create a new table with pre-allocated array capacity.
    pub fn with_capacity(array_cap: usize, _hash_cap: usize) -> Self {
        Table {
            array: Vec::with_capacity(array_cap),
            metatable: None,
        }
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}
