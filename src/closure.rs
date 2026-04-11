//! Lua closure (function + upvalues).
//!
//! Full implementation in M1.4/M1.5. This is a minimal stub for M1.1.

/// A Lua closure: a function prototype combined with captured upvalues.
pub struct Closure {
    // proto: GcRef<Proto>  -- added in M1.4
    // upvalues: Vec<...>   -- added in M1.4/M1.5
}

impl Closure {
    /// Create a new closure (placeholder for M1.1).
    pub fn new() -> Self {
        Closure {}
    }
}

impl Default for Closure {
    fn default() -> Self {
        Self::new()
    }
}
