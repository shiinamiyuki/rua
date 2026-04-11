//! Error types for the Lua interpreter.
//!
//! Full implementation with Value-based errors in M1.5.

/// A Lua runtime error.
#[derive(Debug)]
pub struct LuaError {
    pub message: String,
}

impl LuaError {
    pub fn new(message: impl Into<String>) -> Self {
        LuaError {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for LuaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for LuaError {}
