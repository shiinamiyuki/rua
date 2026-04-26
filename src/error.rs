//! Error types for the Lua interpreter.

use crate::value::Value;

/// A Lua runtime error.
///
/// Carries an optional Lua `Value` as the error object (Lua errors can be any
/// value, not just strings). When `value` is `None`, `message` is used.
#[derive(Debug)]
pub struct LuaError {
    pub message: String,
    /// The actual Lua error object (may be nil, string, table, etc.).
    pub value: Option<Value>,
}

impl LuaError {
    pub fn new(message: impl Into<String>) -> Self {
        LuaError {
            message: message.into(),
            value: None,
        }
    }

    /// Create a LuaError with a specific Lua value as the error object.
    pub fn with_value(value: Value) -> Self {
        let message = format!("{value}");
        LuaError {
            message,
            value: Some(value),
        }
    }

    /// Get the error as a Lua Value—either the stored value or a string from message.
    pub fn to_value(&self, gc: &mut crate::gc::Gc) -> Value {
        if let Some(v) = self.value {
            v
        } else {
            Value::Object(gc.new_string(self.message.as_bytes()))
        }
    }
}

impl std::fmt::Display for LuaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for LuaError {}
