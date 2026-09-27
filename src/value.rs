//! Lua value representation.

use std::fmt;

use crate::gc::{GcObjectKind, GcRef};

/// A Lua value.
///
/// All Lua values are represented by this enum. GC-managed objects (strings,
/// tables, closures) are referenced through [`GcRef`].
#[derive(Clone, Copy, Debug)]
pub enum Value {
    /// The nil value.
    Nil,
    /// A boolean value.
    Boolean(bool),
    /// A 64-bit integer.
    Integer(i64),
    /// A 64-bit floating-point number.
    Float(f64),
    /// A reference to a GC-managed object (string, table, closure, etc.).
    Object(GcRef),
}

impl Value {
    /// Returns the Lua type name of this value.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Boolean(_) => "boolean",
            Value::Integer(_) | Value::Float(_) => "number",
            Value::Object(gc_ref) => match &gc_ref.as_object().kind {
                GcObjectKind::String(_) => "string",
                GcObjectKind::Table(_) => "table",
                GcObjectKind::Closure(_) => "function",
                GcObjectKind::Thread(_) => "thread",
                GcObjectKind::Userdata(_) => "userdata",
            },
        }
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    #[inline]
    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer(_))
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float(_))
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_))
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self, Value::Object(r) if r.as_object().as_string().is_some())
    }

    #[inline]
    pub fn is_table(&self) -> bool {
        matches!(self, Value::Object(r) if r.as_object().as_table().is_some())
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        matches!(self, Value::Object(r) if r.as_object().as_closure().is_some())
    }

    #[inline]
    pub fn is_thread(&self) -> bool {
        matches!(self, Value::Object(r) if r.as_object().as_coroutine().is_some())
    }

    /// Lua "truthiness": everything is true except `nil` and `false`.
    #[inline]
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    #[inline]
    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    #[inline]
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Value::Integer(n) => Some(*n),
            _ => None,
        }
    }

    #[inline]
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(n) => Some(*n),
            _ => None,
        }
    }

    /// Get the value as a number (f64), converting integers to floats.
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Integer(n) => Some(*n as f64),
            Value::Float(n) => Some(*n),
            _ => None,
        }
    }

    #[inline]
    pub fn as_gc_ref(&self) -> Option<GcRef> {
        match self {
            Value::Object(r) => Some(*r),
            _ => None,
        }
    }

    /// Get the string bytes, if this is a string value.
    pub fn as_str_bytes(&self) -> Option<&[u8]> {
        match self {
            Value::Object(r) => r.as_object().as_string().map(|s| s.as_bytes()),
            _ => None,
        }
    }
}

/// Exact integer/float equality: true when `f` is finite, integral, and
/// exactly equal to `i` (floats out of the `i64` range never are).
#[inline]
pub fn int_equals_float(i: i64, f: f64) -> bool {
    f.is_finite()
        && f == f.trunc()
        && f >= -(2f64.powi(63))
        && f < 2f64.powi(63)
        && (f as i64) == i
}

impl PartialEq for Value {
    /// Structural equality for Value.
    ///
    /// Note: This is Rust-level equality, not full Lua equality
    /// (which involves metamethods). Full Lua semantics are in the VM.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            // Number equality is exact (Lua 5.5): a float equals an
            // integer only when it has that exact integer value.
            (Value::Integer(i), Value::Float(f)) | (Value::Float(f), Value::Integer(i)) => {
                int_equals_float(*i, *f)
            }
            (Value::Object(a), Value::Object(b)) => {
                // Pointer equality (fast path for interned strings)
                if a == b {
                    return true;
                }
                // Content comparison for strings
                match (&a.as_object().kind, &b.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => sa == sb,
                    // Tables and closures are equal only by identity
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

/// Format a float the way reference Lua does: try `%.15g`, and if reading
/// the result back loses precision, redo it with `%.17g`; finally, add
/// ".0" to values that look like integers.
pub fn lua_float_to_string(n: f64) -> String {
    if n.is_nan() {
        return if n.is_sign_negative() { "-nan" } else { "nan" }.to_string();
    }
    if n.is_infinite() {
        return if n < 0.0 { "-inf" } else { "inf" }.to_string();
    }
    let mut s = format_g(n, 15);
    if s.parse::<f64>() != Ok(n) {
        s = format_g(n, 17);
    }
    if s.bytes().all(|b| b == b'-' || b.is_ascii_digit()) {
        s.push_str(".0");
    }
    s
}

/// A minimal emulation of C's `printf("%.*g", prec, n)`.
fn format_g(n: f64, prec: usize) -> String {
    let p = if prec == 0 { 1 } else { prec };
    let e_str = format!("{:.*e}", p - 1, n);
    let (mant, exp_str) = e_str.split_once('e').unwrap();
    let exp: i32 = exp_str.parse().unwrap();
    if exp >= -4 && (exp as i64) < p as i64 {
        let dec = (p as i64 - 1 - exp as i64).max(0) as usize;
        let mut s = format!("{n:.dec$}");
        strip_g_zeros(&mut s);
        s
    } else {
        let mut m = mant.to_string();
        strip_g_zeros(&mut m);
        let sign = if exp < 0 { '-' } else { '+' };
        format!("{m}e{sign}{:02}", exp.abs())
    }
}

fn strip_g_zeros(s: &mut String) {
    if s.contains('.') {
        while s.ends_with('0') {
            s.pop();
        }
        if s.ends_with('.') {
            s.pop();
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{}", lua_float_to_string(*n)),
            Value::Object(gc_ref) => {
                let obj = gc_ref.as_object();
                match &obj.kind {
                    GcObjectKind::String(s) => write!(f, "{s}"),
                    GcObjectKind::Table(_) => {
                        write!(f, "table: 0x{:x}", gc_ref.ptr_value())
                    }
                    GcObjectKind::Closure(_) => {
                        write!(f, "function: 0x{:x}", gc_ref.ptr_value())
                    }
                    GcObjectKind::Thread(_) => {
                        write!(f, "thread: 0x{:x}", gc_ref.ptr_value())
                    }
                    GcObjectKind::Userdata(_) => {
                        write!(f, "userdata: 0x{:x}", gc_ref.ptr_value())
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::Gc;

    #[test]
    fn test_nil() {
        let v = Value::Nil;
        assert!(v.is_nil());
        assert!(!v.is_truthy());
        assert_eq!(v.type_name(), "nil");
        assert_eq!(format!("{v}"), "nil");
    }

    #[test]
    fn test_boolean() {
        let t = Value::Boolean(true);
        let f = Value::Boolean(false);
        assert!(t.is_boolean());
        assert!(t.is_truthy());
        assert!(!f.is_truthy());
        assert_eq!(t.as_boolean(), Some(true));
        assert_eq!(f.as_boolean(), Some(false));
        assert_eq!(t.type_name(), "boolean");
        assert_eq!(format!("{t}"), "true");
        assert_eq!(format!("{f}"), "false");
    }

    #[test]
    fn test_integer() {
        let v = Value::Integer(42);
        assert!(v.is_integer());
        assert!(v.is_number());
        assert!(v.is_truthy());
        assert_eq!(v.as_integer(), Some(42));
        assert_eq!(v.as_number(), Some(42.0));
        assert_eq!(v.type_name(), "number");
        assert_eq!(format!("{v}"), "42");
    }

    #[test]
    fn test_float() {
        let v = Value::Float(3.14);
        assert!(v.is_float());
        assert!(v.is_number());
        assert!(v.is_truthy());
        assert_eq!(v.as_float(), Some(3.14));
        assert_eq!(v.type_name(), "number");
    }

    #[test]
    fn test_float_display_whole() {
        // Whole-number floats should show ".0"
        let v = Value::Float(42.0);
        let s = format!("{v}");
        assert!(s.contains('.'), "expected decimal point in '{s}'");
    }

    #[test]
    fn test_string() {
        let mut gc = Gc::new();
        let r = gc.new_string(b"hello");
        let v = Value::Object(r);
        assert!(v.is_string());
        assert!(!v.is_table());
        assert!(!v.is_function());
        assert!(v.is_truthy());
        assert_eq!(v.type_name(), "string");
        assert_eq!(v.as_str_bytes(), Some(b"hello".as_slice()));
        assert_eq!(format!("{v}"), "hello");
    }

    #[test]
    fn test_table_value() {
        let mut gc = Gc::new();
        let r = gc.new_table(crate::table::Table::new());
        let v = Value::Object(r);
        assert!(v.is_table());
        assert_eq!(v.type_name(), "table");
        let s = format!("{v}");
        assert!(s.starts_with("table: 0x"));
    }

    #[test]
    fn test_closure_value() {
        let mut gc = Gc::new();
        let r = gc.new_closure(crate::closure::Closure::new());
        let v = Value::Object(r);
        assert!(v.is_function());
        assert_eq!(v.type_name(), "function");
        let s = format!("{v}");
        assert!(s.starts_with("function: 0x"));
    }

    #[test]
    fn test_equality_nil() {
        assert_eq!(Value::Nil, Value::Nil);
    }

    #[test]
    fn test_equality_boolean() {
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_ne!(Value::Boolean(true), Value::Boolean(false));
    }

    #[test]
    fn test_equality_integer() {
        assert_eq!(Value::Integer(42), Value::Integer(42));
        assert_ne!(Value::Integer(42), Value::Integer(43));
    }

    #[test]
    fn test_equality_float() {
        assert_eq!(Value::Float(1.5), Value::Float(1.5));
        assert_ne!(Value::Float(1.5), Value::Float(2.5));
    }

    #[test]
    fn test_equality_integer_float() {
        // Lua: 1 == 1.0 → true
        assert_eq!(Value::Integer(42), Value::Float(42.0));
        assert_eq!(Value::Float(42.0), Value::Integer(42));
        assert_ne!(Value::Integer(42), Value::Float(42.5));
    }

    #[test]
    fn test_equality_different_types() {
        assert_ne!(Value::Nil, Value::Boolean(false));
        assert_ne!(Value::Integer(0), Value::Boolean(false));
        assert_ne!(Value::Integer(0), Value::Nil);
    }

    #[test]
    fn test_equality_interned_strings() {
        let mut gc = Gc::new();
        let r1 = gc.new_string(b"hello");
        let r2 = gc.new_string(b"hello");
        assert_eq!(Value::Object(r1), Value::Object(r2));

        let r3 = gc.new_string(b"world");
        assert_ne!(Value::Object(r1), Value::Object(r3));
    }

    #[test]
    fn test_equality_non_interned_strings() {
        let mut gc = Gc::new();
        let long = vec![b'a'; 50];
        let r1 = gc.new_string(&long);
        let r2 = gc.new_string(&long);
        // Different GcRefs, but content-equals for strings
        assert_ne!(r1, r2);
        assert_eq!(Value::Object(r1), Value::Object(r2));
    }

    #[test]
    fn test_equality_tables_by_identity() {
        let mut gc = Gc::new();
        let r1 = gc.new_table(crate::table::Table::new());
        let r2 = gc.new_table(crate::table::Table::new());
        // Tables are equal only by identity
        assert_ne!(Value::Object(r1), Value::Object(r2));
        assert_eq!(Value::Object(r1), Value::Object(r1));
    }

    #[test]
    fn test_truthiness() {
        assert!(!Value::Nil.is_truthy());
        assert!(!Value::Boolean(false).is_truthy());
        assert!(Value::Boolean(true).is_truthy());
        assert!(Value::Integer(0).is_truthy()); // 0 is truthy in Lua!
        assert!(Value::Float(0.0).is_truthy()); // 0.0 is truthy in Lua!

        let mut gc = Gc::new();
        let r = gc.new_string(b"");
        assert!(Value::Object(r).is_truthy()); // "" is truthy in Lua!
    }

    #[test]
    fn test_special_floats() {
        let nan = Value::Float(f64::NAN);
        assert!(nan.is_float());
        // NaN != NaN in both Rust and Lua
        assert_ne!(nan, nan);

        let inf = Value::Float(f64::INFINITY);
        assert!(inf.is_float());
        assert_eq!(inf, Value::Float(f64::INFINITY));

        let neg_inf = Value::Float(f64::NEG_INFINITY);
        assert_ne!(inf, neg_inf);
    }

    #[test]
    fn test_negative_zero() {
        // -0.0 == 0.0 in IEEE 754 (and Lua)
        assert_eq!(Value::Float(-0.0), Value::Float(0.0));
        assert_eq!(Value::Float(-0.0), Value::Integer(0));
    }
}
