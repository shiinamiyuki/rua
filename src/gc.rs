//! Garbage collector: object allocation and management.
//!
//! M1.1: Basic allocation, GcRef, and string interning.
//! Mark-and-sweep collection will be added in M1.6.

use std::collections::HashMap;
use std::ptr::NonNull;

use crate::closure::Closure;
use crate::string::LuaString;
use crate::table::Table;

/// Header for all GC-managed objects.
pub struct GcHeader {
    /// Marked flag for mark-and-sweep GC (M1.6).
    pub marked: bool,
}

/// The kind of data stored in a GC-managed object.
pub enum GcObjectKind {
    String(LuaString),
    Table(Table),
    Closure(Closure),
}

/// A GC-managed object: header + data.
pub struct GcObject {
    pub header: GcHeader,
    pub kind: GcObjectKind,
}

impl GcObject {
    /// Get a reference to the string data, if this is a string object.
    pub fn as_string(&self) -> Option<&LuaString> {
        match &self.kind {
            GcObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    /// Get a reference to the table data, if this is a table object.
    pub fn as_table(&self) -> Option<&Table> {
        match &self.kind {
            GcObjectKind::Table(t) => Some(t),
            _ => None,
        }
    }

    /// Get a mutable reference to the table data, if this is a table object.
    pub fn as_table_mut(&mut self) -> Option<&mut Table> {
        match &mut self.kind {
            GcObjectKind::Table(t) => Some(t),
            _ => None,
        }
    }

    /// Get a reference to the closure data, if this is a closure object.
    pub fn as_closure(&self) -> Option<&Closure> {
        match &self.kind {
            GcObjectKind::Closure(c) => Some(c),
            _ => None,
        }
    }

    /// Get a mutable reference to the closure data, if this is a closure object.
    pub fn as_closure_mut(&mut self) -> Option<&mut Closure> {
        match &mut self.kind {
            GcObjectKind::Closure(c) => Some(c),
            _ => None,
        }
    }
}

/// A reference to a GC-managed object.
///
/// This is a raw pointer to a heap-allocated `GcObject`. The `Gc` allocator
/// owns the objects and guarantees they remain alive as long as they are
/// reachable (once collection is implemented in M1.6).
#[derive(Clone, Copy, Debug)]
pub struct GcRef {
    ptr: NonNull<GcObject>,
}

impl GcRef {
    /// Access the referenced GC object.
    ///
    /// # Safety invariant
    /// The GcRef must point to a live object (not yet freed by GC).
    /// This is guaranteed as long as the owning `Gc` is alive.
    #[inline]
    pub fn as_object(&self) -> &GcObject {
        // SAFETY: GcRef is only created by Gc::alloc which Box-allocates the object.
        // The Box is stored in Gc::objects, keeping the heap allocation alive.
        unsafe { self.ptr.as_ref() }
    }

    /// Access the referenced GC object mutably.
    ///
    /// # Safety invariant
    /// The caller must ensure no other references to this object exist.
    #[inline]
    pub fn as_object_mut(&mut self) -> &mut GcObject {
        // SAFETY: Same as as_object, plus exclusive access required by caller.
        unsafe { self.ptr.as_mut() }
    }

    /// Get the raw pointer value (for display purposes like "table: 0x...").
    #[inline]
    pub fn ptr_value(&self) -> usize {
        self.ptr.as_ptr() as usize
    }
}

impl PartialEq for GcRef {
    /// Pointer equality: two GcRefs are equal iff they point to the same object.
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Eq for GcRef {}

impl std::hash::Hash for GcRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

/// The garbage collector / object allocator.
///
/// Owns all GC-managed objects. In M1.1, only allocation is implemented.
/// Mark-and-sweep collection will be added in M1.6.
pub struct Gc {
    /// All allocated objects. Each Box provides a stable heap address.
    objects: Vec<Box<GcObject>>,
    /// String interning pool: maps byte content → GcRef for short strings.
    string_pool: HashMap<Vec<u8>, GcRef>,
    /// Approximate total bytes allocated by GC objects.
    bytes_allocated: usize,
}

impl Gc {
    /// Create a new GC allocator.
    pub fn new() -> Self {
        Gc {
            objects: Vec::new(),
            string_pool: HashMap::new(),
            bytes_allocated: 0,
        }
    }

    /// Allocate a new string. Short strings (≤40 bytes) are automatically interned.
    ///
    /// If a short string with the same content already exists in the intern pool,
    /// the existing GcRef is returned (enabling pointer-equality comparison).
    pub fn new_string(&mut self, data: &[u8]) -> GcRef {
        // Check interning pool for short strings
        if data.len() <= crate::string::SHORT_STRING_MAX {
            if let Some(&existing) = self.string_pool.get(data) {
                return existing;
            }
        }

        let lua_string = LuaString::new(data);
        let is_short = lua_string.is_short();
        let gc_ref = self.alloc(GcObjectKind::String(lua_string));

        // Intern short strings
        if is_short {
            self.string_pool.insert(data.to_vec(), gc_ref);
        }

        gc_ref
    }

    /// Allocate a new table.
    pub fn new_table(&mut self, table: Table) -> GcRef {
        self.alloc(GcObjectKind::Table(table))
    }

    /// Allocate a new closure.
    pub fn new_closure(&mut self, closure: Closure) -> GcRef {
        self.alloc(GcObjectKind::Closure(closure))
    }

    /// Internal: allocate a GcObject on the heap and return a GcRef.
    fn alloc(&mut self, kind: GcObjectKind) -> GcRef {
        let size = std::mem::size_of::<GcObject>();
        self.bytes_allocated += size;

        let object = Box::new(GcObject {
            header: GcHeader { marked: false },
            kind,
        });

        // Get a stable pointer to the heap allocation before storing the Box
        let ptr = NonNull::from(object.as_ref());
        self.objects.push(object);

        GcRef { ptr }
    }

    /// Get the approximate number of bytes allocated.
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }

    /// Get the number of live objects.
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// Get the number of interned strings.
    pub fn interned_string_count(&self) -> usize {
        self.string_pool.len()
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_new() {
        let gc = Gc::new();
        assert_eq!(gc.object_count(), 0);
        assert_eq!(gc.bytes_allocated(), 0);
    }

    #[test]
    fn test_alloc_string() {
        let mut gc = Gc::new();
        let r = gc.new_string(b"hello");
        assert_eq!(gc.object_count(), 1);

        let obj = r.as_object();
        let s = obj.as_string().unwrap();
        assert_eq!(s.as_bytes(), b"hello");
    }

    #[test]
    fn test_string_interning() {
        let mut gc = Gc::new();
        let r1 = gc.new_string(b"hello");
        let r2 = gc.new_string(b"hello");
        // Same short string returns the same GcRef (interned)
        assert_eq!(r1, r2);
        assert_eq!(gc.object_count(), 1);
        assert_eq!(gc.interned_string_count(), 1);
    }

    #[test]
    fn test_no_interning_for_long_strings() {
        let mut gc = Gc::new();
        let long = vec![b'x'; 41]; // > SHORT_STRING_MAX
        let r1 = gc.new_string(&long);
        let r2 = gc.new_string(&long);
        // Long strings are not interned: different GcRefs
        assert_ne!(r1, r2);
        assert_eq!(gc.object_count(), 2);
        assert_eq!(gc.interned_string_count(), 0);
    }

    #[test]
    fn test_different_strings() {
        let mut gc = Gc::new();
        let r1 = gc.new_string(b"hello");
        let r2 = gc.new_string(b"world");
        assert_ne!(r1, r2);
        assert_eq!(gc.object_count(), 2);
        assert_eq!(gc.interned_string_count(), 2);
    }

    #[test]
    fn test_alloc_table() {
        let mut gc = Gc::new();
        let r = gc.new_table(Table::new());
        assert_eq!(gc.object_count(), 1);
        assert!(r.as_object().as_table().is_some());
    }

    #[test]
    fn test_alloc_closure() {
        let mut gc = Gc::new();
        let r = gc.new_closure(Closure::new());
        assert_eq!(gc.object_count(), 1);
        assert!(r.as_object().as_closure().is_some());
    }

    #[test]
    fn test_mixed_allocations() {
        let mut gc = Gc::new();
        let s = gc.new_string(b"test");
        let t = gc.new_table(Table::new());
        let c = gc.new_closure(Closure::new());
        assert_eq!(gc.object_count(), 3);

        assert!(s.as_object().as_string().is_some());
        assert!(t.as_object().as_table().is_some());
        assert!(c.as_object().as_closure().is_some());

        // Cross-type checks return None
        assert!(s.as_object().as_table().is_none());
        assert!(t.as_object().as_string().is_none());
    }

    #[test]
    fn test_empty_string_interning() {
        let mut gc = Gc::new();
        let r1 = gc.new_string(b"");
        let r2 = gc.new_string(b"");
        assert_eq!(r1, r2);
        assert_eq!(gc.object_count(), 1);
    }

    #[test]
    fn test_gc_ref_pointer_equality() {
        let mut gc = Gc::new();
        let r1 = gc.new_table(Table::new());
        let r2 = gc.new_table(Table::new());
        // Different table allocations have different GcRefs
        assert_ne!(r1, r2);
        // Same value copies compare equal
        let r1_copy = r1;
        assert_eq!(r1, r1_copy);
    }

    #[test]
    fn test_bytes_allocated_grows() {
        let mut gc = Gc::new();
        assert_eq!(gc.bytes_allocated(), 0);
        gc.new_string(b"hello");
        let after_one = gc.bytes_allocated();
        assert!(after_one > 0);
        gc.new_string(b"world");
        assert!(gc.bytes_allocated() > after_one);
    }
}
