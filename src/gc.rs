//! Garbage collector: object allocation and mark-and-sweep collection.
//!
//! Objects are stored in an intrusive singly-linked list for O(1) allocation
//! and O(1) unlinking during sweep. Mark-and-sweep uses a gray worklist to
//! avoid recursion.

use std::collections::HashMap;
use std::ptr::NonNull;

use crate::closure::{Closure, LuaClosure, Upvalue};
use crate::string::LuaString;
use crate::table::Table;
use crate::value::Value;

// ── GC tuning constants ────────────────────────────────────────────

/// Initial GC threshold (bytes). First collection triggers after this much allocation.
const GC_INITIAL_THRESHOLD: usize = 32 * 1024; // 32 KB

/// After each collection, next threshold = live_bytes * GC_GROW_FACTOR.
const GC_GROW_FACTOR: usize = 2;

// ── GC object header and types ─────────────────────────────────────

/// Header for all GC-managed objects (intrusive linked list + mark bit).
pub struct GcHeader {
    /// Next object in the all-objects linked list.
    next: Option<NonNull<GcObject>>,
    /// Marked flag for mark-and-sweep.
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
    pub fn as_string(&self) -> Option<&LuaString> {
        match &self.kind {
            GcObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<&Table> {
        match &self.kind {
            GcObjectKind::Table(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_table_mut(&mut self) -> Option<&mut Table> {
        match &mut self.kind {
            GcObjectKind::Table(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&Closure> {
        match &self.kind {
            GcObjectKind::Closure(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_closure_mut(&mut self) -> Option<&mut Closure> {
        match &mut self.kind {
            GcObjectKind::Closure(c) => Some(c),
            _ => None,
        }
    }
}

// ── GcRef ──────────────────────────────────────────────────────────

/// A reference to a GC-managed object (raw pointer, Copy).
#[derive(Clone, Copy, Debug)]
pub struct GcRef {
    ptr: NonNull<GcObject>,
}

impl GcRef {
    /// Access the referenced GC object.
    ///
    /// # Safety invariant
    /// The GcRef must point to a live object (not yet freed by GC).
    #[inline]
    pub fn as_object(&self) -> &GcObject {
        // SAFETY: GcRef is only created by Gc::alloc which heap-allocates the object.
        // The object remains alive as long as it's reachable (GC won't sweep marked objects).
        unsafe { self.ptr.as_ref() }
    }

    /// Access the referenced GC object mutably.
    ///
    /// # Safety invariant
    /// The caller must ensure no other references to this object exist.
    #[inline]
    pub fn as_object_mut(&mut self) -> &mut GcObject {
        unsafe { self.ptr.as_mut() }
    }

    /// Get the raw pointer value (for display purposes like "table: 0x...").
    #[inline]
    pub fn ptr_value(&self) -> usize {
        self.ptr.as_ptr() as usize
    }
}

impl PartialEq for GcRef {
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

// ── Gc allocator / collector ───────────────────────────────────────

/// The garbage collector: intrusive linked list of objects + mark-and-sweep.
pub struct Gc {
    /// Head of the all-objects intrusive linked list.
    all_objects: Option<NonNull<GcObject>>,
    /// Number of live objects.
    num_objects: usize,
    /// String interning pool: maps byte content → GcRef for short strings.
    string_pool: HashMap<Vec<u8>, GcRef>,
    /// Approximate total bytes allocated by GC objects.
    bytes_allocated: usize,
    /// Next collection triggers when bytes_allocated exceeds this.
    pub gc_threshold: usize,
}

impl Gc {
    /// Create a new GC allocator.
    pub fn new() -> Self {
        Gc {
            all_objects: None,
            num_objects: 0,
            string_pool: HashMap::new(),
            bytes_allocated: 0,
            gc_threshold: GC_INITIAL_THRESHOLD,
        }
    }

    /// Allocate a new string. Short strings (≤40 bytes) are automatically interned.
    pub fn new_string(&mut self, data: &[u8]) -> GcRef {
        if data.len() <= crate::string::SHORT_STRING_MAX {
            if let Some(&existing) = self.string_pool.get(data) {
                return existing;
            }
        }

        let lua_string = LuaString::new(data);
        let is_short = lua_string.is_short();
        let gc_ref = self.alloc(GcObjectKind::String(lua_string));

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

    /// Returns true if bytes_allocated has exceeded the GC threshold.
    pub fn should_collect(&self) -> bool {
        self.bytes_allocated > self.gc_threshold
    }

    /// Internal: allocate a GcObject, prepend to intrusive linked list.
    fn alloc(&mut self, kind: GcObjectKind) -> GcRef {
        let size = Self::object_size(&kind);
        self.bytes_allocated += size;
        self.num_objects += 1;

        let mut boxed = Box::new(GcObject {
            header: GcHeader {
                next: self.all_objects,
                marked: false,
            },
            kind,
        });

        let ptr = NonNull::from(boxed.as_mut());
        // Leak the Box: ownership transfers to the intrusive list.
        // We recover it in sweep or drop via Box::from_raw.
        std::mem::forget(boxed);

        self.all_objects = Some(ptr);
        GcRef { ptr }
    }

    /// Estimate the size of a GcObject for bookkeeping.
    fn object_size(kind: &GcObjectKind) -> usize {
        let base = std::mem::size_of::<GcObject>();
        match kind {
            GcObjectKind::String(s) => base + s.as_bytes().len(),
            GcObjectKind::Table(t) => {
                base + t.array.capacity() * std::mem::size_of::<Value>()
                    + t.hash.capacity() * std::mem::size_of::<Option<(Value, Value)>>()
            }
            GcObjectKind::Closure(_) => base,
        }
    }

    // ── Mark-and-sweep collection ──────────────────────────────────

    /// Run a full mark-and-sweep GC cycle.
    /// `roots` are the GcRefs directly reachable from the VM (stack, frames, etc.).
    pub fn collect(&mut self, roots: &[GcRef]) {
        // Mark phase: trace from roots using a gray worklist
        let mut gray: Vec<GcRef> = Vec::with_capacity(roots.len());
        for &root in roots {
            self.mark_object(root, &mut gray);
        }
        while let Some(obj_ref) = gray.pop() {
            self.trace_object(obj_ref, &mut gray);
        }

        // Sweep string interning pool: remove dead entries
        self.string_pool
            .retain(|_, gc_ref| unsafe { gc_ref.ptr.as_ref().header.marked });

        // Sweep phase: walk the intrusive list, free unmarked objects
        self.sweep();
    }

    /// Mark a single GcRef as reachable and push it to the gray worklist.
    fn mark_object(&self, gc_ref: GcRef, gray: &mut Vec<GcRef>) {
        let obj = unsafe { gc_ref.ptr.as_ref() };
        if !obj.header.marked {
            // SAFETY: We are the only mutator during stop-the-world collection.
            // We only set marked=true here, which is safe because no other code
            // reads/writes marked during collection.
            let obj_mut = unsafe { &mut *gc_ref.ptr.as_ptr() };
            obj_mut.header.marked = true;
            gray.push(gc_ref);
        }
    }

    /// Trace outgoing references from a marked object.
    fn trace_object(&mut self, gc_ref: GcRef, gray: &mut Vec<GcRef>) {
        let obj = unsafe { gc_ref.ptr.as_ref() };
        match &obj.kind {
            GcObjectKind::String(_) => {
                // Strings are leaf objects — no outgoing references
            }
            GcObjectKind::Table(t) => {
                // Trace array values
                for val in &t.array {
                    if let Value::Object(r) = val {
                        self.mark_object(*r, gray);
                    }
                }
                // Trace hash keys and values
                for entry in &t.hash {
                    if let Some(e) = entry {
                        if let Value::Object(r) = e.key {
                            self.mark_object(r, gray);
                        }
                        if let Value::Object(r) = e.val {
                            self.mark_object(r, gray);
                        }
                    }
                }
                // Trace metatable
                if let Some(mt) = t.metatable {
                    self.mark_object(mt, gray);
                }
            }
            GcObjectKind::Closure(c) => {
                if let Closure::Lua(LuaClosure { upvalues, .. }) = c {
                    for uv_ref in upvalues {
                        if let Upvalue::Closed(Value::Object(r)) = &*uv_ref.borrow() {
                            self.mark_object(*r, gray);
                        }
                    }
                }
            }
        }
    }

    /// Sweep: walk the intrusive list, free unmarked objects, reset marks on survivors.
    fn sweep(&mut self) {
        let mut prev: *mut Option<NonNull<GcObject>> = &mut self.all_objects;
        unsafe {
            while let Some(ptr) = *prev {
                let obj = &mut *ptr.as_ptr();
                if obj.header.marked {
                    // Survivor: clear mark for next cycle, advance
                    obj.header.marked = false;
                    prev = &mut obj.header.next;
                } else {
                    // Dead: unlink and free
                    *prev = obj.header.next;
                    let size = Self::object_size(&obj.kind);
                    self.bytes_allocated = self.bytes_allocated.saturating_sub(size);
                    self.num_objects -= 1;
                    drop(Box::from_raw(ptr.as_ptr()));
                }
            }
        }

        // Adjust threshold for next cycle
        self.gc_threshold = (self.bytes_allocated * GC_GROW_FACTOR).max(GC_INITIAL_THRESHOLD);
    }

    // ── Accessors ──────────────────────────────────────────────────

    /// Get the approximate number of bytes allocated.
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }

    /// Get the number of live objects.
    pub fn object_count(&self) -> usize {
        self.num_objects
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

impl Drop for Gc {
    fn drop(&mut self) {
        // Free all objects in the intrusive list
        let mut cur = self.all_objects;
        while let Some(ptr) = cur {
            unsafe {
                let obj = &*ptr.as_ptr();
                cur = obj.header.next;
                drop(Box::from_raw(ptr.as_ptr()));
            }
        }
        self.all_objects = None;
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
        assert_eq!(r1, r2);
        assert_eq!(gc.object_count(), 1);
        assert_eq!(gc.interned_string_count(), 1);
    }

    #[test]
    fn test_no_interning_for_long_strings() {
        let mut gc = Gc::new();
        let long = vec![b'x'; 41];
        let r1 = gc.new_string(&long);
        let r2 = gc.new_string(&long);
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
        assert_ne!(r1, r2);
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

    // ── Mark-and-sweep tests ───────────────────────────────────────

    #[test]
    fn test_collect_unreachable_freed() {
        let mut gc = Gc::new();
        let alive = gc.new_string(b"alive");
        let _dead = gc.new_string(b"this is a long dead string that won't be interned!!");
        assert_eq!(gc.object_count(), 2);

        gc.collect(&[alive]);
        assert_eq!(gc.object_count(), 1);
        // alive is still accessible
        assert_eq!(alive.as_object().as_string().unwrap().as_bytes(), b"alive");
    }

    #[test]
    fn test_collect_all_unreachable() {
        let mut gc = Gc::new();
        gc.new_table(Table::new());
        gc.new_closure(Closure::new());
        gc.new_string(b"this is a long dead string that won't be interned!!");
        assert_eq!(gc.object_count(), 3);

        gc.collect(&[]);
        // Interned strings survive (string pool is a strong root kept by retain)
        // but the long string + table + closure are freed
        assert_eq!(gc.object_count(), 0);
    }

    #[test]
    fn test_collect_traces_table_contents() {
        let mut gc = Gc::new();
        let inner_str = gc.new_string(b"inner_value_string_long_enough!!");
        let mut t = Table::new();
        t.raw_set(Value::Integer(1), Value::Object(inner_str));
        let table_ref = gc.new_table(t);

        // Also allocate a dead object
        gc.new_closure(Closure::new());
        assert_eq!(gc.object_count(), 3);

        // Only root is the table; inner_str should survive through tracing
        gc.collect(&[table_ref]);
        assert_eq!(gc.object_count(), 2); // table + inner_str
        assert_eq!(
            inner_str.as_object().as_string().unwrap().as_bytes(),
            b"inner_value_string_long_enough!!"
        );
    }

    #[test]
    fn test_collect_dead_interned_string_removed() {
        let mut gc = Gc::new();
        gc.new_string(b"temp");
        assert_eq!(gc.interned_string_count(), 1);
        assert_eq!(gc.object_count(), 1);

        // Collect with no roots: the interned string is dead
        gc.collect(&[]);
        assert_eq!(gc.object_count(), 0);
        assert_eq!(gc.interned_string_count(), 0);
    }

    #[test]
    fn test_collect_threshold_adjusts() {
        let mut gc = Gc::new();
        let s = gc.new_string(b"keep");
        gc.collect(&[s]);

        // Threshold should be at least GC_INITIAL_THRESHOLD
        assert!(gc.gc_threshold >= GC_INITIAL_THRESHOLD);
    }

    #[test]
    fn test_collect_preserves_linked_list() {
        let mut gc = Gc::new();
        let a = gc.new_string(b"alpha_long_string_for_no_intern!!");
        let _b = gc.new_string(b"bravo_long_string_for_no_intern!!");
        let c = gc.new_string(b"charlie_long_string_not_interned!!");
        let _d = gc.new_string(b"delta_long_string_for_no_intern!!");
        assert_eq!(gc.object_count(), 4);

        // Keep a and c, free b and d
        gc.collect(&[a, c]);
        assert_eq!(gc.object_count(), 2);

        // Allocate more after sweep — list should still work
        let e = gc.new_string(b"echo_long_string_for_no_intern!!!");
        assert_eq!(gc.object_count(), 3);
        assert_eq!(
            e.as_object().as_string().unwrap().as_bytes(),
            b"echo_long_string_for_no_intern!!!"
        );
    }
}
