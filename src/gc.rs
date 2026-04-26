//! Garbage collector: object allocation and mark-and-sweep collection.
//!
//! Objects are stored in an intrusive singly-linked list for O(1) allocation
//! and O(1) unlinking during sweep. Mark-and-sweep uses a gray worklist to
//! avoid recursion.

use std::collections::HashMap;
use std::ptr::NonNull;

use std::any::Any;

use crate::closure::{Closure, LuaClosure, Upvalue};
use crate::coroutine::Coroutine;
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
    /// True if this object has a `__gc` finalizer pending the next GC cycle.
    /// Cleared after the finalizer runs (or when the object is removed from
    /// the finalizable list).
    pub has_finalizer: bool,
}

/// The kind of data stored in a GC-managed object.
pub enum GcObjectKind {
    String(LuaString),
    Table(Table),
    Closure(Closure),
    Thread(Coroutine),
    Userdata(LuaUserdata),
}

/// A GC-managed userdata: arbitrary Rust data + optional metatable.
pub struct LuaUserdata {
    pub data: Box<dyn Any>,
    pub metatable: Option<GcRef>,
    /// User-associated values (for debug.getuservalue / debug.setuservalue).
    pub user_values: Vec<Value>,
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

    pub fn as_coroutine(&self) -> Option<&Coroutine> {
        match &self.kind {
            GcObjectKind::Thread(co) => Some(co),
            _ => None,
        }
    }

    pub fn as_coroutine_mut(&mut self) -> Option<&mut Coroutine> {
        match &mut self.kind {
            GcObjectKind::Thread(co) => Some(co),
            _ => None,
        }
    }

    pub fn as_userdata(&self) -> Option<&LuaUserdata> {
        match &self.kind {
            GcObjectKind::Userdata(ud) => Some(ud),
            _ => None,
        }
    }

    pub fn as_userdata_mut(&mut self) -> Option<&mut LuaUserdata> {
        match &mut self.kind {
            GcObjectKind::Userdata(ud) => Some(ud),
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
    pub fn as_object_mut(&self) -> &mut GcObject {
        unsafe { &mut *self.ptr.as_ptr() }
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
    /// Shared metatable for file handles (userdata).
    pub file_metatable: Option<GcRef>,
    /// Tables and userdata that have a `__gc` finalizer registered. Each
    /// object appears at most once. Removed when its finalizer runs (so it
    /// runs at most once) or when the object is swept.
    finalizable: Vec<GcRef>,
    /// Objects that became unreachable but had a finalizer; their `__gc` is
    /// pending. The VM drains this between GC cycles.
    pending_finalizers: Vec<GcRef>,
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
            file_metatable: None,
            finalizable: Vec::new(),
            pending_finalizers: Vec::new(),
        }
    }

    /// Mark an object as having a `__gc` finalizer pending. Idempotent.
    /// Only meaningful for tables and userdata. Tables/userdata get their
    /// finalizer called once, on the GC cycle in which they become
    /// unreachable (with a one-cycle resurrection).
    pub fn register_finalizer(&mut self, obj: GcRef) {
        let header = unsafe { &mut (*obj.ptr.as_ptr()).header };
        if header.has_finalizer {
            return;
        }
        header.has_finalizer = true;
        self.finalizable.push(obj);
    }

    /// Drain the queue of objects whose `__gc` is pending.
    pub fn take_pending_finalizers(&mut self) -> Vec<GcRef> {
        std::mem::take(&mut self.pending_finalizers)
    }

    /// Whether there are pending finalizers waiting to be run.
    pub fn has_pending_finalizers(&self) -> bool {
        !self.pending_finalizers.is_empty()
    }

    /// Push an object back onto the pending-finalizer queue. Used by the VM
    /// while it iterates the previously-drained queue, to keep not-yet-run
    /// finalizers rooted across re-entrant collections.
    pub fn requeue_pending_finalizer(&mut self, obj: GcRef) {
        self.pending_finalizers.push(obj);
    }

    /// Pop one pending finalizer from the queue (LIFO: most-recently
    /// resurrected finalizes first). Returns None if the queue is empty.
    pub fn pop_pending_finalizer(&mut self) -> Option<GcRef> {
        self.pending_finalizers.pop()
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

    /// Find an existing interned string by its bytes (no allocation).
    pub fn find_string(&self, data: &[u8]) -> Option<GcRef> {
        self.string_pool.get(data).copied()
    }

    /// Allocate a new closure.
    pub fn new_closure(&mut self, closure: Closure) -> GcRef {
        self.alloc(GcObjectKind::Closure(closure))
    }

    /// Allocate a new coroutine (thread).
    pub fn new_thread(&mut self, coroutine: Coroutine) -> GcRef {
        self.alloc(GcObjectKind::Thread(coroutine))
    }

    /// Allocate a new userdata.
    pub fn new_userdata(&mut self, data: Box<dyn Any>, metatable: Option<GcRef>) -> GcRef {
        self.alloc(GcObjectKind::Userdata(LuaUserdata { data, metatable, user_values: Vec::new() }))
    }

    /// Returns true if bytes_allocated has exceeded the GC threshold.
    pub fn should_collect(&self) -> bool {
        self.bytes_allocated > self.gc_threshold
    }

    /// Approximate number of live bytes under GC management.
    pub fn bytes_allocated_approx(&self) -> usize {
        self.bytes_allocated
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
                has_finalizer: false,
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
            GcObjectKind::Thread(co) => {
                base + co.stack.capacity() * std::mem::size_of::<Value>()
            }
            GcObjectKind::Userdata(_) => base + 64, // rough estimate
        }
    }

    // ── Mark-and-sweep collection ──────────────────────────────────

    /// Run a full mark-and-sweep GC cycle.
    /// `roots` are the GcRefs directly reachable from the VM (stack, frames, etc.).
    pub fn collect(&mut self, roots: &[GcRef]) {
        // 1. Refresh weak-mode flags from each table's `__mode` metafield. A
        //    table's `__mode` may have been mutated since its metatable was
        //    set, so we re-read it here for correctness.
        self.refresh_weak_modes();

        // 2. Mark phase. Tables encountered here are traced respecting their
        //    weak flags (weak refs are NOT followed, except for ephemeron
        //    propagation handled below). Each weak table is recorded in
        //    `weak_tables` so we can clear dead entries afterwards.
        let mut gray: Vec<GcRef> = Vec::with_capacity(roots.len());
        let mut weak_tables: Vec<GcRef> = Vec::new();

        // Add pending finalizers as roots so the previously-resurrected
        // objects survive at least until their finalizer has run.
        for &r in &self.pending_finalizers {
            self.mark_object(r, &mut gray);
        }

        for &root in roots {
            self.mark_object(root, &mut gray);
        }
        while let Some(obj_ref) = gray.pop() {
            self.trace_object(obj_ref, &mut gray, &mut weak_tables);
        }

        // 3. Ephemeron fixed-point. For tables with weak keys, an entry's
        //    value is reachable only if its key is reachable. Repeat until
        //    no new marks happen.
        let mut changed = true;
        while changed {
            changed = false;
            for &t_ref in &weak_tables {
                let t = match t_ref.as_object().as_table() {
                    Some(t) => t,
                    None => continue,
                };
                if !t.weak_keys || t.weak_values {
                    // Weak-keys-only tables are ephemerons. (Weak-values-only
                    // and full-weak tables don't propagate marks at all.)
                    continue;
                }
                for (k, v) in t.entries() {
                    if let Value::Object(kr) = k {
                        if Self::is_marked(kr) {
                            // Key is alive; mark the value.
                            if let Value::Object(vr) = v {
                                if !Self::is_marked(vr) {
                                    self.mark_object(vr, &mut gray);
                                    changed = true;
                                }
                            }
                        }
                    } else {
                        // Non-object keys (numbers, booleans) are always
                        // "alive"; treat their values as reachable.
                        if let Value::Object(vr) = v {
                            if !Self::is_marked(vr) {
                                self.mark_object(vr, &mut gray);
                                changed = true;
                            }
                        }
                    }
                }
            }
            // Drain any new gray nodes produced by ephemeron propagation.
            while let Some(obj_ref) = gray.pop() {
                self.trace_object(obj_ref, &mut gray, &mut weak_tables);
            }
        }

        // 4. Finalizers: any registered finalizable object that is still
        //    unmarked becomes pending; resurrect it (mark + trace) so its
        //    finalizer can safely run. Each object is finalized at most once.
        let mut to_finalize: Vec<GcRef> = Vec::new();
        let mut still_finalizable: Vec<GcRef> = Vec::with_capacity(self.finalizable.len());
        let finalizable = std::mem::take(&mut self.finalizable);
        for obj in finalizable {
            if Self::is_marked(obj) {
                still_finalizable.push(obj);
            } else {
                // Resurrect: mark + trace so it stays alive for finalization.
                self.mark_object(obj, &mut gray);
                while let Some(g) = gray.pop() {
                    self.trace_object(g, &mut gray, &mut weak_tables);
                }
                // Clear its has_finalizer so subsequent collections sweep it.
                let header = unsafe { &mut (*obj.ptr.as_ptr()).header };
                header.has_finalizer = false;
                to_finalize.push(obj);
            }
        }
        self.finalizable = still_finalizable;
        // Re-run ephemeron propagation in case finalizer-resurrection
        // exposed more reachability.
        if !to_finalize.is_empty() {
            let mut changed = true;
            while changed {
                changed = false;
                for &t_ref in &weak_tables {
                    let t = match t_ref.as_object().as_table() {
                        Some(t) => t,
                        None => continue,
                    };
                    if !t.weak_keys || t.weak_values {
                        continue;
                    }
                    for (k, v) in t.entries() {
                        let key_alive = match k {
                            Value::Object(kr) => Self::is_marked(kr),
                            Value::Nil => false,
                            _ => true,
                        };
                        if key_alive {
                            if let Value::Object(vr) = v {
                                if !Self::is_marked(vr) {
                                    self.mark_object(vr, &mut gray);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
                while let Some(g) = gray.pop() {
                    self.trace_object(g, &mut gray, &mut weak_tables);
                }
            }
        }
        self.pending_finalizers.extend(to_finalize);

        // 5. Clear dead entries from weak tables.
        for t_ref in weak_tables {
            self.clear_weak_entries(t_ref);
        }

        // 6. Sweep string interning pool: remove dead entries.
        self.string_pool
            .retain(|_, gc_ref| unsafe { gc_ref.ptr.as_ref().header.marked });

        // 7. Sweep phase: walk the intrusive list, free unmarked objects.
        self.sweep();
    }

    /// Re-scan all live tables and refresh their weak_keys/weak_values flags
    /// from their metatables' `__mode` field.
    fn refresh_weak_modes(&mut self) {
        let mut cur = self.all_objects;
        unsafe {
            while let Some(ptr) = cur {
                let obj = &mut *ptr.as_ptr();
                if let GcObjectKind::Table(_) = &obj.kind {
                    let mt = obj.as_table().unwrap().metatable;
                    let mode_bytes = mt
                        .and_then(|mt_ref| {
                            let mt_table = mt_ref.as_object().as_table()?;
                            // Find the interned __mode key, if any
                            let mode_key = self.string_pool.get(b"__mode".as_slice()).copied()?;
                            let v = mt_table.raw_get(&Value::Object(mode_key));
                            if let Value::Object(r) = v {
                                let s = r.as_object().as_string()?;
                                Some(s.as_bytes().to_vec())
                            } else {
                                None
                            }
                        });
                    obj.as_table_mut()
                        .unwrap()
                        .set_weak_mode(mode_bytes.as_deref());
                }
                cur = obj.header.next;
            }
        }
    }

    /// Test if a GcRef points to a marked object.
    #[inline]
    fn is_marked(r: GcRef) -> bool {
        unsafe { r.ptr.as_ref().header.marked }
    }

    /// Remove entries from a weak table whose key or value is dead.
    fn clear_weak_entries(&self, t_ref: GcRef) {
        let weak_keys;
        let weak_values;
        let mut to_remove: Vec<Value> = Vec::new();
        {
            let t = match t_ref.as_object().as_table() {
                Some(t) => t,
                None => return,
            };
            weak_keys = t.weak_keys;
            weak_values = t.weak_values;
            for (k, v) in t.entries() {
                if v.is_nil() {
                    continue;
                }
                let dead_key = weak_keys
                    && matches!(k, Value::Object(r) if !Self::is_marked(r));
                let dead_val = weak_values
                    && matches!(v, Value::Object(r) if !Self::is_marked(r));
                if dead_key || dead_val {
                    to_remove.push(k);
                }
            }
        }
        if !to_remove.is_empty() {
            let t_mut = t_ref.as_object_mut().as_table_mut().unwrap();
            for k in to_remove {
                t_mut.raw_remove(&k);
            }
        }
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

    /// Trace outgoing references from a marked object. Tables with weak
    /// flags are recorded in `weak_tables` and have their weak refs skipped.
    fn trace_object(
        &mut self,
        gc_ref: GcRef,
        gray: &mut Vec<GcRef>,
        weak_tables: &mut Vec<GcRef>,
    ) {
        let obj = unsafe { gc_ref.ptr.as_ref() };
        match &obj.kind {
            GcObjectKind::String(_) => {
                // Strings are leaf objects — no outgoing references
            }
            GcObjectKind::Table(t) => {
                let weak_keys = t.weak_keys;
                let weak_values = t.weak_values;
                if weak_keys || weak_values {
                    weak_tables.push(gc_ref);
                }
                // Trace metatable always.
                if let Some(mt) = t.metatable {
                    self.mark_object(mt, gray);
                }

                // For weak-key tables, the value is only reachable if the
                // key is reachable (ephemeron semantics). We therefore skip
                // marking values here; the ephemeron fixed-point loop in
                // `collect` handles them.
                //
                // Array entries are always under integer keys, which are
                // value-typed and hence "alive"; so array values follow the
                // weak_values flag only.
                let trace_array_vals = !weak_values;
                let trace_hash_keys = !weak_keys;
                // Hash values: marked here only if BOTH keys and values are
                // strong. Weak-key tables defer value marking to the
                // ephemeron loop; weak-value tables obviously skip them.
                let trace_hash_vals = !weak_keys && !weak_values;

                if trace_array_vals {
                    for val in &t.array {
                        if let Value::Object(r) = val {
                            self.mark_object(*r, gray);
                        }
                    }
                }
                for entry in &t.hash {
                    if let Some(e) = entry {
                        if trace_hash_keys {
                            if let Value::Object(r) = e.key {
                                self.mark_object(r, gray);
                            }
                        }
                        if trace_hash_vals {
                            if let Value::Object(r) = e.val {
                                self.mark_object(r, gray);
                            }
                        }
                    }
                }
            }
            GcObjectKind::Closure(c) => {
                match c {
                    Closure::Lua(LuaClosure { upvalues, .. }) => {
                        for uv_ref in upvalues {
                            if let Upvalue::Closed(Value::Object(r)) = &*uv_ref.borrow() {
                                self.mark_object(*r, gray);
                            }
                        }
                    }
                    Closure::WrapIterator(co_ref) => {
                        self.mark_object(*co_ref, gray);
                    }
                    _ => {}
                }
            }
            GcObjectKind::Thread(co) => {
                // Trace stack values
                for val in &co.stack[..co.top.min(co.stack.len())] {
                    if let Value::Object(r) = val {
                        self.mark_object(*r, gray);
                    }
                }
                // Trace frame closures, varargs, and upvalues
                for frame in &co.frames {
                    self.mark_object(frame.closure, gray);
                    for val in &frame.varargs {
                        if let Value::Object(r) = val {
                            self.mark_object(*r, gray);
                        }
                    }
                    for uv in &frame.upvalues {
                        if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                            self.mark_object(*r, gray);
                        }
                    }
                }
                // Trace open upvalues
                for uv in &co.open_upvalues {
                    if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                        self.mark_object(*r, gray);
                    }
                }
                // Trace body function
                if let Some(body) = co.body {
                    self.mark_object(body, gray);
                }
                // Trace pcall guard handler values
                for guard in &co.pcall_guards {
                    if let Value::Object(r) = guard.handler {
                        self.mark_object(r, gray);
                    }
                }
            }
            GcObjectKind::Userdata(ud) => {
                if let Some(mt) = ud.metatable {
                    self.mark_object(mt, gray);
                }
                for val in &ud.user_values {
                    if let Value::Object(r) = val {
                        self.mark_object(*r, gray);
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
