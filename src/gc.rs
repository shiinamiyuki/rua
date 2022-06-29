use lazy_static::lazy_static;
use parking_lot::RwLock;
use std::alloc::Layout;
use std::ops::Deref;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
pub struct Heap {
    pid: usize, // processor id
    pub(crate) head: AtomicUsize,
    // pub(crate) lock: usize,
    pub(crate) alloc_count: AtomicUsize,
    trace_list: RwLock<Vec<PtrTraceable>>,
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Color {
    Black = 0,
    Grey = 1,
    White = 2,
}
#[repr(C)]
pub(crate) struct GcObjectHeader {
    color: AtomicU8,
    next: AtomicUsize,
}
type Deleter = fn(*const TraceableObejct);
struct TraceableObejct {
    header: GcObjectHeader,
    deleter: Deleter,
    vtable_data: *const dyn Traceable,
    offset: usize,
}
struct TraceableObjectExplicit<T: Traceable + Sized + 'static> {
    object: TraceableObejct,
    data: T,
}
#[derive(Clone, Copy)]
pub(crate) struct PtrTraceable(*const TraceableObejct);
unsafe impl Sync for PtrTraceable {}
unsafe impl Send for PtrTraceable {}
fn deleter<T: Traceable + 'static + Sized>(ptr: *const TraceableObejct) {
    // let ptr = ptr as *mut TraceableObjectExplicit<T>;
    let ptr = PtrTraceable(ptr);
    let layout = Layout::new::<TraceableObjectExplicit<T>>();
    unsafe {
        std::mem::drop(std::ptr::read(ptr.cast::<T>()));
        std::alloc::dealloc(ptr.0 as *mut u8, layout);
    }
}
impl PtrTraceable {
    fn null() -> Self {
        Self(std::ptr::null())
    }
    fn new<T: Traceable + 'static + Sized>(object: T) -> (Self, *const TraceableObjectExplicit<T>) {
        unsafe {
            let layout = Layout::new::<TraceableObjectExplicit<T>>();
            let ptr = std::alloc::alloc(layout) as *mut TraceableObjectExplicit<T>;

            let r = ptr.as_mut().unwrap();
            assert_eq!(ptr as usize, &mut r.object as *mut TraceableObejct as usize);
            std::ptr::write(&mut r.data as *mut T, object);
            std::ptr::write(
                &mut r.object,
                TraceableObejct {
                    header: GcObjectHeader {
                        color: AtomicU8::new(Color::Black as u8),
                        next: AtomicUsize::new(std::ptr::null::<()>() as usize),
                    },
                    deleter: deleter::<T>,
                    vtable_data: &r.data as &dyn Traceable as *const dyn Traceable,
                    offset: &mut r.data as *mut T as usize - ptr as usize,
                },
            );
            (
                Self(ptr as *const TraceableObejct),
                ptr as *const TraceableObjectExplicit<T>,
            )
        }
    }
    unsafe fn cast<T: Traceable + 'static + Sized>(&self) -> *const T {
        let ptr = (self.0 as *mut u8).offset((*self.0).offset as isize) as *const T;
        ptr
    }
    fn as_ref(&self) -> &dyn Traceable {
        unsafe { &*(*self.0).vtable_data }
    }
    fn dealloc(self) {
        unsafe {
            ((*self.0).deleter)(self.0);
        }
    }
}

pub unsafe trait Traceable: Sync + Send {
    fn trace(&self, ctx: &mut TraceContext);
}

mod test {

    #[test]
    fn test_size() {
        use std::mem::size_of;

        use crate::gc::PtrTraceable;
        assert!(size_of::<PtrTraceable>() == 8);
    }
}
/*
A garbage collected *raw* pointer
Eq and Hash implemented on pointer value alone
*/
pub struct Gc<T: Traceable + 'static> {
    ptr: *const TraceableObjectExplicit<T>,
}
unsafe impl<T> Sync for Gc<T> where T: Traceable + 'static {}
unsafe impl<T> Send for Gc<T> where T: Traceable + 'static {}
impl<T> Gc<T>
where
    T: Traceable + 'static,
{
    pub fn ptr_eq(&self, other: &Gc<T>) -> bool {
        self.ptr == other.ptr
    }
    pub fn as_ptr(&self) -> *const T {
        unsafe { &(&*self.ptr).data as *const T }
    }
}
impl<T: Traceable + 'static> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.as_ptr(), state)
    }
}
impl<T: Traceable + 'static> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe { &(&*self.ptr).data }
    }
}
impl<T: Traceable + 'static> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}
impl<T: Traceable + 'static> Eq for Gc<T> {}
impl<T: Traceable + 'static> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.ptr).data }
    }
}

impl<T> Copy for Gc<T> where T: Traceable + 'static {}
impl<T> Clone for Gc<T>
where
    T: Traceable + 'static,
{
    fn clone(&self) -> Self {
        *self
    }
}
// pub struct GcLockGuard {
//     gc: Rc<GcState>,
// }
// impl Drop for GcLockGuard {
//     fn drop(&mut self) {
//         self.gc.inner.borrow_mut().lock -= 1;
//     }
// }
pub struct TraceContext {
    depth: usize,
    heap: *const Heap,
}
impl TraceContext {
    pub fn trace_ptr<T: Traceable + Sized + 'static>(&mut self, ptr: Gc<T>) {
        unsafe {
            let heap = &*self.heap;
            self.depth += 1;
            let p = PtrTraceable(ptr.ptr as *const TraceableObejct);
            if heap.mark_ptr(p) {
                if self.depth <= 128 {
                    (*ptr).trace(self);
                } else {
                    heap.push_ptr(p);
                }
            }
            self.depth -= 1;
        }
    }
    pub fn trace<T: Traceable + Sized + 'static>(&mut self, object: &T) {
        unsafe {
            let heap = &*self.heap;
            self.depth += 1;
            object.trace(self);
            self.depth -= 1;
        }
    }
}
impl Heap {
    // move an object onto heap
    pub(crate) fn allocate<T: Traceable + 'static>(&self, object: T) -> Gc<T> {
        let (ptr_dyn, ptr) = PtrTraceable::new(object);
        // println!("allocated {:?}", ptr);
        unsafe {
            self.alloc_count.fetch_add(1, Ordering::Relaxed);
            let trace_object = &*ptr_dyn.0;
            // trace_object.header.next.set(gc.head.0 as usize);
            let mut old_head = self.head.load(Ordering::Acquire);
            loop {
                trace_object.header.next.store(old_head, Ordering::Relaxed);
                match self.head.compare_exchange_weak(
                    old_head,
                    ptr_dyn.0 as usize,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                ) {
                    Err(x) => {
                        old_head = x;
                    }
                    Ok(_) => {
                        break;
                    }
                }
            }
        }
        Gc { ptr }
    }
    pub(crate) fn push_ptr(&self, ptr: PtrTraceable) {
        let mut trace_list = self.trace_list.write();
        trace_list.push(ptr);
    }
    pub(crate) fn mark_ptr(&self, ptr: PtrTraceable) -> bool {
        unsafe {
            let gc_box = &*ptr.0;
            let color = &gc_box.header.color;
            if color.load(Ordering::Relaxed) == Color::Black as u8 {
                return false;
            }

            let need_scan = match color.compare_exchange(
                Color::White as u8,
                Color::Grey as u8,
                Ordering::Acquire,
                Ordering::Relaxed,
            ) {
                Ok(_) => true,
                Err(color) => {
                    if color == Color::Black as u8 {
                        return false;
                    }
                    false
                }
            };
            // grey
            // some other thread is scanning the object
            if !need_scan {
                return false;
            }
            true
        }
    }
    // pub(crate) fn trace_ptr<T: Traceable + 'static>(&self, obj: Gc<T>, defer: bool) {
    //     self.trace_raw_ptr(PtrTraceable(obj.ptr as *const TraceableObejct), defer)
    //     // self.trace_raw_ptr(PtrTraceable(obj.ptr as *const TraceableObejct));
    //     // let mutself.trace_list
    //     // .push(PtrTraceable(obj.ptr as *const TraceableObejct));
    // }

    pub fn new(pid: usize) -> Self {
        Self {
            pid,
            head: AtomicUsize::new(0),
            alloc_count: AtomicUsize::new(0),
            trace_list: RwLock::new(Vec::new()),
        }
    }
}
impl Drop for Heap {
    fn drop(&mut self) {}
}

struct GcState {
    heaps: Vec<Heap>,
}

impl GcState {
    fn new() -> Self {
        let n_cpus = num_cpus::get();
        let heaps = (0..n_cpus).map(|pid| Heap::new(pid)).collect();
        Self { heaps }
    }
}
lazy_static! {
    static ref GC: GcState = GcState::new();
}
static THREAD_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    static THREAD_ID:usize = THREAD_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn gc_new<T: Traceable + 'static>(object: T) -> Gc<T> {
    let heap_id = THREAD_ID.with(|tid| *tid % GC.heaps.len());
    let heap = &GC.heaps[heap_id];
    heap.allocate(object)
}
