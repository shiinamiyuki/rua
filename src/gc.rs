use std::cell::{Cell, RefCell};
use std::ops::Deref;
use std::ptr::NonNull;
use std::rc::Rc;
pub struct GcState {
    pub(crate) inner: RefCell<GcInner>,
}
// #[derive(Clone, Copy, PartialEq, Eq)]
// enum Color {
//     White,
//     Black,
//     Grey,
// }
// #[derive(Clone, Copy, PartialEq, Eq)]
// enum Mark
#[repr(C)]
pub(crate) struct GcBox<T: Traceable + ?Sized + 'static> {
    marked: Cell<bool>,
    next: Cell<PtrTraceable>,
    prev: Cell<PtrTraceable>,
    strong_ref_count: Cell<usize>,
    data: T,
}
impl<T: Traceable + ?Sized + 'static> GcBox<T> {
    unsafe fn remove(&self) {
        let prev = self.prev.get();
        let next = self.next.get();
        if let Some(prev) = prev {
            prev.as_ref().next.set(next);
        }
        if let Some(next) = next {
            next.as_ref().prev.set(prev);
        }
    }
}
pub(crate) type PtrTraceable = Option<NonNull<GcBox<dyn Traceable>>>;
pub trait Traceable {
    fn trace(&self, gc: &GcState);
}

/*
A garbage collected *raw* pointer
Eq and Hash implemented on pointer value alone

*/
pub(crate) struct Gc<T: Traceable + 'static + ?Sized> {
    ptr: NonNull<GcBox<T>>,
}
impl<T> Gc<T>
where
    T: Traceable + 'static + ?Sized,
{
    pub fn ptr_eq(&self, other: &Gc<T>) -> bool {
        self.ptr == other.ptr
    }
    pub fn as_ptr(&self) -> *const T {
        unsafe { &self.ptr.as_ref().data as *const T }
    }
}
impl<T: Traceable + 'static + ?Sized> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.as_ptr(), state)
    }
}
impl<T: Traceable + 'static + ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe { &self.ptr.as_ref().data }
    }
}
impl<T: Traceable + 'static + ?Sized> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}
impl<T: Traceable + 'static + ?Sized> Eq for Gc<T> {}
impl<T: Traceable + 'static + ?Sized> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &self.ptr.as_ref().data }
    }
}
// impl<T> Copy for Gc<T> where T: Traceable + 'static + ?Sized {}
impl<T> Clone for Gc<T>
where
    T: Traceable + 'static + ?Sized,
{
    fn clone(&self) -> Self {
        unsafe {
            self.ptr
                .as_ref()
                .strong_ref_count
                .set(self.ptr.as_ref().strong_ref_count.get() + 1);
            Self { ptr: self.ptr }
        }
    }
}
impl<T> Drop for Gc<T>
where
    T: Traceable + 'static + ?Sized,
{
    fn drop(&mut self) {
        unsafe {
            let cnt = self.ptr.as_ref().strong_ref_count.get();
            if cnt == 1 {
                self.ptr.as_ref().remove();
                Box::from_raw(self.ptr.as_ptr());
            } else if cnt > 1 {
                self.ptr.as_ref().strong_ref_count.set(cnt - 1);
            } else {
                panic!("cnt=0");
            }
        }
    }
}
pub(crate) struct GcInner {
    head: PtrTraceable,
    lock: usize,
    pub(crate) alloc_count: usize,
}
pub struct GcLockGuard {
    gc: Rc<GcState>,
}
impl Drop for GcLockGuard {
    fn drop(&mut self) {
        self.gc.inner.borrow_mut().lock -= 1;
    }
}
impl GcState {
    // move an object onto heap
    pub(crate) fn allocate<T: Traceable + 'static>(&self, object: T) -> Gc<T> {
        let gc_box: *mut GcBox<T> = Box::into_raw(Box::new(GcBox {
            // color: Cell::new(Color::White),
            marked: Cell::new(false),
            data: object,
            next: Cell::new(None),
            prev: Cell::new(None),
            strong_ref_count: Cell::new(1),
        }));
        // println!("allocate object {:0x}",gc_box as u64);
        unsafe {
            let gc_box_dyn: *mut GcBox<dyn Traceable> = gc_box;
            let mut gc = self.inner.borrow_mut();
            gc.alloc_count += 1;
            if let Some(head) = &mut gc.head {
                (*gc_box_dyn)
                    .next
                    .set(Some(NonNull::new(head.as_ptr()).unwrap()));
            }
            if let Some(head) = gc.head {
                head.as_ref()
                    .prev
                    .set(Some(NonNull::new(gc_box_dyn).unwrap()));
            }
            gc.head = Some(NonNull::new(gc_box_dyn).unwrap());
        }
        Gc {
            ptr: NonNull::new(gc_box).unwrap(),
        }
    }
    pub(crate) fn trace_ptr<T: Traceable + ?Sized + 'static>(&self, obj: &Gc<T>) {
        unsafe {
            let ptr = obj.ptr;
            let gc_box = ptr.as_ref();
            // println!("tracing object {:0x} {}", obj.as_ptr().cast::<()>() as u64, gc_box.marked.get());
            if !gc_box.marked.get() {
                gc_box.marked.set(true);
                return obj.trace(self);
            }
        }
    }
    pub fn trace<T: Traceable + ?Sized + 'static>(&self, obj: &T) {
        obj.trace(self);
    }
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(GcInner {
                head: None,
                lock: 0,
                alloc_count: 0,
            }),
        }
    }
    pub fn lock(self: &Rc<GcState>) -> GcLockGuard {
        self.inner.borrow_mut().lock += 1;
        GcLockGuard { gc: self.clone() }
    }

    pub(crate) fn start_trace(&self) {}
    pub(crate) fn end_trace(&self) {}
    pub(crate) fn collect(&self, force: bool) {
        let mut gc = self.inner.borrow_mut();
        assert!(!force || gc.lock == 0);
        let mut cur = gc.head;
        // println!("collecting");
        unsafe {
            while let Some(p) = cur {
                let object = p.as_ref();
                let next = object.next.get();
                if !object.marked.get() {
                    if cur == gc.head {
                        gc.head = next;
                    }
                    object.remove();
                    // println!("collected object {:0x}", p.as_ptr().cast::<()>() as u64);
                    Box::from_raw(p.as_ptr());
                } else {
                    object.marked.set(false);
                }
                cur = next;
            }
        }
    }
}
impl Drop for GcState {
    fn drop(&mut self) {
        self.collect(true);
    }
}
