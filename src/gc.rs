use std::cell::{Cell, RefCell};
use std::ops::Deref;
use std::ptr::NonNull;
pub struct GcState {
    inner: RefCell<GcInner>,
}
#[repr(C)]
pub(crate) struct GcBox<T: Traceable + ?Sized + 'static> {
    next: Cell<PtrTraceable>,
    data: T,
}
pub(crate) type PtrTraceable = Option<NonNull<GcBox<dyn Traceable>>>;
pub trait Traceable {
    fn trace(&self, gc: &GcState);
}

/*
A garbage collected *raw* pointer
Eq and Hash implemented on pointer value alone

*/
pub struct Gc<T: Traceable + 'static + ?Sized> {
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
impl<T> Copy for Gc<T> where T: Traceable + 'static + ?Sized {}
impl<T> Clone for Gc<T>
where
    T: Traceable + 'static + ?Sized,
{
    fn clone(&self) -> Self {
        *self
    }
}
struct GcInner {
    head: PtrTraceable,
}
impl GcState {
    // move an object onto heap
    pub fn allocate<T: Traceable + 'static>(&self, object: T) -> Gc<T> {
        let gc_box: *mut GcBox<T> = Box::into_raw(Box::new(GcBox {
            data: object,
            next: Cell::new(None),
        }));
        unsafe {
            let p = &mut (*gc_box).data as *mut T;
            let gc_box_dyn: *mut GcBox<dyn Traceable> = gc_box;
            let mut gc = self.inner.borrow_mut();
            if let Some(head) = &mut gc.head {
                (*gc_box_dyn)
                    .next
                    .set(Some(NonNull::new(head.as_ptr()).unwrap()));
            }
            gc.head = Some(NonNull::new(gc_box_dyn).unwrap());
        }
        Gc {
            ptr: NonNull::new(gc_box).unwrap(),
        }
    }
    pub fn trace_ptr<T: Traceable + ?Sized + 'static>(&self, obj: Gc<T>) {
        todo!()
    }
    pub fn trace<T: Traceable + ?Sized + 'static>(&self, obj: &T) {
        todo!()
    }
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(GcInner { head: None }),
        }
    }
}
impl Drop for GcState {
    fn drop(&mut self) {}
}
