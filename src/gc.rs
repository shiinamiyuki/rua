use std::cell::{Cell, RefCell};
use std::ptr::NonNull;
pub struct Gc {
    inner: RefCell<GcInner>,
}
#[repr(C)]
pub(crate) struct GcBox<T: Traceable + ?Sized + 'static> {
    next: Cell<PtrTraceable>,
    data: T,
}
pub(crate) type PtrTraceable = Option<NonNull<GcBox<dyn Traceable>>>;
pub trait Traceable {
    fn trace(&self, gc: &Gc);
}

struct GcInner {
    head: PtrTraceable,
}
impl Gc {
    // move an object onto heap
    pub fn manage<T: Traceable + 'static>(&self, object: T) -> *const T {
        let gc_box: *mut GcBox<T> = Box::into_raw(Box::new(GcBox {
            data: object,
            next: Cell::new(None),
        }));
        unsafe {
            let p = &mut (*gc_box).data as *mut T;
            let gc_box: *mut GcBox<dyn Traceable> = gc_box;
            let mut gc = self.inner.borrow_mut();
            if let Some(head) = &mut gc.head {
                (*gc_box)
                    .next
                    .set(Some(NonNull::new(head.as_ptr()).unwrap()));
            }
            gc.head = Some(NonNull::new(gc_box).unwrap());
            p
        }
    }
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(GcInner { head: None }),
        }
    }
}
