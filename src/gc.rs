pub struct Gc {
    head: *const dyn Traceable,
}
pub trait Traceable {
    fn set_next(&self, next: *const dyn Traceable);
    fn next(&self) -> *const dyn Traceable;
    fn trace(&self, gc: &Gc);
}