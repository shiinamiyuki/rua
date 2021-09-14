use crate::runtime::Module;

pub trait Bind {
    fn bind(module:&mut Module);
}


// pub struct StructBinder<'a> {
//     module:&'a mut Module,
// }

// impl<'a> StructBinder<'a> {
//     fn method<F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(&mut self, ) {

//     }
// }