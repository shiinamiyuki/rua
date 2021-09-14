use crate::{runtime::{Module, RuntimeError}, state::CallContext};

pub trait Bind {
    fn bind(module:&mut Module);
}


// pub struct StructBinder<'a> {
//     module:&'a mut Module,
//     name:String,
// }

// impl<'a> StructBinder<'a> {
//     fn init<F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(&mut self, f:F ) {
//         self.module.function(self.name.clone(), ||)
//     }
//     fn method<F: Fn(&CallContext<'_>) -> Result<(), RuntimeError> + 'static>(&mut self,name:& str, f:F ) {


//     }
//     fn field<T>(&mut self, name:& str){

//     }
//     fn finish(self){
        
//     }
// }