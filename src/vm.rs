use std::{cell::{RefCell, RefMut}, rc::Rc};

use crate::{gc::Gc, state::State, value::Value};

/*
The instances represents a thread
*/
pub struct Instance{
    gc:Rc<Gc>,
    state:RefCell<State>,
}

impl Instance {
    /*
    
    */
    pub fn lock<'a>(&self)->RefMut<State>{
        self.state.borrow_mut()
    }
    fn eval_loop(&self) {
        loop {
            
        }
    }
}