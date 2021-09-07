use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{
    bytecode::{ByteCode, ByteCodeModule, OpCode},
    gc::Gc,
    runtime::RuntimeError,
    state::{Frame, State},
    value::{Closure, Managed, Value},
};

/*
The instances represents a thread
*/
pub struct Instance {
    pub(crate) gc: Rc<Gc>,
    pub(crate) state: State,
}

impl Instance {
    /*

    */
    // pub fn lock<'a>(&self) -> RefMut<State> {
    //     self.state.borrow_mut()
    // }
    fn exec_frame(&self, state: &State) -> Result<(), RuntimeError> {
        let mut frames = state.frames.borrow_mut();
        let frame = frames.last_mut().unwrap();
        let ip = &mut frame.ip;
        let module = unsafe { &*(*frame.closure).data.module };
        let mut eval_stack = state.eval_stack.borrow_mut();
        while *ip < module.code.len() {
            macro_rules! binary_op_impl {
                ($func:ident) => {{
                    let rhs = eval_stack.pop().unwrap();
                    let lhs = eval_stack.last_mut().unwrap();
                    *lhs = state.$func(*lhs, rhs)?;
                }};
            }
            let instruction = module.code[*ip];
            let mut ip_modified = false;
            match instruction {
                crate::bytecode::ByteCode::Op(op) => match op {
                    OpCode::Nop => {}
                    OpCode::LoadNumber => {
                        ip_modified = true;
                        let lo = match module.code[*ip + 1] {
                            ByteCode::FloatLo(bytes) => bytes,
                            _ => unreachable!(),
                        };
                        let hi = match module.code[*ip + 2] {
                            ByteCode::FloatHi(bytes) => bytes,
                            _ => unreachable!(),
                        };
                        let bytes = [lo[0], lo[1], lo[2], lo[3], hi[0], hi[1], hi[2], hi[3]];
                        eval_stack.push(Value::from_number(f64::from_le_bytes(bytes)));
                        *ip += 3;
                    }
                    OpCode::LoadNil => {
                        eval_stack.push(Value::nil());
                    }
                    OpCode::LoadTrue => {
                        eval_stack.push(Value::from_bool(true));
                    }
                    OpCode::LoadFalse => {
                        eval_stack.push(Value::from_bool(false));
                    }
                    OpCode::Add => {
                        binary_op_impl!(add)
                    }
                    OpCode::Sub => {
                        binary_op_impl!(sub)
                    }
                    OpCode::Mul => {
                        binary_op_impl!(mul)
                    }
                    OpCode::Mod => {
                        binary_op_impl!(mod_)
                    }
                    OpCode::Div => {
                        binary_op_impl!(div)
                    }
                    OpCode::IDiv => {
                        binary_op_impl!(idiv)
                    }
                    _ => unreachable!(),
                },
                crate::bytecode::ByteCode::Op3U8(_, _) => todo!(),
                crate::bytecode::ByteCode::FloatHi(_) => unreachable!(),
                crate::bytecode::ByteCode::FloatLo(_) => unreachable!(),
                crate::bytecode::ByteCode::JumpAddress(_) => unreachable!(),
                crate::bytecode::ByteCode::BranchAddress(_) => unreachable!(),
            }
            if !ip_modified {
                *ip += 1;
            }
        }
        println!("{}", eval_stack.last().unwrap().print());
        Ok(())
    }
    // top level
    pub fn exec(&self, module: ByteCodeModule) -> Result<(), RuntimeError> {
        let closure = self.gc.manage(Managed::<Closure> {
            data: Closure {
                entry: 0,
                n_locals: 0,
                module: Rc::new(module),
                upvalues: RefCell::new(vec![]),
            },
        });
        {
            let mut frames = self.state.frames.borrow_mut();
            frames.push(Frame::new(0, 0, closure));
        }
        self.exec_frame(&self.state)?;

        Ok(())
    }
}
