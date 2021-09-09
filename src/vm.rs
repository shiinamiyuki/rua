use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{
    bytecode::{u32_from_3xu8, ByteCode, ByteCodeModule, OpCode},
    closure::{Callable, Closure},
    gc::Gc,
    runtime::{ErrorKind, RuntimeError},
    state::{CallContext, Frame, State},
    table::Table,
    value::{Managed, ManagedCell, Value, ValueData},
};

/*
The instances represents a thread
*/
pub struct Instance {
    pub(crate) gc: Rc<Gc>,
    pub(crate) state: State,
}

enum Continue {
    Return,
    NewFrame(Frame),
    CallExt(*const Managed<Box<dyn Callable>>, Frame),
}
impl Instance {
    /*

    */
    // pub fn lock<'a>(&self) -> RefMut<State> {
    //     self.state.borrow_mut()
    // }
    fn exec_frame(&self, state: &State) -> Result<Continue, RuntimeError> {
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
                ByteCode::Op(op) => match op {
                    OpCode::Nop => {}
                    OpCode::Dup => {
                        let top = *eval_stack.last().unwrap();
                        eval_stack.push(top);
                    }
                    OpCode::RotBCA => {
                        let i = eval_stack.len() - 1;
                        let (a, b, c) = (eval_stack[i - 2], eval_stack[i - 1], eval_stack[i]);
                        eval_stack[i - 2] = b;
                        eval_stack[i - 1] = c;
                        eval_stack[i] = a;
                    }
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
                    OpCode::LoadGlobal => {
                        let name = eval_stack.pop().unwrap();
                        eval_stack.push(self.state.get_global(name));
                    }
                    OpCode::LoadTable => {
                        let table = eval_stack.pop().unwrap();
                        let key = eval_stack.pop().unwrap();
                        let table = table.as_table().unwrap();
                        let table = table.borrow();
                        eval_stack.push(table.get(key));
                    }
                    OpCode::StoreTable => {
                        let table = eval_stack.pop().unwrap();
                        let key = eval_stack.pop().unwrap();
                        let value = eval_stack.pop().unwrap();
                        let table = table.as_table().unwrap();
                        let mut table = table.borrow_mut();
                        table.set(key, value);
                    }
                    OpCode::StoreGlobal => {
                        let name = eval_stack.pop().unwrap();
                        let v = eval_stack.pop().unwrap();
                        self.state.set_global(name, v);
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
                    OpCode::LessThan => {
                        binary_op_impl!(lt)
                    }
                    OpCode::LessThanEqual => {
                        binary_op_impl!(le)
                    }
                    OpCode::GreaterThan => {
                        binary_op_impl!(gt)
                    }
                    OpCode::GreaterThanEqual => {
                        binary_op_impl!(ge)
                    }
                    OpCode::NotEqual => {
                        binary_op_impl!(ne)
                    }
                    OpCode::Equal => {
                        binary_op_impl!(eq)
                    }
                    OpCode::Pop => {
                        #[cfg(debug_assertions)]
                        eval_stack.pop().unwrap();
                        #[cfg(not(debug_assertions))]
                        eval_stack.pop();
                    }
                    OpCode::Jump => {
                        ip_modified = true;
                        let addr = match module.code[*ip + 1] {
                            ByteCode::Address(bytes) => u32::from_le_bytes(bytes),
                            _ => unreachable!(),
                        };
                        *ip = addr as usize;
                    }
                    OpCode::Return => {
                        break;
                    }
                    _ => panic!("unreachable, instruction is {:#?}", instruction),
                },
                ByteCode::Op3U8(op, operands) => match op {
                    OpCode::NewTable => {
                        let n_array = u16::from_le_bytes([operands[0], operands[1]]) as usize;
                        let n_hash = operands[2] as usize;
                        if n_array == 0 && n_hash == 0 {
                            eval_stack.push(state.create_table(Table::new()));
                        } else {
                            eval_stack.push(
                                state.create_table(Table::new_with(n_array, (1 << n_hash).max(16))),
                            );
                        }
                    }
                    OpCode::LoadStr => {
                        let idx = u32_from_3xu8(operands);
                        eval_stack
                            .push(state.create_string(module.string_pool[idx as usize].clone()));
                    }
                    OpCode::LoadLocal => {
                        let idx = operands[0];
                        eval_stack.push(frame.locals[idx as usize]);
                    }
                    OpCode::StoreLocal => {
                        let idx = operands[0];
                        frame.locals[idx as usize] = eval_stack.pop().unwrap();
                    }
                    OpCode::Call => {
                        let n_args = operands[0] as usize;
                        let func = eval_stack.pop().unwrap();
                        match func.data {
                            ValueData::Closure(closure) => {
                                *ip += 1;
                                let mut frame =
                                    Frame::new(eval_stack.len() - n_args, n_args, closure);
                                for i in 0..n_args {
                                    frame.locals[i] = eval_stack[eval_stack.len() - n_args + i];
                                }
                                let len = eval_stack.len();
                                eval_stack.resize(len - n_args, Value::nil());
                                return Ok(Continue::NewFrame(frame));
                            }
                            ValueData::Callable(callable) => {
                                *ip += 1;
                                let mut frame =
                                    Frame::new(eval_stack.len() - n_args, n_args, std::ptr::null());
                                for i in 0..n_args {
                                    frame.locals[i] = eval_stack[eval_stack.len() - n_args + i];
                                }
                                let len = eval_stack.len();
                                eval_stack.resize(len - n_args, Value::nil());
                                return Ok(Continue::CallExt(callable, frame));
                            }
                            _ => {
                                unimplemented!()
                            }
                        }
                    }
                    OpCode::TestJump => {
                        ip_modified = true;
                        let top = *eval_stack.last().unwrap();
                        let addr = match module.code[*ip + 1] {
                            ByteCode::Address(bytes) => u32::from_le_bytes(bytes),
                            _ => unreachable!(),
                        };
                        let b = operands[0];
                        let pop_t = operands[1];
                        let pop_f = operands[1];
                        if top.as_bool() == (b != 0) {
                            *ip = addr as usize;
                            if pop_t != 0 {
                                eval_stack.pop();
                            }
                        } else {
                            *ip += 2;
                            if pop_f != 0 {
                                eval_stack.pop();
                            }
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
            if !ip_modified {
                *ip += 1;
            }
        }
        // println!("{}", eval_stack.last().unwrap().print());
        Ok(Continue::Return)
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
        loop {
            let cont = self.exec_frame(&self.state)?;
            match cont {
                Continue::CallExt(callable, frame) => {
                    {
                        let mut frames = self.state.frames.borrow_mut();
                        frames.push(frame);
                    }
                    let func = unsafe { &(*callable).data };
                    let ctx = CallContext {
                        state: &self.state,
                        ret_values: RefCell::new(vec![]),
                    };
                    func.call(&ctx);
                    {
                        let mut frames = self.state.frames.borrow_mut();
                        frames.pop().unwrap();
                    }
                }
                Continue::NewFrame(frame) => {
                    let mut frames = self.state.frames.borrow_mut();
                    frames.push(frame);
                }
                Continue::Return => {
                    let mut frames = self.state.frames.borrow_mut();
                    frames.pop().unwrap();
                    if frames.is_empty() {
                        break;
                    }
                }
            }
        }

        Ok(())
    }
}
