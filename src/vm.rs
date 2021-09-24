use crate::{
    api::{BaseApi, StateApi},
    bytecode::{get_3xu8, u32_from_3xu8, ByteCode, ByteCodeModule, OpCode},
    closure::{Callable, Closure, UpValue, UpValueInner},
    gc::{Gc, GcState, Traceable},
    runtime::{ConstantsIndex, ErrorKind, RuntimeError, RuntimeInner, ValueRef},
    state::{CallContext, Frame, State},
    table::Table,
    value::{Managed, ManagedCell, Tuple, TupleFlag, Value},
    Stack,
};
use smallvec::smallvec;
use std::{
    cell::{Cell, RefCell, RefMut},
    collections::HashMap,
    iter::FromIterator,
    rc::Rc,
};

/*
The instances represents a thread
*/
pub struct Instance {
    pub(crate) gc: Rc<GcState>,
    pub(crate) state: State,
    pub(crate) runtime: Rc<RefCell<RuntimeInner>>,
}

enum Continue {
    Return,
    NewFrame(Frame, u8),
    CallExt(Gc<Box<dyn Callable>>, Frame, u8),
}
macro_rules! new_frame {
    ($gc:expr, $eval_stack:expr, $n_args:expr,$closure:expr) => {{
        let closure: Option<Gc<Closure>> = $closure;
        let mut frame = Frame::new($n_args, $closure);
        let is_ext = closure.is_none();
        let n_parameters: usize = closure.map_or(u8::MAX as usize, |c| c.n_args);
        let has_varargs = closure.map_or(false, |c| c.has_varargs);
        let n_arg_to_local = if is_ext {
            $n_args
        } else {
            ($n_args).min(n_parameters)
        };
        for i in 0..n_arg_to_local {
            frame.locals[i] = $eval_stack[$eval_stack.len() - 1 - i];
        }
        if !is_ext && has_varargs {
            let mut tv = smallvec![];
            for i in n_parameters..$n_args {
                tv.push($eval_stack[$eval_stack.len() - 1 - i]);
            }
            frame.locals[n_parameters] = Value::Tuple($gc.allocate(Tuple {
                values: RefCell::new(tv),
                flag: TupleFlag::VarArgs,
                metatable: Cell::new(Value::Nil),
            }));
        }
        let len = $eval_stack.len();
        $eval_stack.resize(len - $n_args, Value::nil());
        frame
    }};
}
macro_rules! new_frame_direct {
    ($gc:expr, $args:expr,$closure:expr) => {{
        let n_args = $args.len();
        let closure: Option<Gc<Closure>> = $closure;
        let mut frame = Frame::new(n_args, $closure);
        let is_ext = closure.is_none();
        let n_parameters: usize = closure.map_or(u8::MAX as usize, |c| c.n_args);
        let has_varargs = closure.map_or(false, |c| c.has_varargs);
        let n_arg_to_local = if is_ext {
            n_args
        } else {
            (n_args).min(n_parameters)
        };
        for i in 0..n_arg_to_local {
            frame.locals[i] = $args[i];
        }
        if !is_ext && has_varargs {
            let mut tv = smallvec![];
            for i in n_parameters..n_args {
                tv.push($args[i]);
            }
            frame.locals[n_parameters] = Value::Tuple($gc.allocate(Tuple {
                values: RefCell::new(tv),
                flag: TupleFlag::VarArgs,
                metatable: Cell::new(Value::Nil),
            }));
        }
        frame
    }};
}
struct RetGuard {
    bomb: bool,
}
impl RetGuard {
    fn disarm(&mut self) {
        self.bomb = false;
    }
}
impl Drop for RetGuard {
    fn drop(&mut self) {
        assert!(self.bomb == false);
    }
}
impl Instance {
    /*

    */
    // pub fn lock<'a>(&self) -> RefMut<State> {
    //     self.state.borrow_mut()
    // }
    pub(crate) fn call(&self, closure: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        match closure {
            Value::Closure(closure) => {
                let frame = { new_frame_direct!(self.gc, args, Some(closure)) };
                let level = {
                    let mut frames = self.state.frames.borrow_mut();
                    let level = frames.len();
                    frames.push(frame);
                    level
                };
                self.exec_frames_loop(level)?;
                {
                    let mut eval_stack = self.state.eval_stack.borrow_mut();
                    Ok(eval_stack.pop().unwrap())
                }
            }
            Value::Callable(callable) => {
                let frame = { new_frame_direct!(self.gc, args, None) };
                {
                    let mut frames = self.state.frames.borrow_mut();
                    frames.push(frame);
                }
                {
                    let frames = self.state.frames.borrow();
                    let ctx = CallContext {
                        instance: self,
                        state: &self.state,
                        frames: &*frames,
                        ret_values: RefCell::new(smallvec![]),
                        n_expected_rets: u8::MAX,
                    };
                    let func = { &(*callable) };
                    func.call(&ctx)?;
                }
                {
                    let mut frames = self.state.frames.borrow_mut();
                    frames.pop().unwrap();
                }
                {
                    let mut eval_stack = self.state.eval_stack.borrow_mut();
                    Ok(eval_stack.pop().unwrap())
                }
            }
            _ => Err(RuntimeError {
                kind: ErrorKind::ArithmeticError,
                msg: format!("attempt to call a {} value", closure.type_of()),
            }),
        }
    }
    fn exec_frame(&self, state: &State, mut n_expected_rets: u8) -> Result<Continue, RuntimeError> {
        let mut guard = RetGuard { bomb: false };
        let cur_closure = {
            let mut frames = state.frames.borrow_mut();
            let frame = frames.last_mut().unwrap();
            let closure = &frame.closure.unwrap();
            if closure.proto_idx == usize::MAX {
                assert!(closure.upvalues.len() == 1);
            } else {
                for (i, v) in closure.upvalues.iter().enumerate() {
                    let proto = &(*closure.module).protypes[closure.proto_idx];
                    let info = &proto.upvalues[i];
                    if i == 0 && info.is_special {
                        let mut v = v.inner.borrow_mut();
                        *v = Rc::new(Cell::new(UpValueInner::Closed(self.state.globals)));
                    } else {
                        if !info.from_parent {
                            let mut v = v.inner.borrow_mut();
                            *v = Rc::new(Cell::new(UpValueInner::Open(
                                &mut frame.locals[info.location as usize] as *mut Value,
                            )));
                        }
                    }
                }
            }
            closure.clone()
        };
        let (mut ip, code_len, module) = {
            let mut frames = state.frames.borrow_mut();
            let frame = frames.last_mut().unwrap();
            let module = frame.closure.unwrap().module.clone();
            (frame.ip, module.code.len(), module)
        };
        macro_rules! on_return {
            ($ret:expr) => {{
                guard.disarm();
                let mut frames = state.frames.borrow_mut();
                let frame = frames.last_mut().unwrap();
                frame.ip = ip;
                return $ret;
            }};
        }
        while ip < code_len {
            let mut eval_stack = state.eval_stack.borrow_mut();
            macro_rules! binary_op_impl {
                ($func:ident) => {{
                    let rhs = eval_stack.pop().unwrap();
                    let mut lhs = *eval_stack.last_mut().unwrap();
                    std::mem::drop(eval_stack);
                    lhs = state.$func(lhs, rhs)?;
                    let mut eval_stack = state.eval_stack.borrow_mut();
                    *eval_stack.last_mut().unwrap() = lhs;
                }};
            }
            let instruction = module.code[ip];
            #[cfg(debug_assertions)]
            {
                if let Ok(s) = std::env::var("PRINT_INST") {
                    if s == "1" {
                        println!("{} {:?}", ip, instruction);
                    }
                }
            }

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
                    OpCode::Self_ => {
                        let method = eval_stack.pop().unwrap();
                        let table = eval_stack.last().unwrap();
                        let f = state.table_get(*table, method)?;
                        eval_stack.push(f);
                    }
                    OpCode::Not => {
                        let top = eval_stack.last_mut().unwrap();
                        *top = state.not(*top)?;
                    }
                    OpCode::BitwiseNot => {
                        let top = eval_stack.last_mut().unwrap();
                        *top = state.bitwise_not(*top)?;
                    }
                    OpCode::Neg => {
                        let top = eval_stack.last_mut().unwrap();
                        *top = state.neg(*top)?;
                    }
                    OpCode::Len => {
                        let top = eval_stack.last_mut().unwrap();
                        *top = state.len(*top)?;
                    }
                    OpCode::LoadNumber => {
                        ip_modified = true;
                        let lo = match module.code[ip + 1] {
                            ByteCode::FloatLo(bytes) => bytes,
                            _ => unreachable!(),
                        };
                        let hi = match module.code[ip + 2] {
                            ByteCode::FloatHi(bytes) => bytes,
                            _ => unreachable!(),
                        };
                        let bytes = [lo[0], lo[1], lo[2], lo[3], hi[0], hi[1], hi[2], hi[3]];
                        eval_stack.push(Value::from_number(f64::from_le_bytes(bytes)));
                        ip += 3;
                    }
                    // OpCode::LoadGlobal => {
                    //     let name = eval_stack.pop().unwrap();
                    //     std::mem::drop(eval_stack);
                    //     {
                    //         let globals = cur_closure.upvalues[0].inner.borrow();
                    //         let globals = match globals.get() {
                    //             UpValueInner::Closed(v) => v,
                    //             _ => unreachable!(),
                    //         };
                    //         let v = self.state.table_get(globals, name)?;
                    //         let mut eval_stack = state.eval_stack.borrow_mut();
                    //         eval_stack.push(v);
                    //     }
                    // }
                    OpCode::LoadTable => {
                        let table = eval_stack.pop().unwrap();
                        let key = eval_stack.pop().unwrap();
                        std::mem::drop(eval_stack);
                        let v = state.table_get(table, key)?;
                        let mut eval_stack = state.eval_stack.borrow_mut();
                        eval_stack.push(v);
                    }
                    OpCode::StoreTable => {
                        let table = eval_stack.pop().unwrap();
                        let key = eval_stack.pop().unwrap();
                        let value = eval_stack.pop().unwrap();
                        std::mem::drop(eval_stack);
                        self.state.table_set(table, key, value)?;
                    }
                    // OpCode::StoreGlobal => {
                    //     let name = eval_stack.pop().unwrap();
                    //     let v = eval_stack.pop().unwrap();
                    //     std::mem::drop(eval_stack);
                    //     {
                    //         let globals = cur_closure.upvalues[0].inner.borrow();
                    //         let globals = match globals.get() {
                    //             UpValueInner::Closed(v) => v,
                    //             _ => unreachable!(),
                    //         };
                    //         self.state.table_set(globals, name, v)?;
                    //     }
                    //     // self.state.set_global(name, v);
                    // }
                    OpCode::LoadNil => {
                        eval_stack.push(Value::nil());
                    }
                    OpCode::LoadTrue => {
                        eval_stack.push(Value::from_bool(true));
                    }
                    OpCode::LoadFalse => {
                        eval_stack.push(Value::from_bool(false));
                    }
                    OpCode::And => {
                        binary_op_impl!(bitwise_and)
                    }
                    OpCode::Or => {
                        binary_op_impl!(bitwise_or)
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
                    OpCode::Pow => {
                        binary_op_impl!(pow)
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
                        let addr = match module.code[ip + 1] {
                            ByteCode::Address(bytes) => u32::from_le_bytes(bytes),
                            _ => unreachable!(),
                        };
                        ip = addr as usize;
                    }
                    OpCode::Return => {
                        assert!(n_expected_rets > 0);
                        if n_expected_rets != u8::MAX {
                            let ret = *eval_stack.last().unwrap();
                            match ret {
                                Value::Tuple(tuple) => {
                                    if tuple.flag == TupleFlag::VarArgs {
                                        eval_stack.pop();
                                        let values = tuple.values.borrow();
                                        for i in 0..(n_expected_rets as usize).min(values.len()) {
                                            eval_stack.push(values[i]);
                                        }
                                        for _ in values.len()..(n_expected_rets as usize) {
                                            eval_stack.push(Value::Nil);
                                        }
                                    }
                                }
                                _ => {
                                    if n_expected_rets > 1 {
                                        for _ in 1..n_expected_rets {
                                            eval_stack.push(Value::Nil);
                                        }
                                    }
                                }
                            };
                            // *eval_stack.last_mut().unwrap() = ret;
                        }
                        break;
                    }
                    _ => panic!("unreachable, instruction is {:#?}", instruction),
                },
                ByteCode::Op3U8(op, operands) => match op {
                    // OpCode::Unpack => {
                    //     let cnt = u32_from_3xu8(operands) as usize;
                    //     let v = eval_stack.pop().unwrap();
                    //     match v {
                    //         Value::Tuple(t) => {
                    //             if t.flag == TupleFlag::VarArgs {

                    //             } else {
                    //                 eval_stack.push(v);
                    //                 for _ in 1..cnt {
                    //                     eval_stack.push(Value::Nil);
                    //                 }
                    //             }
                    //         }
                    //         _ => {
                    //             eval_stack.push(v);
                    //             for _ in 1..cnt {
                    //                 eval_stack.push(Value::Nil);
                    //             }
                    //         }
                    //     }
                    // }
                    OpCode::Pack => {
                        let cnt = u32_from_3xu8(operands) as usize;
                        let mut tv = smallvec![];
                        for i in 0..cnt {
                            tv.push(eval_stack[eval_stack.len() + i - cnt]);
                        }
                        let st_len = eval_stack.len() - cnt;
                        eval_stack.resize(st_len, Value::Nil);
                        eval_stack.push(Value::Tuple(self.gc.allocate(Tuple {
                            values: RefCell::new(tv),
                            flag: TupleFlag::VarArgs,
                            metatable: Cell::new(Value::nil()),
                        })));
                    }
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
                        eval_stack.push(module.string_pool_cache[idx as usize]);
                        // eval_stack
                        // .push(state.create_string(module.string_pool[idx as usize].clone()));
                    }
                    OpCode::LoadTableStringKey => {
                        let table = eval_stack.pop().unwrap();
                        let idx = u32_from_3xu8(operands);
                        let key = module.string_pool_cache[idx as usize];
                        std::mem::drop(eval_stack);
                        let v = state.table_get(table, key)?;
                        let mut eval_stack = state.eval_stack.borrow_mut();
                        eval_stack.push(v);
                    }
                    OpCode::StoreTableStringKey => {
                        let table = eval_stack.pop().unwrap();
                        let value = eval_stack.pop().unwrap();
                        let idx = u32_from_3xu8(operands);
                        let key = module.string_pool_cache[idx as usize];
                        std::mem::drop(eval_stack);
                        self.state.table_set(table, key, value)?;
                    }
                    OpCode::StoreUpvalue => {
                        let idx = u32_from_3xu8(operands);
                        let v = eval_stack.pop().unwrap();
                        {
                            let mut frames = state.frames.borrow_mut();
                            let frame = frames.last_mut().unwrap();
                            let c = frame.closure.as_ref().unwrap();
                            c.set_upvalue(idx, v);
                        }
                    }
                    OpCode::LoadUpvalue => {
                        let idx = u32_from_3xu8(operands);
                        {
                            let mut frames = state.frames.borrow_mut();
                            let frame = frames.last_mut().unwrap();
                            let c = frame.closure.as_ref().unwrap();
                            eval_stack.push(c.get_upvalue(idx));
                        }
                    }
                    OpCode::LoadLocal => {
                        let idx = operands[0];
                        let mut frames = state.frames.borrow_mut();
                        let frame = frames.last_mut().unwrap();
                        eval_stack.push(frame.locals[idx as usize]);
                    }
                    OpCode::StoreLocal => {
                        let idx = operands[0];
                        let mut frames = state.frames.borrow_mut();
                        let frame = frames.last_mut().unwrap();
                        frame.locals[idx as usize] = eval_stack.pop().unwrap();
                    }
                    OpCode::StoreTableArray => {
                        let cnt = u32_from_3xu8(operands);
                        let table = eval_stack[eval_stack.len() - cnt as usize - 1];
                        if cnt > 0 {
                            for i in 0..(cnt - 1) {
                                self.state.table_rawset(
                                    table,
                                    Value::from_number((i + 1) as f64),
                                    eval_stack[eval_stack.len() + i as usize - cnt as usize],
                                )?;
                            }
                            let last = eval_stack[eval_stack.len() - 1];
                            match last {
                                Value::Tuple(tuple) => match tuple.flag {
                                    crate::value::TupleFlag::Empty => {
                                        self.state.table_rawset(
                                            table,
                                            Value::from_number(cnt as f64),
                                            last,
                                        )?;
                                    }
                                    crate::value::TupleFlag::VarArgs => {
                                        let values = tuple.values.borrow();
                                        for (i, v) in values.iter().enumerate() {
                                            self.state.table_rawset(
                                                table,
                                                Value::from_number((cnt + i as u32) as f64),
                                                *v,
                                            )?;
                                        }
                                    }
                                },
                                _ => {
                                    self.state.table_rawset(
                                        table,
                                        Value::from_number(cnt as f64),
                                        last,
                                    )?;
                                }
                            }
                        }
                        let len = eval_stack.len();
                        eval_stack.resize(len - cnt as usize - 1, Value::Nil);
                    }
                    OpCode::TailCall => {
                        let n_args = operands[0] as usize;
                        let mut func = eval_stack.pop().unwrap();
                        loop {
                            match func {
                                Value::Closure(closure) => {
                                    let mut frames = state.frames.borrow_mut();
                                    let frame = frames.last_mut().unwrap();
                                    ip_modified = true;
                                    ip = Frame::get_ip(Some(closure));
                                    for i in 0..n_args {
                                        frame.locals[i] = eval_stack[eval_stack.len() - 1 - i];
                                    }
                                    frame.n_args = n_args;
                                    let len = eval_stack.len();
                                    eval_stack.resize(len - n_args, Value::nil());
                                    n_expected_rets = operands[1];
                                    break;
                                }
                                Value::Callable(callable) => {
                                    ip += 1;
                                    let mut frame = Frame::new(n_args, None);
                                    for i in 0..n_args {
                                        frame.locals[i] = eval_stack[eval_stack.len() - 1 - i];
                                    }
                                    let len = eval_stack.len();
                                    eval_stack.resize(len - n_args, Value::nil());
                                    on_return!(Ok(Continue::CallExt(callable, frame, operands[1])));
                                }
                                _ => {
                                    let mt = self.state.get_metatable(func);
                                    if mt.is_nil() {
                                        on_return!(Err(RuntimeError {
                                            kind: ErrorKind::TypeError,
                                            msg: format!(
                                                "attempt to call a {} value",
                                                func.type_of()
                                            )
                                        }))
                                    } else {
                                        func = self.state.table_get(
                                            mt,
                                            self.state.global_state.constants
                                                [ConstantsIndex::MtKeyCall as usize]
                                                .get(),
                                        )?;
                                    }
                                }
                            }
                        }
                    }
                    OpCode::Call => {
                        let n_args = operands[0] as usize;
                        let mut func = eval_stack.pop().unwrap();
                        loop {
                            match func {
                                Value::Closure(closure) => {
                                    ip += 1;
                                    let frame =
                                        new_frame!(self.gc, eval_stack, n_args, Some(closure));
                                    on_return!(Ok(Continue::NewFrame(frame, operands[1])));
                                }
                                Value::Callable(callable) => {
                                    ip += 1;
                                    let frame = new_frame!(self.gc, eval_stack, n_args, None);
                                    on_return!(Ok(Continue::CallExt(callable, frame, operands[1])));
                                }
                                _ => {
                                    let mt = self.state.get_metatable(func);
                                    if mt.is_nil() {
                                        on_return!(Err(RuntimeError {
                                            kind: ErrorKind::TypeError,
                                            msg: format!(
                                                "attempt to call a {} value",
                                                func.type_of()
                                            )
                                        }))
                                    } else {
                                        func = self.state.table_get(
                                            mt,
                                            self.state.global_state.constants
                                                [ConstantsIndex::MtKeyCall as usize]
                                                .get(),
                                        )?;
                                    }
                                }
                            }
                        }
                    }
                    OpCode::TestJump => {
                        ip_modified = true;
                        let top = *eval_stack.last().unwrap();
                        let addr = match module.code[ip + 1] {
                            ByteCode::Address(bytes) => u32::from_le_bytes(bytes),
                            _ => unreachable!(),
                        };
                        let b = operands[0];
                        let pop_t = operands[1];
                        let pop_f = operands[1];
                        if top.to_bool() == (b != 0) {
                            ip = addr as usize;
                            if pop_t != 0 {
                                eval_stack.pop();
                            }
                        } else {
                            ip += 2;
                            if pop_f != 0 {
                                eval_stack.pop();
                            }
                        }
                    }
                    OpCode::NewClosure => {
                        ip_modified = true;
                        let proto_idx = u32_from_3xu8(operands);
                        let proto = &module.protypes[proto_idx as usize];
                        let entry = match module.code[ip + 1] {
                            ByteCode::Address(bytes) => u32::from_le_bytes(bytes),
                            _ => unreachable!(),
                        };
                        let mut frames = state.frames.borrow_mut();
                        let frame = frames.last_mut().unwrap();
                        let parent = frame.closure.as_ref().unwrap();
                        let upvalues = proto
                            .upvalues
                            .iter()
                            .enumerate()
                            .map(|(i, info)| {
                                debug_assert!(i == info.id as usize);
                                if info.from_parent {
                                    parent.upvalues[info.location as usize].clone()
                                } else {
                                    UpValue {
                                        inner: RefCell::new(Rc::new(Cell::new(
                                            UpValueInner::Empty,
                                        ))),
                                    }
                                }
                            })
                            .collect();
                        let closure = state.create_closure(Closure {
                            proto_idx: proto_idx as usize,
                            n_args: proto.n_args,
                            entry: entry as usize,
                            module: frame.closure.unwrap().module.clone(),
                            upvalues,
                            has_varargs: proto.has_varargs,
                            called: Cell::new(false),
                        });
                        eval_stack.push(closure);
                        ip += 2;
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
            if !ip_modified {
                ip += 1;
            }
        }
        on_return!(Ok(Continue::Return))
        // println!("{}", eval_stack.last().unwrap().print());
    }
    fn load_module_string(&self, mut module: ByteCodeModule) -> ByteCodeModule {
        {
            module.string_pool_cache.clear();
            let mut runtime = self.runtime.borrow_mut();
            for (_i, s) in module.string_pool.iter().enumerate() {
                let v = runtime.create_pooled_string(s);
                module.string_pool_cache.push(v);
            }
        }
        module
    }
    pub fn exec(&self, module: ByteCodeModule) -> Result<(), RuntimeError> {
        let module = self.load_module_string(module);
        match self.exec_impl(module) {
            Ok(_) => {}
            Err(e) => {
                self.reset();
                return Err(e);
            }
        }
        assert!(self.state.eval_stack.borrow().is_empty());
        Ok(())
    }
    fn reset(&self) {
        let mut st = self.state.eval_stack.borrow_mut();
        st.clear();
        let mut frames = self.state.frames.borrow_mut();
        *frames = Stack::new();
    }
    pub fn eval_repl(&self, module: ByteCodeModule) -> Result<(), RuntimeError> {
        let module = self.load_module_string(module);
        match self.exec_impl(module) {
            Ok(_) => {}
            Err(e) => {
                self.reset();
                return Err(e);
            }
        }
        assert!(self.state.eval_stack.borrow().len() <= 1);
        if self.state.eval_stack.borrow().len() == 1 {
            let mut st = self.state.eval_stack.borrow_mut();
            let v = st.pop().unwrap();
            println!("{}", v.print());
        }
        Ok(())
    }
    // top level
    fn exec_impl(&self, module: ByteCodeModule) -> Result<(), RuntimeError> {
        let closure = self.gc.allocate(Closure {
            entry: 0,
            n_args: 0,
            // n_locals: 0,
            has_varargs: false,
            module: Rc::new(module),
            upvalues: vec![UpValue {
                inner: RefCell::new(Rc::new(Cell::new(UpValueInner::Closed(self.state.globals)))),
            }],
            called: Cell::new(false),
            proto_idx: usize::MAX,
        });
        {
            let mut frames = self.state.frames.borrow_mut();
            frames.push(Frame::new(0, Some(closure)));
        }
        self.exec_frames_loop(0)
    }

    fn exec_frames_loop(&self, level: usize) -> Result<(), RuntimeError> {
        let mut n_expected_rets = 1;
        loop {
            let cont = self.exec_frame(&self.state, n_expected_rets)?;
            match cont {
                Continue::CallExt(callable, frame, n_expected_rets2) => {
                    n_expected_rets = n_expected_rets2;
                    {
                        let mut frames = self.state.frames.borrow_mut();
                        frames.push(frame);
                    }
                    {
                        let frames = self.state.frames.borrow();
                        let func = &(*callable);
                        let ctx = CallContext {
                            instance: self,
                            state: &self.state,
                            frames: &*frames,
                            n_expected_rets,
                            ret_values: RefCell::new(smallvec![]),
                        };
                        func.call(&ctx)?;
                    }
                    {
                        let mut frames = self.state.frames.borrow_mut();
                        frames.pop().unwrap();
                    }
                }
                Continue::NewFrame(frame, n_expected_rets2) => {
                    n_expected_rets = n_expected_rets2;
                    let mut frames = self.state.frames.borrow_mut();
                    frames.push(frame);
                }
                Continue::Return => {
                    let mut frames = self.state.frames.borrow_mut();
                    unsafe {
                        let closure = frames.last().unwrap().closure.as_ref().unwrap();
                        for v in &closure.upvalues {
                            let v = v.inner.borrow();
                            let inner = v.get();
                            match inner {
                                UpValueInner::Open(p) => {
                                    v.set(UpValueInner::Closed(*p));
                                }
                                UpValueInner::Empty => {
                                    unreachable!()
                                }
                                _ => {}
                            }
                        }
                    }
                    frames.pop().unwrap();
                    if frames.len() == level {
                        break;
                    }
                }
            }
        }

        Ok(())
    }
}
impl BaseApi for Instance {
    fn create_number<'a>(&'a self, x: f64) -> crate::runtime::ValueRef<'a> {
        self.runtime.create_number(x)
    }

    fn create_bool<'a>(&self, x: bool) -> crate::runtime::ValueRef<'a> {
        self.runtime.create_bool(x)
    }

    fn create_userdata<'a, T: crate::value::UserData + Traceable>(
        &self,
        userdata: T,
    ) -> crate::runtime::ValueRef<'a> {
        self.runtime.create_userdata(userdata)
    }

    fn create_string<'a>(&self, s: String) -> crate::runtime::ValueRef<'a> {
        self.runtime.create_string(s)
    }

    fn upgrade<'a>(&'a self, v: crate::runtime::ValueRef<'_>) -> crate::runtime::GcValue {
        self.runtime.upgrade(v)
    }

    fn create_closure<'a>(&self, closure: Box<dyn Callable>) -> ValueRef<'a> {
        self.runtime.create_closure(closure)
    }

    fn create_table<'a>(&self) -> ValueRef<'a> {
        self.runtime.create_table()
    }

    fn set_metatable<'a>(&self, v: ValueRef<'a>, mt: ValueRef<'a>) {
        self.runtime.set_metatable(v, mt)
    }

    fn get_metatable<'a>(&self, v: ValueRef<'a>) -> ValueRef<'a> {
        self.runtime.get_metatable(v)
    }

    fn table_rawset<'a>(
        &'a self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
        value: ValueRef<'a>,
    ) -> Result<(), RuntimeError> {
        self.runtime.table_rawset(table, key, value)
    }

    fn table_rawget<'a>(
        &'a self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
    ) -> Result<ValueRef<'a>, RuntimeError> {
        self.runtime.table_rawget(table, key)
    }
}
impl StateApi for Instance {
    fn table_set<'a>(
        &'a self,
        table: crate::runtime::ValueRef<'a>,
        key: crate::runtime::ValueRef<'a>,
        value: crate::runtime::ValueRef<'a>,
    ) -> Result<(), RuntimeError> {
        self.state.table_set(table.value, key.value, value.value)
    }

    fn table_get<'a>(
        &'a self,
        table: crate::runtime::ValueRef<'a>,
        key: crate::runtime::ValueRef<'a>,
    ) -> Result<crate::runtime::ValueRef<'a>, RuntimeError> {
        Ok(ValueRef::new(self.state.table_get(table.value, key.value)?))
    }
}
impl Traceable for Instance {
    fn trace(&self, gc: &GcState) {
        let st = self.state.eval_stack.borrow();
        for v in st.iter() {
            gc.trace(v);
        }
        gc.trace(&self.state.globals);
        let frames = self.state.frames.borrow();
        for f in frames.iter() {
            for v in &f.locals {
                gc.trace(v)
            }
            if let Some(c) = &f.closure {
                gc.trace_ptr(*c);
            }
        }
    }
}
