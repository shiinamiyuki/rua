use std::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    fmt::Debug,
    marker::PhantomData,
    process::abort,
    rc::Rc,
    sync::Arc,
};

use ordered_float::OrderedFloat;
use smallvec::SmallVec;

use crate::{
    closure::{Closure, UpValue, UpValueInner},
    compile::UpValueInfo,
    context::Context,
    error::{Error, ErrorKind},
    protected,
    thread::Thread,
    u32_from_3xu8,
    value::{Value}, table::Table,
};

pub(crate) const MAX_REG: usize = 256;
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LoadConst {
    Nil,
    True,
    False,
}
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum UnaryOp {
    Not,
    Neg,
    Unwrap,
}
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    BitwiseOr,
    Xor,
}

#[repr(C)]
#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Instruction {
    Nop,
    LoadConst(LoadConst, u8),
    LoadFloat {
        dst: u8,
        bytes: [u8; 2],
    },
    LoadInt {
        dst: u8,
        bytes: [u8; 2],
    },
    LoadString {
        dst: u8,
        str_id: u16,
    },
    // LoadSelf {
    // dst: u8,
    // },
    LoadClosure {
        dst: u8,
        func_id: u16,
    },
    LoadUpvalue {
        dst: u8,
        upvalue: u8,
    },
    StoreUpvalue {
        dst: u8,
        src: u8,
    },
    CloseUpvalue {
        dst: u8,
    },
    NewTable {
        dst: u8,
        is_table: bool,
    },
    LoadTable {
        dst: u8,
        table: u8,
        key: u8,
    },
    StoreTable {
        table: u8,
        key: u8,
        value: u8,
    },
    StoreTableStringKey {
        table: u8,
        value: u8,
    },
    LoadTableStringKey {
        table: u8,
        dst: u8,
    },
    // LoadTableStringKey {
    //     dst:u8,
    //     str_id:u16,
    // },
    LoadGlobal {
        dst: u8,
        name_id: u16,
    },
    StoreGlobal {
        src: u8,
        name_id: u16,
    },
    Move {
        src: u8,
        dst: u8,
    },
    // MoveMulti {
    //     src_list: u16,
    //     dst_list: u16,
    // },
    UnaryInst {
        op: UnaryOp,
        arg: u8,
        ret: u8,
    },
    BinaryInst {
        op: BinaryOp,
        lhs: u8,
        rhs: u8,
        ret: u8,
    },
    MakeTuple {
        start: u8,
        end: u8,
        ret: u8,
    },
    MakeList {
        start: u8,
        end: u8,
        ret: u8,
    },
    Flatmap {
        //followed by a label
        opt: u8,
        ret: u8,
    },
    // CallDirect,
    Call {
        // [function, args...] = [arg_start..arg_end]
        arg_start: u8,
        arg_end: u8,
        ret: u8,
    },
    // CallMethodDirect,
    CallMethod {
        // [self, function name, args...] = [arg_start..arg_end]
        arg_start: u8,
        arg_end: u8,
        ret: u8,
    },
    Jump,
    JumpConditional {
        cond: bool,
        arg: u8,
    },
    Return {
        arg: u8,
    },
    Byte([u8; 3]), // 3+3+2 = 8
    Label([u8; 3]),
    JumpAddress([u8; 3]),
    Pop,
}
pub(crate) enum ConstantsIndex {
    MtNumber,
    MtString,
    MtBool,
    MtClosure,
    MtKeyIndex,
    MtKeyNewIndex,
    MtKeyAdd,
    MtKeySub,
    MtKeyMul,
    MtKeyDiv,
    MtKeyIDiv,
    MtKeyMod,
    MtKeyPow,
    MtKeyBitwiseAnd,
    MtKeyBitwiseXor,
    MtKeyBitwiseOr,
    MtKeyCmp,
    MtKeyLt,
    MtKeyGt,
    MtKeyLe,
    MtKeyGe,
    MtKeyEq,
    MtKeyNe,
    MtKeyNeg,
    MtKeyCall,
    MtKeyToString,
    NumConstants,
}

#[derive(Clone, Debug)]
pub struct CompiledFunction {
    // pub(crate) entry: usize,
    pub(crate) n_args: usize,
    pub(crate) is_method: bool,
    // pub(crate) has_varargs: bool,
    pub(crate) code: Vec<Instruction>,
    pub(crate) upvalues: Vec<UpValueInfo>,
}
#[derive(Clone)]
pub struct ByteCodeModule {
    pub(crate) string_pool: Vec<String>,
    pub(crate) string_pool_cache: Vec<Value>,
    pub(crate) functions: Vec<CompiledFunction>, // functions[0] is the toplevel code
                                                 // pub(crate) debug_info: ModuleDebugInfo,
}

impl Debug for ByteCodeModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ByteCodeModule")
            .field("functions", &self.functions)
            .field("string_pool", &self.string_pool)
            .finish()
    }
}

#[allow(dead_code)]
pub(crate) struct Vm;

macro_rules! binary_op_impl {
    ($self:expr, $op:tt,$a:expr,$b:expr,$metamethod:ident) => {
        match ($a, $b) {
            (Value::Number(x), Value::Number(y)) => {
                Ok(Value::from((x $op y).0))
            }
            _ => {dbg!((stringify!($op), $a,$b));todo!()},
        }

    };
}
macro_rules! int_binary_op_impl {
    ($self:expr, $op:tt,$a:expr,$b:expr,$metamethod:ident) => {

        match ($a.as_int_exact(), $b.as_int_exact()) {
            (Some(x), Some(y)) => Ok(Value::from(x $op y)),
            _ => todo!(),
        }

    };
}

macro_rules! cmp_op_impl {
    ($thread:expr, $map:expr,$op:tt, $a:expr,$b:expr) => {
        if let Some(ordering) = Self::cmp($thread, $a, $b)? {
            Ok(Value::Bool($map(ordering)))
        } else {
            Err(Error {
                kind: ErrorKind::ArithmeticError,
                msg: format!(
                    "attempt to perform {} {} with {}",
                    stringify!($op),
                    $a.type_of(),
                    $b.type_of()
                ),
            })
        }
    };
}
pub(crate) enum TableAccessError {
    NoSuchKey,
    InvalidTableType,
    InvalidKeyType,
    KeyOutOfRange,
}
enum Continue {
    Return,
    NewFrame { ret: u8 },
}

impl Vm {
    pub(crate) fn add(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        binary_op_impl!(thread, +, a,b, MtKeyAdd)
    }
    pub(crate) fn sub(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        binary_op_impl!(thread, -, a,b, MtKeySub)
    }
    pub(crate) fn mul(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        binary_op_impl!(thread, *, a,b, MtKeyMul)
    }
    pub(crate) fn div(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        binary_op_impl!(thread,/, a,b, MtKeyDiv)
    }
    pub(crate) fn mod_(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        binary_op_impl!(thread,%, a,b,MtKeyMod)
    }
    pub(crate) fn pow(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        match (a, b) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::from(x.0.powf(y.0))),
            _ => todo!(),
        }
    }
    pub(crate) fn idiv(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        int_binary_op_impl!(thread,/, a, b,MtKeyIDiv)
    }
    pub(crate) fn bitwise_and(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        int_binary_op_impl!(thread,&, a, b,MtKeyBitwiseAnd)
    }
    pub(crate) fn bitwise_xor(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        int_binary_op_impl!(thread,&, a, b,MtKeyBitwiseXor)
    }
    pub(crate) fn bitwise_or(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        int_binary_op_impl!(thread,|, a, b,MtKeyBitwiseOr)
    }
    pub(crate) fn lt(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Less=>true,
            _=>false
        },<,a,b)
    }
    pub(crate) fn le(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Less=>true,
            Ordering::Equal=>true,
            _=>false
        },<=,a,b)
    }
    pub(crate) fn gt(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Greater=>true,
            _=>false
        },>,a,b)
    }
    pub(crate) fn ge(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Greater=>true,
            Ordering::Equal=>true,
            _=>false
        },>=,a,b)
    }
    pub(crate) fn eq(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Equal=>true,
            _=>false
        },==,a,b)
    }
    pub(crate) fn ne(thread: &mut Thread, a: Value, b: Value) -> Result<Value, Error> {
        cmp_op_impl!(thread,|ord|match ord{
            Ordering::Equal=>false,
            _=>true
        },==,a,b)
    }

    pub(crate) fn cmp(thread: &mut Thread, a: Value, b: Value) -> Result<Option<Ordering>, Error> {
        let ord = a.partial_cmp(&b);
        // #[cfg(debug_assertions)]
        // {
        //     match res {
        //         Ok(Some(ordering)) => {
        //             if ordering == Ordering::Equal {
        //                 debug_assert!(a == b);
        //             } else {
        //                 debug_assert!(a != b);
        //             }
        //         }
        //         _ => {}
        //     }
        // }
        Ok(ord)
    }
    pub(crate) fn table_rawget(table: Value, key: Value) -> Result<Value, TableAccessError> {
        if let Some(table) = table.as_table() {
            let table = table.read();
            match table.get(key) {
                Value::Nil => Err(TableAccessError::NoSuchKey),
                x @ _ => Ok(x),
            }
        } else {
            Err(TableAccessError::InvalidTableType)
        }
    }
    pub(crate) fn table_rawset(
        table: Value,
        key: Value,
        value: Value,
    ) -> Result<(), TableAccessError> {
        if let Some(table) = table.as_table() {
            let mut table = table.write();
            table.set(key, value);
            Ok(())
        } else {
            Err(TableAccessError::InvalidTableType)
        }
    }
    pub(crate) fn table_get(
        context: &Context,
        thread: &mut Thread,
        table: Value,
        key: Value,
    ) -> Result<Value, Error> {
        let raw = Self::table_rawget(table, key);
        match raw {
            Ok(x) => Ok(x),
            Err(_) => {
                todo!()
            }
        }
    }
    pub(crate) fn table_set(
        context: &Context,
        thread: &mut Thread,
        table: Value,
        key: Value,
        value: Value,
    ) -> Result<(), Error> {
        let raw = Self::table_rawset(table, key, value);
        match raw {
            Ok(_) => Ok(()),
            Err(_) => todo!(),
        }
    }
    pub(crate) fn execute(context: &Context, thread: &mut Thread) -> Result<(), Error> {
        Self::execute_impl(context, thread)
    }
    fn execute_impl(context: &Context, thread: &mut Thread) -> Result<(), Error> {
        loop {
            if thread.finished {
                break;
            }
            match Self::execute_frame(context, thread) {
                Ok(cont) => match cont {
                    Continue::NewFrame { ret: _ } => {}
                    Continue::Return => {
                        let ret_val = thread.pop_stack_frame().ret;
                        if !thread.finished {
                            let ret = thread.top_frame().ret_reg.unwrap();
                            *thread.local_mut(ret as usize) = ret_val;
                        }
                    }
                },
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(())
    }
    fn execute_frame(context: &Context, thread: &mut Thread) -> Result<Continue, Error> {
        let module: *const ByteCodeModule = Arc::as_ptr(&thread.top_frame().closure.module);
        loop {
            let module = unsafe { &*module };
            let cur_function = &module.functions[thread.top_frame().closure.entry];
            let code = &cur_function.code;
            let mut ip = thread.top_frame().ip;
            if ip == 0 {
                let closure = thread.top_frame().closure;
                // init upvalues
                for (i, u) in cur_function.upvalues.iter().enumerate() {
                    match u {
                        UpValueInfo::Parent { location, .. } => {
                            let mut v = closure.upvalues[i].inner.borrow_mut();
                            *v = Arc::new(Cell::new(UpValueInner::Open(
                                thread.local_mut(*location as usize) as *mut Value,
                            )));
                        }
                        _ => {}
                    }
                }
            }
            // macro_rules! save_ip {
            //     ($e:expr) => {{
            //         thread.top_frame_mut().ip = $e;
            //     }};
            //     () => {{
            //         save_ip!(ip + 1);
            //     }};
            // }
            while ip < code.len() {
                let instruction = code[ip];
                let mut ip_modified = false;
                macro_rules! binary_op_impl {
                    ($func:ident,$a:expr,$b:expr,$c:expr) => {{
                        let a = thread.local($a as usize);
                        let b = thread.local($b as usize);
                        *thread.local_mut($c as usize) = Self::$func(thread, a, b)?;
                    }};
                }
                match instruction {
                    Instruction::BinaryInst { op, lhs, ret, rhs } => match op {
                        BinaryOp::Add => binary_op_impl!(add, lhs, rhs, ret),
                        BinaryOp::Sub => binary_op_impl!(sub, lhs, rhs, ret),
                        BinaryOp::Mul => binary_op_impl!(mul, lhs, rhs, ret),
                        BinaryOp::Div => binary_op_impl!(div, lhs, rhs, ret),
                        BinaryOp::IDiv => binary_op_impl!(idiv, lhs, rhs, ret),
                        BinaryOp::Mod => binary_op_impl!(mod_, lhs, rhs, ret),
                        BinaryOp::Pow => binary_op_impl!(pow, lhs, rhs, ret),
                        BinaryOp::LessThan => binary_op_impl!(lt, lhs, rhs, ret),
                        BinaryOp::LessThanEqual => binary_op_impl!(le, lhs, rhs, ret),
                        BinaryOp::GreaterThan => binary_op_impl!(gt, lhs, rhs, ret),
                        BinaryOp::GreaterThanEqual => binary_op_impl!(ge, lhs, rhs, ret),
                        BinaryOp::Equal => binary_op_impl!(eq, lhs, rhs, ret),
                        BinaryOp::NotEqual => binary_op_impl!(ne, lhs, rhs, ret),
                        BinaryOp::BitwiseAnd => binary_op_impl!(bitwise_and, lhs, rhs, ret),
                        BinaryOp::BitwiseOr => binary_op_impl!(bitwise_or, lhs, rhs, ret),
                        BinaryOp::Xor => binary_op_impl!(bitwise_xor, lhs, rhs, ret),
                    },
                    Instruction::Return { arg } => {
                        thread.top_frame_mut().ret = thread.local(arg as usize);
                        break;
                    }
                    Instruction::CallMethod {
                        arg_start,
                        arg_end,
                        ret,
                    } => {
                        let func = thread.local(arg_start as usize);
                        let object = thread.local((arg_start + 1) as usize);
                        match func {
                            Value::Callable(callable) => {
                                let st = if callable.is_method() {
                                    arg_start as usize + 1
                                } else {
                                    arg_start as usize + 2
                                };
                                let r =
                                    callable.call(context, thread.locals(st, arg_end as usize))?;
                                *thread.local_mut(ret as usize) = r;
                            }
                            Value::Closure(closure) => {
                                let st = if closure.is_method {
                                    arg_start as usize + 1
                                } else {
                                    arg_start as usize + 2
                                };
                                let args: SmallVec<[Value; 8]> = thread
                                    .locals(st, arg_end as usize)
                                    .iter()
                                    .map(|x| *x)
                                    .collect();
                                thread.top_frame_mut().ret_reg = Some(ret);
                                thread.top_frame_mut().ip = ip + 1;
                                thread.create_stack_frame(closure);
                                thread.top_frame_mut().self_ = object;
                                thread.locals_mut(0, args.len()).copy_from_slice(&args);
                                return Ok(Continue::NewFrame { ret });
                            }
                            _ => todo!(),
                        }
                    }
                    Instruction::Call {
                        arg_start,
                        arg_end,
                        ret,
                    } => {
                        let func = thread.local(arg_start as usize);
                        match func {
                            Value::Callable(callable) => {
                                let r = callable.call(
                                    context,
                                    thread.locals(arg_start as usize + 1, arg_end as usize),
                                )?;
                                *thread.local_mut(ret as usize) = r;
                            }
                            Value::Closure(closure) => {
                                let args: SmallVec<[Value; 8]> = thread
                                    .locals(arg_start as usize + 1, arg_end as usize)
                                    .iter()
                                    .map(|x| *x)
                                    .collect();
                                thread.top_frame_mut().ret_reg = Some(ret);
                                thread.top_frame_mut().ip = ip + 1;
                                thread.create_stack_frame(closure);
                                thread.locals_mut(0, args.len()).copy_from_slice(&args);
                                return Ok(Continue::NewFrame { ret });
                            }
                            _ => todo!(),
                        }
                    }
                    Instruction::LoadString { dst, str_id } => {
                        *thread.local_mut(dst as usize) =
                            context.new_string(module.string_pool[str_id as usize].clone());
                    }
                    Instruction::LoadGlobal { dst, name_id } => {
                        let key = module.string_pool_cache[name_id as usize];
                        let key = match key {
                            Value::String(s) => s,
                            _ => {
                                unreachable!()
                            }
                        };
                        if let Some(v) = context.get_global(key) {
                            *thread.local_mut(dst as usize) = v;
                        } else {
                            return Err(Error {
                                kind: crate::error::ErrorKind::NameError,
                                msg: format!(
                                    "undefined global variable '{}'",
                                    module.string_pool[name_id as usize]
                                ),
                            });
                        }
                        //
                    }
                    // Instruction::LoadSelf { dst } => {
                    //     *thread.local_mut(dst as usize) = thread.top_frame().self_;
                    // }
                    Instruction::LoadConst(c, dst) => {
                        let v = match c {
                            LoadConst::Nil => Value::nil(),
                            LoadConst::True => Value::Bool(true),
                            LoadConst::False => Value::Bool(false),
                        };
                        *thread.local_mut(dst as usize) = v;
                    }
                    Instruction::LoadInt { dst, bytes } => {
                        let x = match code[ip + 1] {
                            Instruction::Byte(x) => x,
                            _ => unreachable!(),
                        };
                        let y = match code[ip + 2] {
                            Instruction::Byte(x) => x,
                            _ => unreachable!(),
                        };
                        ip += 3;
                        ip_modified = true;
                        *thread.local_mut(dst as usize) = Value::from(i64::from_le_bytes([
                            bytes[0], bytes[1], x[0], x[1], x[2], y[0], y[1], y[2],
                        ]))
                    }
                    Instruction::LoadFloat { dst, bytes } => {
                        let x = match code[ip + 1] {
                            Instruction::Byte(x) => x,
                            _ => unreachable!(),
                        };
                        let y = match code[ip + 2] {
                            Instruction::Byte(x) => x,
                            _ => unreachable!(),
                        };
                        ip += 3;
                        ip_modified = true;
                        *thread.local_mut(dst as usize) = Value::from(f64::from_le_bytes([
                            bytes[0], bytes[1], x[0], x[1], x[2], y[0], y[1], y[2],
                        ]))
                    }
                    Instruction::StoreGlobal { src, name_id } => {
                        let key = module.string_pool_cache[name_id as usize];
                        let key = match key {
                            Value::String(s) => s,
                            _ => {
                                unreachable!()
                            }
                        };
                        context.set_global(key, thread.local(src as usize));
                    }
                    Instruction::Move { dst, src } => {
                        *thread.local_mut(dst as usize) = thread.local(src as usize);
                    }
                    Instruction::LoadClosure { dst, func_id } => {
                        let module = thread.top_frame().closure.module.clone();
                        let proto = &module.functions[func_id as usize];
                        let upvalues = proto
                            .upvalues
                            .iter()
                            .map(|u| match u {
                                UpValueInfo::Parent { .. } => UpValue {
                                    inner: RefCell::new(Arc::new(Cell::new(UpValueInner::Empty))),
                                },
                                UpValueInfo::Child {
                                    parent_location, ..
                                } => thread.top_frame().closure.upvalues[*parent_location as usize]
                                    .clone(),
                            })
                            .collect();
                        let closure = Closure {
                            entry: func_id as usize,
                            module: module.clone(),
                            stack_frame_size: MAX_REG,
                            upvalues,
                            is_method: proto.is_method,
                        };
                        *thread.local_mut(dst as usize) = context.new_closure(closure);
                    }
                    Instruction::NewTable { dst, is_table } => {
                        *thread.local_mut(dst as usize) = context.new_table(Table::new());
                    }
                    Instruction::StoreTable { table, key, value } => {
                        let table = thread.local(table as usize);
                        let key = thread.local(key as usize);
                        let value = thread.local(value as usize);
                        Self::table_set(context, thread, table, key, value)?;
                    }
                    Instruction::StoreTableStringKey { table, value } => {
                        let table = thread.local(table as usize);
                        let value = thread.local(value as usize);
                        ip_modified = true;
                        let sid = match code[ip + 1] {
                            Instruction::Byte(x) => u32_from_3xu8(x) as usize,
                            _ => unreachable!(),
                        };
                        Self::table_set(
                            context,
                            thread,
                            table,
                            module.string_pool_cache[sid],
                            value,
                        )?;
                        ip += 2;
                    }
                    Instruction::LoadTable { table, key, dst } => {
                        let table = thread.local(table as usize);
                        let key = thread.local(key as usize);
                        *thread.local_mut(dst as usize) =
                            Self::table_get(context, thread, table, key)?;
                    }
                    Instruction::LoadTableStringKey { table, dst } => {
                        let table = thread.local(table as usize);
                        ip_modified = true;
                        let sid = match code[ip + 1] {
                            Instruction::Byte(x) => u32_from_3xu8(x) as usize,
                            _ => unreachable!(),
                        };
                        *thread.local_mut(dst as usize) =
                            Self::table_get(context, thread, table, module.string_pool_cache[sid])?;
                        ip += 2;
                    }
                    Instruction::LoadUpvalue { dst, upvalue } => {
                        let closure = thread.top_frame().closure;
                        *thread.local_mut(dst as usize) =
                            match closure.upvalues[upvalue as usize].inner.borrow().get() {
                                UpValueInner::Empty => unreachable!(),
                                UpValueInner::Open(p) => unsafe { *p },
                                UpValueInner::Closed(p) => p,
                            };
                    }
                    Instruction::StoreUpvalue { dst, src } => {
                        let closure = thread.top_frame().closure;
                        let src = thread.local(src as usize);
                        match closure.upvalues[dst as usize].inner.borrow().get() {
                            UpValueInner::Empty => unreachable!(),
                            UpValueInner::Open(p) => unsafe {
                                *(&mut *p) = src;
                            },
                            UpValueInner::Closed(_) => closure.upvalues[dst as usize]
                                .inner
                                .borrow()
                                .set(UpValueInner::Closed(src)),
                        };
                    }
                    Instruction::CloseUpvalue { dst } => {
                        let closure = thread.top_frame().closure;
                        closure.close(dst as usize);
                    }
                    Instruction::Jump => {
                        let addr = code[ip + 1];
                        ip = match addr {
                            Instruction::JumpAddress(x) => u32_from_3xu8(x) as usize,
                            _ => unreachable!(),
                        };
                        ip_modified = true;
                    }
                    Instruction::JumpConditional { cond, arg } => {
                        let a = thread.local(arg as usize);
                        let b = match a {
                            Value::Nil => false,
                            Value::Bool(b) => b,
                            _ => {
                                return Err(Error {
                                    kind: ErrorKind::ArithmeticError,
                                    msg: format!(
                                        "null or boolean expected in conditional but found {}",
                                        a.type_of()
                                    ),
                                });
                            }
                        };
                        if b == cond {
                            let addr = code[ip + 1];
                            ip = match addr {
                                Instruction::JumpAddress(x) => u32_from_3xu8(x) as usize,
                                _ => unreachable!(),
                            };
                            ip_modified = true;
                        } else {
                            ip += 2;
                            ip_modified = true;
                        }
                    }
                    i @ _ => unimplemented!("{:?}", i),
                }
                if !ip_modified {
                    ip += 1;
                }
            }
            break;
        }
        let closure = thread.top_frame().closure;
        for i in 0..closure.upvalues.len() {
            closure.close(i);
        }
        Ok(Continue::Return)
    }
}
