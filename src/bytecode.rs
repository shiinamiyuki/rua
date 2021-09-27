use std::fmt::Debug;

use crate::parse::SourceLocation;
use crate::{closure::ClosurePrototype, value::Value};
use std::collections::{BTreeMap, HashMap};

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpCode {
    Nop,

    // No operands
    LoadNil,
    LoadTrue,
    LoadFalse,
    BitwiseNot,
    Not,
    Neg,
    Len,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    Concat,
    And,
    Or,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    Pop,
    Return,
    Dup,
    RotBCA, // stack: A B C -> B C A

    // LoadGlobal,  // TOS = name
    // StoreGlobal, // TOS~1=value TOS=name global[name]= value

    // 3xu8 operand
    /*union _3xu8{
        u8 s[3];
        u24 i;
    };

    }*/
    NewTable, //NewTable array len, hash len
    NewClosure,
    LoadStr,
    LoadLocal,  // PUSH  local[i]
    StoreLocal, // local[i] = TOS;POP
    Pack,
    Unpack, // Unpack TOS to n values,
    StoreTableArray,
    LoadTableStringKey,
    StoreTableStringKey,

    LoadTable,    // TOS = key, table
    StoreTable,   // TOS = value, key, table
    LoadUpvalue,  // TOS=upvalue, push upvalue[i]
    StoreUpvalue, // TOS~1=upvalue, TOS=value upvalue[i] = value

    CloseUpvalue,
    Scatter, // Scatter{n, table_idx} local[scatter_table[table_idx]..] = stack[TOP-n-1..TOP-n]
    Gather,

    // with extended bytes
    LoadNumber,
    Jump,
    Self_,
    Call, // Call{n_args:u8,}, TOS = func, TOS~[1..=n_args] = args, pop func
    TailCall,
    TestJump, // TestJump b, pop_t, pop_f, addr;  if TOS==b, if pop_t{pop} jmp addr, else {if pop_f pop}
}
/*
a or b
if a then a else b

eval a
jt
pop
eval b
end

a or b
if not a then a else b

eval a
jf
pop
eval b
end


*/
// #[derive(Clone, Debug)]
// pub struct VarDebugInfo {
//     name: String,
//     loc: SourceLocation,
// }
// #[derive(Clone, Debug)]
// pub struct FuncDebugInfo {
//     locals: HashMap<u8, VarDebugInfo>,
//     upvalues: HashMap<u8, VarDebugInfo>,
// }
// #[derive(Clone, Debug)]
// pub struct ModuleDebugInfo {
//     funcs: Vec<FuncDebugInfo>,
// }
// impl ModuleDebugInfo {
//     pub fn new() -> Self {
//         Self { funcs: vec![] }
//     }
// }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteCode {
    Op(OpCode),
    Op3U8(OpCode, [u8; 3]),
    FloatHi([u8; 4]),
    FloatLo([u8; 4]),
    Address([u8; 4]),
    Label([u8; 4]), // used only during compilation
}
#[derive(Clone)]
pub struct ByteCodeModule {
    pub(crate) prototypes: Vec<ClosurePrototype>,
    pub(crate) string_pool: Vec<String>,
    pub(crate) string_pool_cache: Vec<Value>,
    pub(crate) code: Vec<ByteCode>,
    // pub(crate) debug_info: ModuleDebugInfo,
}
impl ByteCodeModule {
    pub(crate) fn pretty_print_instruction(&self, i: usize) -> String {
        let inst = self.code[i];
        match inst {
            ByteCode::Op(op) => match op {
                OpCode::Add => "add".into(),
                OpCode::Sub => "sub".into(),
                OpCode::Mul => "mul".into(),
                OpCode::Div => "div".into(),
                OpCode::Mod => "mof".into(),
                OpCode::Pow => "pow".into(),
                OpCode::Pop => "pop".into(),
                OpCode::NotEqual => "ne".into(),
                OpCode::Equal => "eq".into(),
                OpCode::Return => "ret".into(),
                OpCode::RotBCA => "rot-bca".into(),
                OpCode::Dup => "dup".into(),
                OpCode::Jump => "jump".into(),
                OpCode::StoreTable => "store table".into(),
                OpCode::LoadTable => "load table".into(),
                _ => format!("{:?}", inst),
            },
            ByteCode::Op3U8(op, operands) => match op {
                OpCode::LoadStr => format!(
                    "load \"{}\"",
                    self.string_pool[u32_from_3xu8(operands) as usize].escape_debug()
                ),
                OpCode::LoadTableStringKey => format!(
                    "load table[\"{}\"]",
                    self.string_pool[u32_from_3xu8(operands) as usize].escape_debug()
                ),
                OpCode::StoreTableStringKey => format!(
                    "store table[\"{}\"]",
                    self.string_pool[u32_from_3xu8(operands) as usize].escape_debug()
                ),

                OpCode::Call => format!("call n_args={}, n_rets={}", operands[0], operands[1]),
                OpCode::NewClosure => format!("new closure, proto={}", u32_from_3xu8(operands)),
                OpCode::StoreLocal => format!("store local[{}]", u32_from_3xu8(operands)),
                OpCode::LoadLocal => format!("load local[{}]", u32_from_3xu8(operands)),
                OpCode::StoreUpvalue => format!("store upval[{}]", u32_from_3xu8(operands)),
                OpCode::LoadUpvalue => format!("load upval[{}]", u32_from_3xu8(operands)),
                OpCode::CloseUpvalue => format!("close upval[{}]", u32_from_3xu8(operands)),
                OpCode::TestJump => {
                    format!("test jump {} {} {}", operands[0], operands[1], operands[2])
                }
                _ => format!("{:?}", inst),
            },
            _ => panic!(),
        }
    }
}
impl Debug for ByteCodeModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ByteCodeModule")
            .field("prototypes", &self.prototypes)
            .field("string_pool", &self.string_pool)
            .field("code", &self.code)
            .finish()
    }
}
mod test {
    #[test]
    fn check_size() {
        use crate::bytecode::ByteCode;
        use std::mem::size_of;
        println!("{}", size_of::<ByteCode>());
        assert!(size_of::<ByteCode>() == 5);
    }
}
pub(crate) fn get_3xu8(i: u32) -> [u8; 3] {
    let bytes = i.to_le_bytes();
    assert!(bytes[3] == 0);
    [bytes[0], bytes[1], bytes[2]]
}
pub(crate) fn u32_from_3xu8(bytes: [u8; 3]) -> u32 {
    u32::from_le_bytes([bytes[0], bytes[1], bytes[2], 0])
}
