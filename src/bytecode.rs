#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpCode {
    Nop,
    
    // No operands
    LoadNil,
    LoadTrue,
    LoadFalse,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
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

    LoadGlobal,// TOS = name
    StoreGlobal,// TOS~1=name TOS=value global[name]= value

    // 3xu8 operand 
    /*union _3xu8{
        u8 s[3];
        u24 i;
    };

    }*/
    LoadStr,
    LoadLocal, // PUSH  local[i]
    StoreLocal, // local[i] = TOS;POP
    Unpack, // Unpack TOS to n values, 
    
    LoadTable,// TOS~1 = table, TOS = key
    StoreTable, // TOS~2 = table, key, value
    LoadUpvalue, // TOS=upvalue, push upvalue[i]
    StoreUpvalue,// TOS~1=upvalue, TOS=value upvalue[i] = value

    Scatter, // Scatter{n, table_idx} local[scatter_table[table_idx]..] = stack[TOP-n-1..TOP-n]
    Gather,

    // with extended bytes
    LoadNumber,
    Jump,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteCode {
    Op(OpCode),
    Op3U8(OpCode, [u8; 3]),
    FloatHi([u8; 4]),
    FloatLo([u8; 4]),
    Address([u8; 4]),
}
#[derive(Clone, Debug)]
pub struct ByteCodeModule {
    pub(crate) string_pool:Vec<String>,
    pub(crate) code: Vec<ByteCode>,
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
pub(crate) fn u32_from_3xu8(bytes:[u8; 3]) -> u32 {
    u32::from_le_bytes([bytes[0], bytes[1], bytes[2], 0])
}