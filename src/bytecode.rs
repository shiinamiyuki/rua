#[repr(u8)]
#[derive(Clone, Copy, Debug)]
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
    Mod,
    Pow,
    And,
    Or,

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
    
    LoadUpvalue, // TOS=upvalue, push upvalue[i]
    StoreUpvalue,// TOS~1=upvalue, TOS=value upvalue[i] = value

    Scatter, // Scatter{n, table_idx} local[scatter_table[table_idx]..] = stack[TOP-n-1..TOP-n]
    Gather,

    // with extended bytes
    LoadNumber,
    BranchIfFalse,
    Jump,
    Call, // Call{n_args:u8,}, TOS = func, TOS~[1..=n_args] = args
    TailCall,
    Return,
}

#[derive(Clone, Copy, Debug)]
pub enum ByteCode {
    Op(OpCode),
    Op3U8(OpCode, [u8; 3]),
    FloatHi([u8; 4]),
    FloatLo([u8; 4]),
    JumpAddress([u8; 4]),   // absolute
    BranchAddress([u8; 4]), //relative
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
