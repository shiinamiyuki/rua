
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Nop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BranchIfFalse,
    Jump,
    Call,
    Return,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum ByteCode {
    Arithmetic{
        opcode:OpCode,
        a:u8,
        b:u8,
        c:u8,
    },
    FloatHi([u8;4]),
    FloatLo([u8;4]),
    JumpAddress(u32),// absolute
    BranchAddress(u32), //relative
}