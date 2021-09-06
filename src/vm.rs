use crate::value::Value;

pub const MAX_LOCALS:usize = 200;
pub const REGISTERS_PER_FRAME:usize = 256;
struct Frame {
    registers:[Value; REGISTERS_PER_FRAME],
}

pub struct VM{

}