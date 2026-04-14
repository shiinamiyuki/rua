//! Bytecode instruction encoding/decoding and function prototypes.
//!
//! See `design_notes/bytecode.md` for the full design reference.

use std::fmt;

use crate::value::Value;

// ── Opcodes ────────────────────────────────────────────────────────

/// All 50 VM opcodes. See `bytecode.md` §4 for the complete reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    // Loading
    Move = 0,       // ABC:  R[A] := R[B]
    LoadI = 1,      // AsBx: R[A] := sBx (signed integer)
    LoadK = 2,      // ABx:  R[A] := K[Bx]
    LoadKX = 3,     // ABx:  R[A] := K[EXTRAARG] (next instruction)
    LoadBool = 4,   // ABC:  R[A] := (bool)B; if C then PC++
    LoadNil = 5,    // ABC:  R[A..A+B] := nil

    // Upvalues
    GetUpval = 6,   // ABC:  R[A] := UpValue[B]
    SetUpval = 7,   // ABC:  UpValue[B] := R[A]
    GetTabUp = 8,   // ABC:  R[A] := UpValue[B][K[C]]
    SetTabUp = 9,   // ABC:  UpValue[A][K[B]] := R[C]

    // Tables
    NewTable = 10,  // ABC:  R[A] := {} (B=arr hint, C=hash hint)
    GetTable = 11,  // ABC:  R[A] := R[B][R[C]]
    SetTable = 12,  // ABC:  R[A][R[B]] := R[C]
    SetList = 13,   // ABC:  R[A][(C-1)*FPF+i] := R[A+i], 1 ≤ i ≤ B

    // Arithmetic
    Add = 14,       // ABC:  R[A] := R[B] + R[C]
    Sub = 15,       // ABC:  R[A] := R[B] - R[C]
    Mul = 16,       // ABC:  R[A] := R[B] * R[C]
    Div = 17,       // ABC:  R[A] := R[B] / R[C] (float div)
    IDiv = 18,      // ABC:  R[A] := R[B] // R[C] (floor div)
    Mod = 19,       // ABC:  R[A] := R[B] % R[C]
    Pow = 20,       // ABC:  R[A] := R[B] ^ R[C]
    Unm = 21,       // ABC:  R[A] := -R[B]

    // Bitwise
    BAnd = 22,      // ABC:  R[A] := R[B] & R[C]
    BOr = 23,       // ABC:  R[A] := R[B] | R[C]
    BXor = 24,      // ABC:  R[A] := R[B] ~ R[C]
    Shl = 25,       // ABC:  R[A] := R[B] << R[C]
    Shr = 26,       // ABC:  R[A] := R[B] >> R[C]
    BNot = 27,      // ABC:  R[A] := ~R[B]

    // Logic
    Not = 28,       // ABC:  R[A] := not R[B]

    // String & Length
    Concat = 29,    // ABC:  R[A] := R[B]..R[B+1]..R[C]
    Len = 30,       // ABC:  R[A] := #R[B]

    // Comparison & Conditional
    Eq = 31,        // ABC:  if (R[B] == R[C]) ~= A then PC++
    Lt = 32,        // ABC:  if (R[B] <  R[C]) ~= A then PC++
    Le = 33,        // ABC:  if (R[B] <= R[C]) ~= A then PC++
    Test = 34,      // ABC:  if (not R[A]) == C then PC++
    TestSet = 35,   // ABC:  if (not R[B]) == C then PC++ else R[A]:=R[B]

    // Control flow
    Jmp = 36,       // AsBx: PC += sBx
    ForPrep = 37,   // AsBx: init numeric for, jump to FORLOOP
    ForLoop = 38,   // AsBx: step numeric for, branch back
    TForPrep = 39,  // AsBx: init generic for, jump to TFORLOOP
    TForLoop = 40,  // ABC:  call iter + test nil + branch

    // Functions
    Closure = 41,   // ABx:  R[A] := closure(Proto[Bx])
    Call = 42,      // ABC:  R[A..A+C-2] := R[A](R[A+1]..R[A+B-1])
    TailCall = 43,  // ABC:  return R[A](R[A+1]..R[A+B-1])
    Return = 44,    // ABC:  return R[A..A+B-2]
    VarArg = 45,    // ABC:  R[A..A+C-2] := vararg
    VarArgPrep = 46, // ABC: adjust varargs (A = fixed params)

    // Scope & cleanup
    Close = 47,     // ABC:  close upvalues >= R[A], close TBC vars
    Tbc = 48,       // ABC:  mark R[A] as to-be-closed
    ExtraArg = 49,  // special: 24-bit extra argument (A:Bx combined)
}

impl OpCode {
    /// Convert from u8 to OpCode.
    pub fn from_u8(v: u8) -> Option<OpCode> {
        if v <= 49 {
            // SAFETY: all values 0..=49 are valid OpCode variants with repr(u8).
            Some(unsafe { std::mem::transmute(v) })
        } else {
            None
        }
    }

    /// Name of the opcode (for disassembly).
    pub fn name(self) -> &'static str {
        match self {
            OpCode::Move => "MOVE",
            OpCode::LoadI => "LOADI",
            OpCode::LoadK => "LOADK",
            OpCode::LoadKX => "LOADKX",
            OpCode::LoadBool => "LOADBOOL",
            OpCode::LoadNil => "LOADNIL",
            OpCode::GetUpval => "GETUPVAL",
            OpCode::SetUpval => "SETUPVAL",
            OpCode::GetTabUp => "GETTABUP",
            OpCode::SetTabUp => "SETTABUP",
            OpCode::NewTable => "NEWTABLE",
            OpCode::GetTable => "GETTABLE",
            OpCode::SetTable => "SETTABLE",
            OpCode::SetList => "SETLIST",
            OpCode::Add => "ADD",
            OpCode::Sub => "SUB",
            OpCode::Mul => "MUL",
            OpCode::Div => "DIV",
            OpCode::IDiv => "IDIV",
            OpCode::Mod => "MOD",
            OpCode::Pow => "POW",
            OpCode::Unm => "UNM",
            OpCode::BAnd => "BAND",
            OpCode::BOr => "BOR",
            OpCode::BXor => "BXOR",
            OpCode::Shl => "SHL",
            OpCode::Shr => "SHR",
            OpCode::BNot => "BNOT",
            OpCode::Not => "NOT",
            OpCode::Concat => "CONCAT",
            OpCode::Len => "LEN",
            OpCode::Eq => "EQ",
            OpCode::Lt => "LT",
            OpCode::Le => "LE",
            OpCode::Test => "TEST",
            OpCode::TestSet => "TESTSET",
            OpCode::Jmp => "JMP",
            OpCode::ForPrep => "FORPREP",
            OpCode::ForLoop => "FORLOOP",
            OpCode::TForPrep => "TFORPREP",
            OpCode::TForLoop => "TFORLOOP",
            OpCode::Closure => "CLOSURE",
            OpCode::Call => "CALL",
            OpCode::TailCall => "TAILCALL",
            OpCode::Return => "RETURN",
            OpCode::VarArg => "VARARG",
            OpCode::VarArgPrep => "VARARGPREP",
            OpCode::Close => "CLOSE",
            OpCode::Tbc => "TBC",
            OpCode::ExtraArg => "EXTRAARG",
        }
    }
}

// ── Instruction encoding ───────────────────────────────────────────

/// Fields per flush for SETLIST (matches PUC-Rio).
pub const FIELDS_PER_FLUSH: u32 = 50;

/// Encode an ABC-format instruction.
#[inline]
pub fn encode_abc(op: OpCode, a: u8, b: u8, c: u8) -> u32 {
    (op as u32) | ((a as u32) << 8) | ((b as u32) << 16) | ((c as u32) << 24)
}

/// Encode an ABx-format instruction.
#[inline]
pub fn encode_abx(op: OpCode, a: u8, bx: u16) -> u32 {
    (op as u32) | ((a as u32) << 8) | ((bx as u32) << 16)
}

/// Encode an AsBx-format instruction (signed Bx, biased by 32768).
#[inline]
pub fn encode_asbx(op: OpCode, a: u8, sbx: i16) -> u32 {
    let bx = (sbx as i32 + 32768) as u16;
    encode_abx(op, a, bx)
}

/// Decode the opcode field.
#[inline]
pub fn decode_op(inst: u32) -> u8 {
    (inst & 0xFF) as u8
}

/// Decode the A field.
#[inline]
pub fn decode_a(inst: u32) -> u8 {
    ((inst >> 8) & 0xFF) as u8
}

/// Decode the B field.
#[inline]
pub fn decode_b(inst: u32) -> u8 {
    ((inst >> 16) & 0xFF) as u8
}

/// Decode the C field.
#[inline]
pub fn decode_c(inst: u32) -> u8 {
    ((inst >> 24) & 0xFF) as u8
}

/// Decode the Bx field (unsigned 16-bit).
#[inline]
pub fn decode_bx(inst: u32) -> u16 {
    ((inst >> 16) & 0xFFFF) as u16
}

/// Decode the sBx field (signed 16-bit, biased).
#[inline]
pub fn decode_sbx(inst: u32) -> i16 {
    (decode_bx(inst) as i32 - 32768) as i16
}

/// Decode the EXTRAARG 24-bit payload (A:Bx combined).
#[inline]
pub fn decode_ax(inst: u32) -> u32 {
    inst >> 8
}

/// Encode an EXTRAARG instruction with 24-bit payload.
#[inline]
pub fn encode_extraarg(ax: u32) -> u32 {
    (OpCode::ExtraArg as u32) | (ax << 8)
}

// ── Function prototype ─────────────────────────────────────────────

/// A compiled function prototype.
#[derive(Debug, Clone)]
pub struct Proto {
    /// Bytecode instructions.
    pub code: Vec<u32>,
    /// Constant pool (numbers, strings, nil, booleans).
    pub constants: Vec<Constant>,
    /// Nested function prototypes (referenced by CLOSURE).
    pub protos: Vec<Proto>,
    /// Upvalue descriptors.
    pub upvalues: Vec<UpvalueDesc>,
    /// Line number for each instruction (parallel to `code`).
    pub line_info: Vec<u32>,
    /// Local variable debug info.
    pub locals: Vec<LocalVarInfo>,
    /// Source file name.
    pub source: Option<String>,
    /// Number of fixed parameters.
    pub num_params: u8,
    /// Whether this function is variadic.
    pub is_vararg: bool,
    /// Maximum stack size (number of registers needed).
    pub max_stack_size: u8,
    /// Register for named vararg table (`... name` syntax), if any.
    pub vararg_name_reg: Option<u8>,
}

/// A constant in the constant pool.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
}

impl Constant {
    /// Convert to a runtime Value (requires a GC for string allocation).
    pub fn to_value(&self, gc: &mut crate::gc::Gc) -> Value {
        match self {
            Constant::Nil => Value::Nil,
            Constant::Boolean(b) => Value::Boolean(*b),
            Constant::Integer(n) => Value::Integer(*n),
            Constant::Float(n) => Value::Float(*n),
            Constant::String(s) => Value::Object(gc.new_string(s)),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Nil => write!(f, "nil"),
            Constant::Boolean(b) => write!(f, "{b}"),
            Constant::Integer(n) => write!(f, "{n}"),
            Constant::Float(n) => write!(f, "{n}"),
            Constant::String(s) => match std::str::from_utf8(s) {
                Ok(s) => write!(f, "{s:?}"),
                Err(_) => write!(f, "<bytes({})>", s.len()),
            },
        }
    }
}

/// Describes how an upvalue is captured.
#[derive(Debug, Clone)]
pub struct UpvalueDesc {
    /// Debug name (if available).
    pub name: Option<String>,
    /// true = upvalue refers to a register in the immediately enclosing function.
    /// false = upvalue refers to an upvalue in the enclosing function's upvalue list.
    pub in_stack: bool,
    /// If in_stack: register index in parent. If not: upvalue index in parent.
    pub index: u8,
}

/// Debug info for a local variable.
#[derive(Debug, Clone)]
pub struct LocalVarInfo {
    /// Variable name.
    pub name: String,
    /// First instruction where the variable is active (inclusive).
    pub start_pc: u32,
    /// Last instruction where the variable is active (inclusive).
    pub end_pc: u32,
}

impl Proto {
    /// Create a new empty prototype.
    pub fn new(source: Option<String>) -> Self {
        Proto {
            code: Vec::new(),
            constants: Vec::new(),
            protos: Vec::new(),
            upvalues: Vec::new(),
            line_info: Vec::new(),
            locals: Vec::new(),
            source,
            num_params: 0,
            is_vararg: false,
            max_stack_size: 2, // minimum usable stack
            vararg_name_reg: None,
        }
    }
}

// ── Disassembler ───────────────────────────────────────────────────

impl Proto {
    /// Disassemble this prototype to a string.
    pub fn disassemble(&self) -> String {
        let mut out = String::new();
        self.disassemble_impl(&mut out, 0);
        out
    }

    fn disassemble_impl(&self, out: &mut String, depth: usize) {
        let indent = "  ".repeat(depth);
        let kind = if self.is_vararg { "vararg" } else { "fixed" };
        let src = self.source.as_deref().unwrap_or("?");
        out.push_str(&format!(
            "{indent}function <{src}> ({} instructions, {} params, {kind})\n",
            self.code.len(),
            self.num_params,
        ));
        out.push_str(&format!(
            "{indent}{} constants, {} protos, {} upvalues, {} locals, {} stack\n",
            self.constants.len(),
            self.protos.len(),
            self.upvalues.len(),
            self.locals.len(),
            self.max_stack_size,
        ));

        for (i, &inst) in self.code.iter().enumerate() {
            let line = self.line_info.get(i).copied().unwrap_or(0);
            let op_byte = decode_op(inst);
            let a = decode_a(inst);
            let b = decode_b(inst);
            let c = decode_c(inst);
            let bx = decode_bx(inst);
            let sbx = decode_sbx(inst);

            let op_name = OpCode::from_u8(op_byte)
                .map(|op| op.name())
                .unwrap_or("???");

            let args = match OpCode::from_u8(op_byte) {
                Some(op) => match op {
                    // AsBx format
                    OpCode::LoadI => format!("{a} {sbx}"),
                    OpCode::Jmp => format!("{sbx} ; to {}", i as i32 + 1 + sbx as i32),
                    OpCode::ForPrep | OpCode::ForLoop | OpCode::TForPrep => {
                        format!("{a} {sbx} ; to {}", i as i32 + 1 + sbx as i32)
                    }

                    // ABx format
                    OpCode::LoadK => {
                        let k_str = self.constant_str(bx as usize);
                        format!("{a} {bx} ; {k_str}")
                    }
                    OpCode::LoadKX => format!("{a}"),
                    OpCode::Closure => format!("{a} {bx}"),

                    // ABC format with constant references
                    OpCode::GetTabUp => {
                        let k_str = self.constant_str(c as usize);
                        format!("{a} {b} {c} ; UpValue[{b}][{k_str}]")
                    }
                    OpCode::SetTabUp => {
                        let k_str = self.constant_str(b as usize);
                        format!("{a} {b} {c} ; UpValue[{a}][{k_str}] := R[{c}]")
                    }

                    // Standard ABC format
                    OpCode::Move => format!("{a} {b}"),
                    OpCode::LoadBool => format!("{a} {b} {c}"),
                    OpCode::LoadNil => format!("{a} {b}"),
                    OpCode::GetUpval | OpCode::SetUpval => format!("{a} {b}"),
                    OpCode::NewTable => format!("{a} {b} {c}"),
                    OpCode::GetTable | OpCode::SetTable => format!("{a} {b} {c}"),
                    OpCode::SetList => format!("{a} {b} {c}"),
                    OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div
                    | OpCode::IDiv | OpCode::Mod | OpCode::Pow
                    | OpCode::BAnd | OpCode::BOr | OpCode::BXor
                    | OpCode::Shl | OpCode::Shr => format!("{a} {b} {c}"),
                    OpCode::Unm | OpCode::BNot | OpCode::Not | OpCode::Len => {
                        format!("{a} {b}")
                    }
                    OpCode::Concat => format!("{a} {b} {c}"),
                    OpCode::Eq | OpCode::Lt | OpCode::Le => format!("{a} {b} {c}"),
                    OpCode::Test => format!("{a} {c}"),
                    OpCode::TestSet => format!("{a} {b} {c}"),
                    OpCode::TForLoop => format!("{a} {b} {c}"),
                    OpCode::Call | OpCode::TailCall => format!("{a} {b} {c}"),
                    OpCode::Return => format!("{a} {b}"),
                    OpCode::VarArg => format!("{a} {c}"),
                    OpCode::VarArgPrep => format!("{a}"),
                    OpCode::Close | OpCode::Tbc => format!("{a}"),
                    OpCode::ExtraArg => format!("{}", decode_ax(inst)),
                },
                None => format!("{a} {b} {c}"),
            };

            out.push_str(&format!(
                "{indent}  [{i:4}] {line:4}  {op_name:<12} {args}\n"
            ));
        }

        // Dump constants
        if !self.constants.is_empty() {
            out.push_str(&format!("{indent}constants:\n"));
            for (i, k) in self.constants.iter().enumerate() {
                out.push_str(&format!("{indent}  [{i}] {k}\n"));
            }
        }

        // Dump upvalues
        if !self.upvalues.is_empty() {
            out.push_str(&format!("{indent}upvalues:\n"));
            for (i, uv) in self.upvalues.iter().enumerate() {
                let name = uv.name.as_deref().unwrap_or("?");
                let loc = if uv.in_stack { "stack" } else { "upvalue" };
                out.push_str(&format!("{indent}  [{i}] {name} ({loc} {}) \n", uv.index));
            }
        }

        // Recurse into nested protos
        for (i, p) in self.protos.iter().enumerate() {
            out.push_str(&format!("{indent}proto [{i}]:\n"));
            p.disassemble_impl(out, depth + 1);
        }
    }

    fn constant_str(&self, idx: usize) -> String {
        self.constants
            .get(idx)
            .map(|k| format!("{k}"))
            .unwrap_or_else(|| format!("K[{idx}]?"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_abc() {
        let inst = encode_abc(OpCode::Add, 1, 2, 3);
        assert_eq!(decode_op(inst), OpCode::Add as u8);
        assert_eq!(decode_a(inst), 1);
        assert_eq!(decode_b(inst), 2);
        assert_eq!(decode_c(inst), 3);
    }

    #[test]
    fn test_encode_decode_abx() {
        let inst = encode_abx(OpCode::LoadK, 5, 1000);
        assert_eq!(decode_op(inst), OpCode::LoadK as u8);
        assert_eq!(decode_a(inst), 5);
        assert_eq!(decode_bx(inst), 1000);
    }

    #[test]
    fn test_encode_decode_asbx() {
        for sbx in [-32768i16, -1, 0, 1, 32767] {
            let inst = encode_asbx(OpCode::Jmp, 0, sbx);
            assert_eq!(decode_op(inst), OpCode::Jmp as u8);
            assert_eq!(decode_sbx(inst), sbx);
        }
    }

    #[test]
    fn test_encode_decode_extraarg() {
        let inst = encode_extraarg(0x123456);
        assert_eq!(decode_op(inst), OpCode::ExtraArg as u8);
        assert_eq!(decode_ax(inst), 0x123456);
    }

    #[test]
    fn test_opcode_from_u8() {
        assert_eq!(OpCode::from_u8(0), Some(OpCode::Move));
        assert_eq!(OpCode::from_u8(49), Some(OpCode::ExtraArg));
        assert_eq!(OpCode::from_u8(50), None);
        assert_eq!(OpCode::from_u8(255), None);
    }
}
