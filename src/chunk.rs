//! Binary chunk serialization for `string.dump` and binary `load`.
//!
//! The format is custom to Rua (not compatible with PUC-Rio's `luac`
//! output). It is versioned and validated on load:
//!
//! ```text
//! header:
//!   [0..4)   signature "\x1bRua"
//!   [4]      format version (u8)
//!   [5]      endianness (1 = little, 0 = big)
//!   [6]      sizeof(int)
//!   [7]      sizeof(size_t)
//!   [8]      sizeof(lua_Integer)
//!   [9]      sizeof(lua_Number)
//!   [10..18) integer check value (i64 LE, 0x5678)
//!   [18..26) number check value (f64 bits LE, 370.5)
//! proto: (recursive)
//!   code:        u32 count, then u32 instructions
//!   constants:   u32 count, then tagged values
//!   protos:      u32 count, then nested protos
//!   upvalues:    u32 count, then descriptors
//!   line_info:   u32 count, then u32 lines (0 when stripped)
//!   locals:      u32 count, then name + start/end pc (0 when stripped)
//!   source:      optional string (absent when stripped)
//!   num_params:  u8
//!   is_vararg:   u8
//!   max_stack:   u8
//!   vararg_reg:  u8 (0 = none, 1 = present) + u8
//! ```

use crate::bytecode::{Constant, LocalVarInfo, Proto, UpvalueDesc};

/// Chunk signature. The first byte (0x1b) marks the chunk as binary.
pub const SIGNATURE: [u8; 4] = [0x1b, b'L', b'u', b'a'];

/// Header format version (matches reference Lua 5.5's 0x55).
pub const VERSION: u8 = 0x55;

const FORMAT: u8 = 0;
const LUAC_DATA: [u8; 6] = [0x19, 0x93, b'\r', b'\n', 0x1a, b'\n'];
const INT_CHECK: i32 = -0x5678;
const INST_CHECK: u32 = 0x1234_5678;
const INTEGER_CHECK: i64 = -0x5678;
const NUM_CHECK: f64 = -370.5;

// ── Writing ────────────────────────────────────────────────────────

struct Writer {
    buf: Vec<u8>,
}

impl Writer {
    fn new() -> Self {
        Writer { buf: Vec::new() }
    }

    fn u8(&mut self, v: u8) {
        self.buf.push(v);
    }

    fn u32(&mut self, v: u32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    fn u64(&mut self, v: u64) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    /// Length-prefixed byte string.
    fn bytes(&mut self, b: &[u8]) {
        self.u32(b.len() as u32);
        self.buf.extend_from_slice(b);
    }

    fn opt_string(&mut self, s: Option<&str>) {
        match s {
            Some(s) => {
                self.u8(1);
                self.bytes(s.as_bytes());
            }
            None => self.u8(0),
        }
    }

    /// Write the proto's source, inheriting the parent's when identical
    /// (keeps dumps compact, matching reference Lua's string reuse).
    fn source(&mut self, source: Option<&str>, parent: Option<&str>, strip: bool) {
        if strip {
            self.u8(0);
            return;
        }
        match source {
            Some(s) if Some(s) == parent => self.u8(1),
            Some(s) => {
                self.u8(2);
                self.bytes(s.as_bytes());
            }
            None => self.u8(0),
        }
    }

    fn proto(&mut self, p: &Proto, strip: bool, parent_source: Option<&str>) {
        self.u32(p.code.len() as u32);
        for &inst in &p.code {
            self.u32(inst);
        }

        self.u32(p.stack_top_at.len() as u32);
        self.buf.extend_from_slice(&p.stack_top_at);

        self.u32(p.constants.len() as u32);
        for k in &p.constants {
            match k {
                Constant::Nil => self.u8(0),
                Constant::Boolean(false) => self.u8(1),
                Constant::Boolean(true) => self.u8(2),
                Constant::Integer(n) => {
                    self.u8(3);
                    self.u64(*n as u64);
                }
                Constant::Float(f) => {
                    self.u8(4);
                    self.u64(f.to_bits());
                }
                Constant::String(s) => {
                    self.u8(5);
                    self.bytes(s);
                }
            }
        }

        self.source(p.source.as_deref(), parent_source, strip);

        self.u32(p.protos.len() as u32);
        for sub in &p.protos {
            self.proto(sub, strip, p.source.as_deref());
        }

        self.u32(p.upvalues.len() as u32);
        for uv in &p.upvalues {
            if strip {
                self.u8(0);
            } else {
                self.u8(1);
                self.bytes(uv.name.as_deref().unwrap_or("").as_bytes());
            }
            self.u8(uv.in_stack as u8);
            self.u8(uv.index);
            self.u8(uv.is_const as u8);
        }

        if strip {
            self.u32(0); // line_info
            self.u32(0); // locals
        } else {
            self.u32(p.line_info.len() as u32);
            for &line in &p.line_info {
                self.u32(line);
            }

            self.u32(p.locals.len() as u32);
            for l in &p.locals {
                self.bytes(l.name.as_bytes());
                self.u8(l.reg);
                self.u32(l.start_pc);
                self.u32(l.end_pc);
            }
        }

        self.u8(p.num_params);
        self.u8(p.is_vararg as u8);
        self.u8(p.max_stack_size);
        self.u32(p.line_defined);
        self.u32(p.last_line_defined);
        match p.vararg_name_reg {
            Some(r) => {
                self.u8(1);
                self.u8(r);
            }
            None => self.u8(0),
        }
    }
}

/// Serialize a prototype into a binary chunk. When `strip` is true,
/// debug information (line info, locals, source, upvalue names) is
/// omitted.
pub fn dump(proto: &Proto, strip: bool) -> Vec<u8> {
    let mut w = Writer::new();
    // Header compatible with the reference Lua 5.5 layout (the payload
    // after the header is Rua-specific).
    w.buf.extend_from_slice(&SIGNATURE);
    w.u8(VERSION);
    w.u8(FORMAT);
    w.buf.extend_from_slice(&LUAC_DATA);
    w.u8(std::mem::size_of::<i32>() as u8);
    w.buf.extend_from_slice(&INT_CHECK.to_le_bytes());
    w.u8(std::mem::size_of::<u32>() as u8);
    w.buf.extend_from_slice(&INST_CHECK.to_le_bytes());
    w.u8(std::mem::size_of::<i64>() as u8);
    w.buf.extend_from_slice(&INTEGER_CHECK.to_le_bytes());
    w.u8(std::mem::size_of::<f64>() as u8);
    w.buf.extend_from_slice(&NUM_CHECK.to_le_bytes());
    w.proto(proto, strip, None);
    w.buf
}

// ── Reading ────────────────────────────────────────────────────────

struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

type ChunkResult<T> = Result<T, String>;

impl<'a> Reader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Reader { data, pos: 0 }
    }

    fn take(&mut self, n: usize) -> ChunkResult<&'a [u8]> {
        if self.pos.checked_add(n).map_or(true, |end| end > self.data.len()) {
            return Err("truncated chunk".to_string());
        }
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    fn u8(&mut self) -> ChunkResult<u8> {
        Ok(self.take(1)?[0])
    }

    fn u32(&mut self) -> ChunkResult<u32> {
        let b = self.take(4)?;
        Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }

    fn u64(&mut self) -> ChunkResult<u64> {
        let b = self.take(8)?;
        Ok(u64::from_le_bytes([
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
        ]))
    }

    fn bytes(&mut self) -> ChunkResult<Vec<u8>> {
        let len = self.u32()? as usize;
        Ok(self.take(len)?.to_vec())
    }

    fn opt_string(&mut self) -> ChunkResult<Option<String>> {
        match self.u8()? {
            0 => Ok(None),
            _ => {
                let b = self.bytes()?;
                Ok(Some(String::from_utf8_lossy(&b).to_string()))
            }
        }
    }

    fn source(&mut self, inherited: Option<&str>) -> ChunkResult<Option<String>> {
        match self.u8()? {
            0 => Ok(None),
            1 => Ok(inherited.map(|s| s.to_string())),
            _ => {
                let b = self.bytes()?;
                Ok(Some(String::from_utf8_lossy(&b).to_string()))
            }
        }
    }

    /// Read a length that must not be absurdly large, to avoid
    /// pre-allocating giant vectors on corrupted input.
    fn count(&mut self, elem: usize) -> ChunkResult<usize> {
        let n = self.u32()? as usize;
        if elem > 0 && n > self.data.len() / elem + 1 {
            return Err("truncated chunk".to_string());
        }
        Ok(n)
    }

    fn proto(&mut self, inherited_source: Option<&str>) -> ChunkResult<Proto> {
        let mut proto = Proto::new(None);

        let ncode = self.count(4)?;
        proto.code.reserve(ncode);
        for _ in 0..ncode {
            proto.code.push(self.u32()?);
        }

        let ntop = self.count(1)?;
        proto.stack_top_at.extend_from_slice(self.take(ntop)?);

        let nconst = self.count(1)?;
        proto.constants.reserve(nconst);
        for _ in 0..nconst {
            match self.u8()? {
                0 => proto.constants.push(Constant::Nil),
                1 => proto.constants.push(Constant::Boolean(false)),
                2 => proto.constants.push(Constant::Boolean(true)),
                3 => proto.constants.push(Constant::Integer(self.u64()? as i64)),
                4 => proto.constants.push(Constant::Float(f64::from_bits(self.u64()?))),
                5 => proto.constants.push(Constant::String(self.bytes()?)),
                _ => return Err("invalid constant type".to_string()),
            }
        }

        proto.source = self.source(inherited_source)?;

        let nprotos = self.count(1)?;
        proto.protos.reserve(nprotos);
        for _ in 0..nprotos {
            proto.protos.push(self.proto(proto.source.as_deref())?);
        }

        let nupvals = self.count(1)?;
        proto.upvalues.reserve(nupvals);
        for _ in 0..nupvals {
            let name = match self.u8()? {
                0 => None,
                _ => Some(String::from_utf8_lossy(&self.bytes()?).to_string()),
            };
            let in_stack = self.u8()? != 0;
            let index = self.u8()?;
            let is_const = self.u8()? != 0;
            proto.upvalues.push(UpvalueDesc {
                name,
                in_stack,
                index,
                is_const,
            });
        }

        let nlines = self.count(4)?;
        proto.line_info.reserve(nlines);
        for _ in 0..nlines {
            proto.line_info.push(self.u32()?);
        }

        let nlocals = self.count(1)?;
        proto.locals.reserve(nlocals);
        for _ in 0..nlocals {
            let name = String::from_utf8_lossy(&self.bytes()?).to_string();
            let reg = self.u8()?;
            let start_pc = self.u32()?;
            let end_pc = self.u32()?;
            proto.locals.push(LocalVarInfo {
                name,
                reg,
                start_pc,
                end_pc,
            });
        }

        proto.num_params = self.u8()?;
        proto.is_vararg = self.u8()? != 0;
        proto.max_stack_size = self.u8()?;
        proto.line_defined = self.u32()?;
        proto.last_line_defined = self.u32()?;
        proto.vararg_name_reg = match self.u8()? {
            0 => None,
            _ => Some(self.u8()?),
        };

        Ok(proto)
    }
}

/// Parse a binary chunk back into a prototype.
pub fn undump(data: &[u8], _chunkname: &str) -> ChunkResult<Proto> {
    let mut r = Reader::new(data);

    let sig = r.take(SIGNATURE.len())?;
    if sig != SIGNATURE {
        return Err("not a binary chunk".to_string());
    }
    if r.u8()? != VERSION {
        return Err("version mismatch".to_string());
    }
    if r.u8()? != FORMAT {
        return Err("format mismatch".to_string());
    }
    if r.take(LUAC_DATA.len())? != LUAC_DATA {
        return Err("corrupted chunk".to_string());
    }
    if r.u8()? != std::mem::size_of::<i32>() as u8 {
        return Err("int size mismatch".to_string());
    }
    let b = r.take(4)?;
    if i32::from_le_bytes([b[0], b[1], b[2], b[3]]) != INT_CHECK {
        return Err("int format mismatch".to_string());
    }
    if r.u8()? != std::mem::size_of::<u32>() as u8 {
        return Err("instruction size mismatch".to_string());
    }
    let b = r.take(4)?;
    if u32::from_le_bytes([b[0], b[1], b[2], b[3]]) != INST_CHECK {
        return Err("instruction format mismatch".to_string());
    }
    if r.u8()? != std::mem::size_of::<i64>() as u8 {
        return Err("Lua integer size mismatch".to_string());
    }
    let b = r.take(8)?;
    if i64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]) != INTEGER_CHECK
    {
        return Err("Lua integer format mismatch".to_string());
    }
    if r.u8()? != std::mem::size_of::<f64>() as u8 {
        return Err("Lua number size mismatch".to_string());
    }
    let b = r.take(8)?;
    if f64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]) != NUM_CHECK {
        return Err("Lua number format mismatch".to_string());
    }

    let proto = r.proto(None)?;
    Ok(proto)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{encode_abc, OpCode};

    fn sample_proto() -> Proto {
        let mut p = Proto::new(Some("@test.lua".to_string()));
        p.code.push(encode_abc(OpCode::LoadI, 0, 0, 0));
        p.code.push(encode_abc(OpCode::Return, 0, 2, 0));
        p.line_info = vec![1, 2];
        p.constants.push(Constant::Integer(42));
        p.constants.push(Constant::String(b"hello".to_vec()));
        p.constants.push(Constant::Float(3.5));
        p.constants.push(Constant::Boolean(true));
        p.upvalues.push(UpvalueDesc {
            name: Some("_ENV".to_string()),
            in_stack: true,
            index: 0,
            is_const: false,
        });
        p.locals.push(LocalVarInfo {
            name: "x".to_string(),
            reg: 0,
            start_pc: 0,
            end_pc: 2,
        });
        p.num_params = 1;
        p.is_vararg = true;
        p.max_stack_size = 4;
        p.vararg_name_reg = Some(1);

        let mut child = Proto::new(Some("@test.lua".to_string()));
        child.code.push(encode_abc(OpCode::Return, 0, 1, 0));
        child.line_info = vec![5];
        p.protos.push(child);
        p
    }

    #[test]
    fn test_dump_roundtrip() {
        let proto = sample_proto();
        let bytes = dump(&proto, false);
        assert_eq!(&bytes[..4], &SIGNATURE);
        let back = undump(&bytes, "=(load)").unwrap();
        assert_eq!(back.code, proto.code);
        assert_eq!(back.constants, proto.constants);
        assert_eq!(back.line_info, proto.line_info);
        assert_eq!(back.locals.len(), 1);
        assert_eq!(back.locals[0].name, "x");
        assert_eq!(back.source.as_deref(), Some("@test.lua"));
        assert_eq!(back.num_params, 1);
        assert!(back.is_vararg);
        assert_eq!(back.max_stack_size, 4);
        assert_eq!(back.vararg_name_reg, Some(1));
        assert_eq!(back.upvalues.len(), 1);
        assert_eq!(back.upvalues[0].name.as_deref(), Some("_ENV"));
        assert_eq!(back.protos.len(), 1);
        assert_eq!(back.protos[0].line_info, vec![5]);
    }

    #[test]
    fn test_dump_strip() {
        let proto = sample_proto();
        let bytes = dump(&proto, true);
        let back = undump(&bytes, "=(load)").unwrap();
        assert_eq!(back.code, proto.code);
        assert!(back.line_info.is_empty());
        assert!(back.locals.is_empty());
        assert!(back.source.is_none());
        assert!(back.upvalues[0].name.is_none());
        assert!(back.protos[0].line_info.is_empty());
    }

    #[test]
    fn test_undump_bad_header() {
        assert!(undump(b"not a chunk", "x").is_err());
        let mut bytes = dump(&sample_proto(), false);
        bytes[4] = 99; // version
        assert!(undump(&bytes, "x").is_err());
        let mut bytes = dump(&sample_proto(), false);
        bytes.truncate(20);
        assert!(undump(&bytes, "x").is_err());
    }
}
