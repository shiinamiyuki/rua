# Rua Bytecode Design

> Detailed bytecode design for the Rua Lua 5.5 interpreter.
> This document supersedes section 6 of `rua-design.md`.

## Design Philosophy

PUC-Rio Lua 5.4 has **83 opcodes** across 7 instruction formats. Many of these are specialized variants
(e.g., `ADDK`, `ADDI`, `GETI`, `SETFIELD`, `EQI`, `LTI`, ...) that exist purely for performance in C's
`switch` dispatch. We take a **simplicity-first** approach:

1. **Start with generic opcodes only** (~47 opcodes, 3 instruction formats).
2. **Defer specialization to Phase 4**, guided by actual benchmark data.
3. **Leverage Rust's `match`** — which compiles to efficient jump tables — rather than fighting C's
   computed-goto model with lots of redundant opcodes.

This gives us fewer opcodes to implement, test, and debug in the compiler and VM, while preserving a
clean path to add specialized fast-path opcodes later as a mechanical optimization.

---

## 1. Instruction Format

All instructions are **fixed 32-bit**, encoded in one of **3 formats**:

```
ABC:   [opcode:8][A:8][B:8][C:8]       — 3 operands (registers, small constants)
ABx:   [opcode:8][A:8][Bx:16]          — 1 register + 1 unsigned 16-bit operand
AsBx:  [opcode:8][A:8][sBx:16]         — 1 register + 1 signed 16-bit operand
```

### Field ranges

| Field   | Bits | Range                      | Typical use                           |
|---------|------|----------------------------|---------------------------------------|
| opcode  | 8    | 0–255                      | Instruction type                      |
| A       | 8    | 0–255                      | Destination register                  |
| B       | 8    | 0–255                      | Source register or small operand      |
| C       | 8    | 0–255                      | Source register or small operand      |
| Bx      | 16   | 0–65535                    | Constant index, proto index           |
| sBx     | 16   | −32768–32767               | Signed jump offset, small integer     |

### Why not more formats?

PUC-Rio uses 7 formats including `iAx` (24-bit payload), `isJ` (signed 25-bit jump), and variants
with a `k` flag bit. We avoid these:

- **No `AX` format:** If a constant pool exceeds 65535 entries (vanishingly rare), we use a
  two-instruction sequence: `LOADKX` followed by `EXTRAARG` (which "borrows" 24 bits from a
  separate instruction word). This avoids a special format for a case that almost never occurs.
- **No `sJ` format:** Jumps use the `AsBx` format with A=0. The 16-bit signed range (±32K
  instructions) is sufficient for any reasonable function. Functions with >32K instructions would
  need restructuring regardless.
- **No `k` flag bit:** PUC-Rio uses a bit in some formats to indicate "B or C is a constant index."
  We avoid this complexity — operands are always registers. Constants are loaded into registers
  first via `LOADK`/`LOADI`, or accessed through dedicated opcodes like `GETTABUP`/`SETTABUP`.

### Encoding helpers (Rust)

```rust
/// Encode an ABC-format instruction.
fn encode_abc(op: OpCode, a: u8, b: u8, c: u8) -> u32 {
    (op as u32) | ((a as u32) << 8) | ((b as u32) << 16) | ((c as u32) << 24)
}

/// Encode an ABx-format instruction.
fn encode_abx(op: OpCode, a: u8, bx: u16) -> u32 {
    (op as u32) | ((a as u32) << 8) | ((bx as u32) << 16)
}

/// Encode an AsBx-format instruction (signed Bx via bias).
fn encode_asbx(op: OpCode, a: u8, sbx: i16) -> u32 {
    // Store as unsigned with bias so decoding is a simple subtract
    let bx = (sbx as i32 + 32768) as u16;
    encode_abx(op, a, bx)
}

/// Decode fields from a 32-bit instruction.
fn decode_op(inst: u32) -> u8  { (inst & 0xFF) as u8 }
fn decode_a(inst: u32) -> u8   { ((inst >> 8) & 0xFF) as u8 }
fn decode_b(inst: u32) -> u8   { ((inst >> 16) & 0xFF) as u8 }
fn decode_c(inst: u32) -> u8   { ((inst >> 24) & 0xFF) as u8 }
fn decode_bx(inst: u32) -> u16 { ((inst >> 16) & 0xFFFF) as u16 }
fn decode_sbx(inst: u32) -> i16 { (decode_bx(inst) as i32 - 32768) as i16 }
```

---

## 2. Register Allocation

- Each function has a **fixed register window** (max 250 registers).
- **Locals map 1:1 to registers**, assigned sequentially from R[0].
- **Temporaries** are allocated on top of locals (bump a "free register" pointer).
- The compiler tracks `free_reg: u8` — the next available register.
- After evaluating a sub-expression, temporaries are freed (decrement `free_reg`).

```
Register layout for a function with 3 locals and a temp expression:

  R[0]  R[1]  R[2]  R[3]    R[4] ...
  |--locals---|  |--temps--|  |--free--|
   a     b     c   (a+b)
                    ^
                    free_reg after expr eval, freed after use
```

For numeric `for` loops, the compiler reserves 3 consecutive registers for the internal
loop variables (index, limit, step), plus a 4th for the visible loop variable:

```
  R[base]   R[base+1]  R[base+2]  R[base+3]
  (index)   (limit)    (step)     i  ← user-visible variable
```

---

## 3. Opcode Reference

### 3.1 Loading (5 opcodes)

| Opcode    | Format | Semantics                                                          |
|-----------|--------|--------------------------------------------------------------------|
| `LOADNIL` | ABC    | `R[A], R[A+1], ..., R[A+B] := nil`                                |
| `LOADBOOL`| ABC    | `R[A] := (bool)B; if C then PC++`                                 |
| `LOADI`   | AsBx   | `R[A] := sBx` (signed 16-bit integer, range ±32767)               |
| `LOADK`   | ABx    | `R[A] := K[Bx]` (constant from pool)                              |
| `LOADKX`  | ABx    | `R[A] := K[EXTRAARG]` (next instruction supplies 24-bit index)    |

**Notes:**
- `LOADNIL A B` nils a range of B+1 registers. Common for `local a, b, c` without initializers.
- `LOADBOOL` with `C=1` skips the next instruction — used for compiling boolean expressions
  that branch (e.g., `local x = a > b` compiles to a comparison + two `LOADBOOL` instructions).
- `LOADI` handles small integer constants inline (avoids constant pool lookup for common values
  like 0, 1, 2, loop bounds, etc.). Range: −32768 to 32767.
- `LOADK` loads from the per-function constant pool (integers, floats, strings).
- `LOADKX` is used when the constant index exceeds 16 bits. The next instruction must be
  `EXTRAARG` with a 24-bit unsigned index, giving access to up to 16M constants.

### 3.2 Move & Upvalues (5 opcodes)

| Opcode     | Format | Semantics                                                        |
|------------|--------|------------------------------------------------------------------|
| `MOVE`     | ABC    | `R[A] := R[B]`                                                  |
| `GETUPVAL` | ABC    | `R[A] := UpValue[B]`                                            |
| `SETUPVAL` | ABC    | `UpValue[B] := R[A]`                                            |
| `GETTABUP` | ABC    | `R[A] := UpValue[B][K[C]]`                                      |
| `SETTABUP` | ABC    | `UpValue[A][K[B]] := R[C]`                                      |

**Notes:**
- `GETTABUP`/`SETTABUP` are the primary mechanism for **global variable access**. The chunk's
  `_ENV` table is always upvalue[0]. So `x` (a global) compiles to `GETTABUP R[dest] 0 K["x"]`.
  These take a constant index for the key (C or B field) because global names are always string
  constants — this is the one place we embed a constant reference directly in operands, since
  global access is extremely frequent and loading the key into a register first would double the
  instruction count for every global read/write.
- `MOVE` copies between registers. Used for argument setup, variable aliasing, etc.

### 3.3 Table Operations (4 opcodes)

| Opcode     | Format | Semantics                                                        |
|------------|--------|------------------------------------------------------------------|
| `NEWTABLE` | ABC    | `R[A] := {}` (B=array size hint, C=hash size hint)              |
| `GETTABLE` | ABC    | `R[A] := R[B][R[C]]`                                            |
| `SETTABLE` | ABC    | `R[A][R[B]] := R[C]`                                            |
| `SETLIST`  | ABC    | `R[A][(C-1)*FPF+i] := R[A+i], 1 ≤ i ≤ B`                       |

**Notes:**
- `GETTABLE`/`SETTABLE` are fully generic — the key is always in a register. For constant keys
  (field access like `t.name` or integer indexing like `t[1]`), the compiler emits `LOADK`/`LOADI`
  to load the key into a temp register first, then uses `GETTABLE`/`SETTABLE`.
- Phase 4 will add `GETFIELD`, `SETFIELD`, `GETI`, `SETI` as specialized fast-path variants.
- `SETLIST` handles table constructors with positional elements: `{1, 2, 3, ...}`. `FPF` (fields
  per flush) is a compile-time constant (typically 50). When B=0, the count extends to the top of
  the stack (for vararg trailing elements).

### 3.4 Arithmetic (9 opcodes)

| Opcode | Format | Semantics                         |
|--------|--------|-----------------------------------|
| `ADD`  | ABC    | `R[A] := R[B] + R[C]`            |
| `SUB`  | ABC    | `R[A] := R[B] - R[C]`            |
| `MUL`  | ABC    | `R[A] := R[B] * R[C]`            |
| `DIV`  | ABC    | `R[A] := R[B] / R[C]` (float)    |
| `IDIV` | ABC    | `R[A] := R[B] // R[C]` (floor)   |
| `MOD`  | ABC    | `R[A] := R[B] % R[C]`            |
| `POW`  | ABC    | `R[A] := R[B] ^ R[C]`            |
| `UNM`  | ABC    | `R[A] := -R[B]`                  |
| `NOT`  | ABC    | `R[A] := not R[B]`               |

**VM dispatch fast path (example for ADD):**
```rust
OpCode::Add => {
    let b = self.reg(inst_b);
    let c = self.reg(inst_c);
    let result = match (b, c) {
        // Fast path: both integers
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x.wrapping_add(y)),
        // Fast path: both floats (or int+float promotion)
        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
        (Value::Integer(x), Value::Float(y)) => Value::Float(*x as f64 + y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x + *y as f64),
        // Slow path: metamethod lookup
        _ => self.arith_metamethod("__add", b, c)?,
    };
    self.set_reg(inst_a, result);
}
```

### 3.5 Bitwise (6 opcodes)

| Opcode | Format | Semantics                         |
|--------|--------|-----------------------------------|
| `BAND` | ABC    | `R[A] := R[B] & R[C]`            |
| `BOR`  | ABC    | `R[A] := R[B] \| R[C]`           |
| `BXOR` | ABC    | `R[A] := R[B] ~ R[C]`            |
| `SHL`  | ABC    | `R[A] := R[B] << R[C]`           |
| `SHR`  | ABC    | `R[A] := R[B] >> R[C]`           |
| `BNOT` | ABC    | `R[A] := ~R[B]`                  |

Bitwise operations require integer operands (or values coercible to integer). Non-integer
operands trigger `__band`/`__bor`/etc. metamethods or raise a type error.

### 3.6 String & Length (2 opcodes)

| Opcode   | Format | Semantics                                                     |
|----------|--------|---------------------------------------------------------------|
| `CONCAT` | ABC    | `R[A] := R[B] .. R[B+1] .. ... .. R[C]`                      |
| `LEN`    | ABC    | `R[A] := #R[B]`                                              |

**Notes:**
- `CONCAT` operates on a **consecutive range** of registers `R[B]` through `R[C]`. The compiler
  arranges concatenation operands into consecutive temporaries. This avoids O(n²) intermediate
  string creation — the VM can compute the total length first, allocate once, and copy.
- This consecutive-register design is important because `..` is right-associative and commonly
  chains many values (e.g., `a .. b .. c .. d .. e`).

### 3.7 Comparison & Conditional (5 opcodes)

| Opcode    | Format | Semantics                                                      |
|-----------|--------|----------------------------------------------------------------|
| `EQ`      | ABC    | `if (R[B] == R[C]) ~= A then PC++`                            |
| `LT`      | ABC    | `if (R[B] <  R[C]) ~= A then PC++`                            |
| `LE`      | ABC    | `if (R[B] <= R[C]) ~= A then PC++`                            |
| `TEST`    | ABC    | `if (not R[A]) == C then PC++`                                 |
| `TESTSET` | ABC    | `if (not R[B]) == C then PC++ else R[A] := R[B]`              |

**Notes:**
- Comparisons are **conditional skips**: they skip the next instruction (always a `JMP`) if the
  condition does NOT match. The `A` field inverts the sense: `A=0` means "skip if NOT equal",
  `A=1` means "skip if equal". This compiles both `if a == b` and `if a ~= b` with the same
  opcode, just different `A`.
- `TEST` tests truthiness: used for `if x then` and `if not x then`.
- `TESTSET` combines a truthiness test with an assignment — used for `and`/`or` short-circuit:
  `local x = a or b` compiles to `TESTSET x, a, 1; JMP skip; MOVE x, b`.
- `>` and `>=` are compiled by swapping operands: `a > b` → `LT _ b a`.

### 3.8 Control Flow (6 opcodes)

| Opcode     | Format | Semantics                                                     |
|------------|--------|---------------------------------------------------------------|
| `JMP`      | AsBx   | `PC += sBx` (A is unused, reserved for future close)         |
| `FORPREP`  | AsBx   | Initialize numeric for loop, jump to `FORLOOP`                |
| `FORLOOP`  | AsBx   | Step numeric for loop, branch back if not done                |
| `TFORPREP` | AsBx   | Initialize generic for loop, jump to `TFORLOOP`               |
| `TFORLOOP` | ABC    | Call iterator + test nil + branch (merged TFORCALL+TFORLOOP)  |

**Numeric for loop** (`for i = init, limit, step do ... end`):

The compiler reserves 4 consecutive registers:
```
R[A]   = internal index (mutated each iteration)
R[A+1] = limit
R[A+2] = step  
R[A+3] = loop variable (visible to user as `i`)
```

Compilation:
```
  LOADI/LOADK  R[A], init
  LOADI/LOADK  R[A+1], limit
  LOADI/LOADK  R[A+2], step
  FORPREP      A, offset_to_FORLOOP    -- validate & adjust, jump to loop test
  <loop body>
  FORLOOP      A, offset_back_to_body  -- step += index; if not done, jump back
```

`FORPREP`:
1. Validates that init, limit, step are all numbers (or coercible). Raises error if not.
2. For integer loops: checks that step ≠ 0.
3. Jumps forward to the `FORLOOP` instruction (which does the first boundary check).

`FORLOOP`:
1. `R[A] += R[A+2]` (add step to index).
2. If step > 0: continue if `R[A] <= R[A+1]`.
3. If step < 0: continue if `R[A] >= R[A+1]`.
4. If continuing: `R[A+3] := R[A]` (copy to user variable), jump back to loop body.
5. Otherwise: fall through (loop ends).

**Generic for loop** (`for k, v in iter, state, init do ... end`):

The compiler reserves registers:
```
R[A]   = iterator function
R[A+1] = invariant state
R[A+2] = control variable
R[A+3] = to-be-closed variable (Lua 5.5)
R[A+4], R[A+5], ... = loop variables (visible to user)
```

Compilation:
```
  <evaluate iterator expressions into R[A], R[A+1], R[A+2]>
  TFORPREP  A, offset_to_TFORLOOP    -- jump to first iteration test
  <loop body>
  TFORLOOP  A, B, C                  -- call iter, test, branch back
```

`TFORLOOP` (merged call + test):
1. Call `R[A](R[A+1], R[A+2])` → results into `R[A+4], R[A+5], ...`
2. If `R[A+4]` (first result) is nil → loop ends, fall through.
3. Otherwise: `R[A+2] := R[A+4]` (update control), jump back to loop body.

> **Why merge TFORCALL + TFORLOOP?** PUC-Rio separates them for a technical reason related to
> stack adjustment. In Rua, we handle the call and result adjustment entirely within one opcode's
> implementation, which simplifies both the compiler and the instruction stream.

### 3.9 Function (6 opcodes)

| Opcode      | Format | Semantics                                                    |
|-------------|--------|--------------------------------------------------------------|
| `CLOSURE`   | ABx    | `R[A] := closure(Proto[Bx])`                                |
| `CALL`      | ABC    | `R[A], ..., R[A+C-2] := R[A](R[A+1], ..., R[A+B-1])`       |
| `TAILCALL`  | ABC    | `return R[A](R[A+1], ..., R[A+B-1])`                        |
| `RETURN`    | ABC    | `return R[A], ..., R[A+B-2]`                                 |
| `VARARG`    | ABC    | `R[A], R[A+1], ..., R[A+C-2] := vararg`                     |
| `VARARGPREP`| ABC    | `(A = num fixed params)` — adjust varargs at function entry  |

**CALL convention:**
- `B=1`: no arguments. `B=2`: one argument `R[A+1]`. `B=0`: arguments extend to top-of-stack
  (used when the last argument is a function call or `...` with unknown count).
- `C=1`: discard all results. `C=2`: one result into `R[A]`. `C=0`: results extend to
  top-of-stack (used when results feed into another multi-value context).

**RETURN convention:**
- `B=1`: return no values. `B=2`: return `R[A]`. `B=0`: return `R[A]` through top-of-stack.

> **Why not RETURN0/RETURN1?** PUC-Rio has separate opcodes for the 0-return and 1-return cases
> to shave off one field decode. In Rust, we handle these as fast-path branches inside the single
> `RETURN` handler — the `match` on `B` is essentially free. This keeps the opcode count down
> without measurable cost.

**TAILCALL:**
1. Move arguments `R[A+1], ..., R[A+B-1]` to the current frame's base.
2. Replace the current `CallFrame`'s closure, proto, and PC.
3. Continue execution without pushing a new frame → no stack growth.

**VARARGPREP:**
- Emitted as the first instruction of any variadic function.
- `A` = number of fixed parameters.
- Adjusts the stack so that fixed params are in their expected registers and extra arguments
  are stored in the vararg area.

### 3.10 Scope & Cleanup (3 opcodes)

| Opcode     | Format | Semantics                                                     |
|------------|--------|---------------------------------------------------------------|
| `CLOSE`    | ABC    | Close all open upvalues `≥ R[A]` + close to-be-closed vars   |
| `TBC`      | ABC    | Mark `R[A]` as to-be-closed                                  |
| `EXTRAARG` | ABx    | Extra 24-bit argument for previous instruction (Ax = A:Bx)   |

**Notes:**
- `CLOSE` is emitted before `break`, `goto`, and `return` when there are open upvalues or
  to-be-closed variables in the scope being exited.
- `TBC` implements `local <close> x = ...`. When the scope exits, `__close` is called on `x`.
- `EXTRAARG` provides extended operand space. Its 24-bit payload (A field concatenated with Bx)
  is used by the preceding instruction. Currently only `LOADKX` uses it.

---

## 4. Complete Opcode Table

47 opcodes total. Numbered 0–46.

```
 #  Name        Format   Description
──  ──────────  ──────   ──────────────────────────────────────────────
 0  MOVE        ABC      R[A] := R[B]
 1  LOADI       AsBx     R[A] := sBx
 2  LOADK       ABx      R[A] := K[Bx]
 3  LOADKX      ABx      R[A] := K[EXTRAARG]
 4  LOADBOOL    ABC      R[A] := (bool)B; if C: PC++
 5  LOADNIL     ABC      R[A..A+B] := nil
 6  GETUPVAL    ABC      R[A] := UpValue[B]
 7  SETUPVAL    ABC      UpValue[B] := R[A]
 8  GETTABUP    ABC      R[A] := UpValue[B][K[C]]
 9  SETTABUP    ABC      UpValue[A][K[B]] := R[C]
10  NEWTABLE    ABC      R[A] := {} (B=arr hint, C=hash hint)
11  GETTABLE    ABC      R[A] := R[B][R[C]]
12  SETTABLE    ABC      R[A][R[B]] := R[C]
13  SETLIST     ABC      R[A][(C-1)*FPF+i] := R[A+i], 1 ≤ i ≤ B
14  ADD         ABC      R[A] := R[B] + R[C]
15  SUB         ABC      R[A] := R[B] - R[C]
16  MUL         ABC      R[A] := R[B] * R[C]
17  DIV         ABC      R[A] := R[B] / R[C]
18  IDIV        ABC      R[A] := R[B] // R[C]
19  MOD         ABC      R[A] := R[B] % R[C]
20  POW         ABC      R[A] := R[B] ^ R[C]
21  UNM         ABC      R[A] := -R[B]
22  BAND        ABC      R[A] := R[B] & R[C]
23  BOR         ABC      R[A] := R[B] | R[C]
24  BXOR        ABC      R[A] := R[B] ~ R[C]
25  SHL         ABC      R[A] := R[B] << R[C]
26  SHR         ABC      R[A] := R[B] >> R[C]
27  BNOT        ABC      R[A] := ~R[B]
28  NOT         ABC      R[A] := not R[B]
29  CONCAT      ABC      R[A] := R[B]..R[B+1]..R[C]
30  LEN         ABC      R[A] := #R[B]
31  EQ          ABC      if (R[B] == R[C]) ~= A: PC++
32  LT          ABC      if (R[B] <  R[C]) ~= A: PC++
33  LE          ABC      if (R[B] <= R[C]) ~= A: PC++
34  TEST        ABC      if (not R[A]) == C: PC++
35  TESTSET     ABC      if (not R[B]) == C: PC++ else R[A]:=R[B]
36  JMP         AsBx     PC += sBx
37  FORPREP     AsBx     init numeric for, jump forward
38  FORLOOP     AsBx     step numeric for, branch back
39  TFORPREP    AsBx     init generic for, jump forward
40  TFORLOOP    ABC      call iter + test nil + branch
41  CLOSURE     ABx      R[A] := closure(Proto[Bx])
42  CALL        ABC      R[A..A+C-2] := R[A](R[A+1]..R[A+B-1])
43  TAILCALL    ABC      return R[A](R[A+1]..R[A+B-1])
44  RETURN      ABC      return R[A..A+B-2]
45  VARARG      ABC      R[A..A+C-2] := vararg
46  VARARGPREP  ABC      adjust varargs (A = fixed params)
47  CLOSE       ABC      close upvalues ≥ R[A], close TBC vars
48  TBC         ABC      mark R[A] as to-be-closed
49  EXTRAARG    special  24-bit extra argument (A:Bx combined)
```

**Total: 50 opcodes** (vs. PUC-Rio 5.4's 83).

---

## 5. Function Prototype (Proto)

Each compiled function is represented as a `Proto`:

```rust
/// A compiled function prototype.
pub struct Proto {
    /// Bytecode instructions.
    pub code: Vec<u32>,
    /// Constant pool (numbers, strings, nil, booleans).
    pub constants: Vec<Value>,
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
}

/// Describes how an upvalue is captured.
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
pub struct LocalVarInfo {
    /// Variable name.
    pub name: String,
    /// First instruction where the variable is active (inclusive).
    pub start_pc: u32,
    /// Last instruction where the variable is active (inclusive).
    pub end_pc: u32,
}
```

The top-level chunk is itself a `Proto` (a variadic function with no fixed params). Its upvalue[0]
is `_ENV`.

---

## 6. Compilation Examples

### 6.1 Global variable read: `print(x)`

```lua
print(x)
```

Assuming `print` is K[0] and `x` is K[1]:
```
GETTABUP  R[0]  0  0    -- R[0] := _ENV["print"]  (upvalue 0 = _ENV, K[0] = "print")
GETTABUP  R[1]  0  1    -- R[1] := _ENV["x"]      (K[1] = "x")
CALL      R[0]  2  1    -- print(R[1]), discard result
```

### 6.2 Arithmetic: `local z = x + y * 2`

```lua
local x, y = 10, 20
local z = x + y * 2
```

```
LOADI     R[0]  10       -- x = 10
LOADI     R[1]  20       -- y = 20
LOADI     R[3]  2        -- temp = 2
MUL       R[3]  R[1] R[3] -- temp = y * 2
ADD       R[2]  R[0] R[3] -- z = x + temp
```

Note: In Phase 4, with `MULI`/`MULK`, the `LOADI` + `MUL` pair would become a single `MULK R[3] R[1] K[2]`.

### 6.3 If/else: `if a > b then ... else ... end`

```lua
if a > b then
    print("yes")
else
    print("no")
end
```

```
-- a > b  is compiled as  b < a  (swap operands)
LT        0  R[b] R[a]   -- if NOT (b < a) then skip JMP
JMP       +offset_else    -- jump to else
-- then block:
GETTABUP  R[t]  0  K["print"]
LOADK     R[t+1] K["yes"]
CALL      R[t]  2  1
JMP       +offset_end     -- jump past else
-- else block:
GETTABUP  R[t]  0  K["print"]
LOADK     R[t+1] K["no"]
CALL      R[t]  2  1
-- end
```

### 6.4 Numeric for loop

```lua
for i = 1, 100 do
    print(i)
end
```

```
LOADI     R[0]  1         -- init = 1
LOADI     R[1]  100       -- limit = 100
LOADI     R[2]  1         -- step = 1
FORPREP   R[0]  +3        -- validate, jump to FORLOOP
-- loop body:
GETTABUP  R[4]  0  K["print"]
MOVE      R[5]  R[3]      -- copy loop var i
CALL      R[4]  2  1
FORLOOP   R[0]  -4        -- step, compare, branch back to body
```

### 6.5 Closure with upvalue

```lua
function make_counter()
    local n = 0
    return function()
        n = n + 1
        return n
    end
end
```

Outer function (make_counter):
```
LOADI     R[0]  0         -- n = 0
CLOSURE   R[1]  Proto[0]  -- create inner closure (captures n from R[0])
RETURN    R[1]  2         -- return the closure
```

Inner function (Proto[0]):
```
-- upvalue[0] = n (in_stack=true, index=0 in parent)
GETUPVAL  R[0]  0         -- R[0] := n
LOADI     R[1]  1         -- R[1] := 1
ADD       R[0]  R[0] R[1] -- R[0] := n + 1
SETUPVAL  R[0]  0         -- n := R[0]
GETUPVAL  R[0]  0         -- R[0] := n (reload for return)
RETURN    R[0]  2         -- return n
```

---

## 7. Phase 4 Specialization Plan

The following specialized opcodes are **deferred to Phase 4** and will only be added if benchmarks
demonstrate meaningful improvement:

### 7.1 Constant-operand arithmetic

| Opcode | Replaces          | Semantics                    |
|--------|--------------------|------------------------------|
| `ADDK` | `LOADK` + `ADD`   | `R[A] := R[B] + K[C]`       |
| `SUBK` | `LOADK` + `SUB`   | `R[A] := R[B] - K[C]`       |
| `MULK` | `LOADK` + `MUL`   | `R[A] := R[B] * K[C]`       |
| `DIVK` | `LOADK` + `DIV`   | `R[A] := R[B] / K[C]`       |
| `IDIVK`| `LOADK` + `IDIV`  | `R[A] := R[B] // K[C]`      |
| `MODK` | `LOADK` + `MOD`   | `R[A] := R[B] % K[C]`       |
| `POWK` | `LOADK` + `POW`   | `R[A] := R[B] ^ K[C]`       |

### 7.2 Immediate integer arithmetic

| Opcode | Replaces          | Semantics                    |
|--------|--------------------|------------------------------|
| `ADDI` | `LOADI` + `ADD`   | `R[A] := R[B] + sC`         |

### 7.3 Specialized table access

| Opcode    | Replaces              | Semantics                       |
|-----------|-----------------------|---------------------------------|
| `GETFIELD`| `LOADK` + `GETTABLE` | `R[A] := R[B][K[C]]` (string)  |
| `SETFIELD`| `LOADK` + `SETTABLE` | `R[A][K[B]] := R[C]` (string)  |
| `GETI`    | `LOADI` + `GETTABLE` | `R[A] := R[B][C]` (integer)    |
| `SETI`    | `LOADI` + `SETTABLE` | `R[A][B] := R[C]` (integer)    |

### 7.4 Specialized comparisons

| Opcode | Replaces           | Semantics                       |
|--------|--------------------|---------------------------------|
| `EQK`  | `LOADK` + `EQ`    | `if (R[B] == K[C]) ~= A: PC++` |
| `EQI`  | `LOADI` + `EQ`    | `if (R[B] == sC) ~= A: PC++`   |
| `LTI`  | `LOADI` + `LT`    | `if (R[B] < sC) ~= A: PC++`    |
| `LEI`  | `LOADI` + `LE`    | `if (R[B] <= sC) ~= A: PC++`   |
| `GTI`  | `LOADI` + `LT`    | `if (R[B] > sC) ~= A: PC++`    |
| `GEI`  | `LOADI` + `LE`    | `if (R[B] >= sC) ~= A: PC++`   |

### 7.5 Adding specialization later

Adding these is a **mechanical transformation** in the compiler:

1. **Peephole pass:** After generating generic bytecode, scan for `LOADK R[t] K[x]; ADD R[d] R[s] R[t]`
   where `R[t]` is a temporary used only once → replace with `ADDK R[d] R[s] K[x]`.
2. **Direct emit:** Alternatively, when the compiler sees `expr + constant`, emit `ADDK` directly
   instead of `LOADK` + `ADD`.

The VM just needs new `match` arms — the dispatch structure doesn't change.

---

## 8. Comparison with PUC-Rio Lua 5.4

| Aspect                    | PUC-Rio 5.4        | Rua (Phase 1–3)            | Rua (Phase 4)            |
|---------------------------|---------------------|-----------------------------|--------------------------|
| Instruction formats       | 7                   | 3                           | 3 (unchanged)            |
| Total opcodes             | 83                  | 50                          | ~70 (with specialization)|
| Specialized arithmetic    | 8 (`ADDK`..`ADDI`) | 0                           | 8                        |
| Specialized table access  | 4 (`GETI`..`SETFIELD`) | 0                        | 4                        |
| Specialized comparisons   | 6 (`EQI`..`GEI`)   | 0                           | 6                        |
| Return variants           | 3 (`RETURN0/1/N`)  | 1 (fast paths inside)       | 1                        |
| Generic for opcodes       | 3 (`TFORPREP/CALL/LOOP`) | 2 (merged CALL+LOOP)  | 2                        |
| Constant-in-operand (RK)  | Yes (bit flag)      | No (use registers)          | No (dedicated opcodes)   |

---

## 9. Rust Implementation Notes

### 9.1 OpCode enum

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Move = 0,
    LoadI,
    LoadK,
    LoadKX,
    LoadBool,
    LoadNil,
    GetUpval,
    SetUpval,
    GetTabUp,
    SetTabUp,
    NewTable,
    GetTable,
    SetTable,
    SetList,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    Unm,
    BAnd,
    BOr,
    BXor,
    Shl,
    Shr,
    BNot,
    Not,
    Concat,
    Len,
    Eq,
    Lt,
    Le,
    Test,
    TestSet,
    Jmp,
    ForPrep,
    ForLoop,
    TForPrep,
    TForLoop,
    Closure,
    Call,
    TailCall,
    Return,
    VarArg,
    VarArgPrep,
    Close,
    Tbc,
    ExtraArg,
}
```

### 9.2 Instruction decoding struct

```rust
/// Decoded instruction fields. Created on-the-fly from a u32.
pub struct Instruction(u32);

impl Instruction {
    pub fn opcode(self) -> OpCode { /* decode bits 0..8 */ }
    pub fn a(self) -> u8          { /* decode bits 8..16 */ }
    pub fn b(self) -> u8          { /* decode bits 16..24 */ }
    pub fn c(self) -> u8          { /* decode bits 24..32 */ }
    pub fn bx(self) -> u16        { /* decode bits 16..32 */ }
    pub fn sbx(self) -> i16       { /* decode bits 16..32, bias */ }
    
    // For EXTRAARG: 24-bit payload from A:Bx
    pub fn ax(self) -> u32        { ((self.0 >> 8) & 0xFFFFFF) }
}
```

### 9.3 Disassembler

A bytecode disassembler is essential for debugging. Format:

```
[001]  LOADI      R[0]  10           ; line 1
[002]  LOADI      R[1]  20           ; line 2
[003]  LOADI      R[3]  2            ; line 3
[004]  MUL        R[3]  R[1]  R[3]   ; line 3
[005]  ADD        R[2]  R[0]  R[3]   ; line 3
```
