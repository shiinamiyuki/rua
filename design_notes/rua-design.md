# Rua — Lua 5.5 Interpreter in Rust: Design Document (v1)

## 1. Project Goals

- Implement a **complete Lua 5.5 interpreter** in safe Rust (with minimal `unsafe` where necessary for performance).
- Support **all language features** except FFI (C API / `package.loadlib` / C modules).
- Performance target: **comparable to PUC-Rio Lua** on standard benchmarks.
- Architecture: **Lexer → Parser → Bytecode Compiler → Register-based VM + GC**.

## 2. Architecture Overview

```
Source Code (.lua)
       │
       ▼
   ┌─────────┐
   │  Lexer   │   Token stream
   └────┬─────┘
        ▼
   ┌─────────┐
   │  Parser  │   AST
   └────┬─────┘
        ▼
   ┌──────────┐
   │ Compiler  │   Bytecode (Proto / FuncProto)
   └────┬──────┘
        ▼
   ┌──────────┐
   │    VM     │   Register-based execution
   │   + GC    │   Tri-color incremental + generational GC
   └──────────┘
```

## 3. Value Representation

### 3.1 LuaValue (Tagged Union / NaN-boxing)

Use **NaN-boxing** for the primary value representation to keep values at 8 bytes and avoid heap allocation for primitives.

```
64-bit IEEE 754 double:
- If the value is a valid (non-NaN) float  → it's a float
- A specific quiet NaN pattern encodes the tag + payload:

  Bits [63..51] = 0x7FF8 | tag (3 bits for type)
  Bits [47..0]  = 48-bit payload (pointer or integer data)

Tags (3 bits, encoded in bits 50..48):
  000 = Nil
  001 = Boolean (payload bit 0 = true/false)
  010 = Integer (payload = index into side table, or inline for small ints)
  011 = GcObject pointer (tables, strings, functions, userdata, threads)
```

**Alternative (simpler, phase 1):** Use a Rust enum with `Copy` semantics for non-GC types:

```rust
#[derive(Clone, Copy)]
enum Value {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    // GC-managed objects are behind a GcRef
    Object(GcRef),
}
```

**Decision:** Start with the enum approach for correctness. Migrate to NaN-boxing as an optimization in a later phase.

### 3.2 GC-managed Object Types

```rust
enum GcObject {
    String(LuaString),
    Table(Table),
    Closure(Closure),      // Lua closure
    NativeFn(NativeFn),    // Rust-implemented function
    Coroutine(Thread),
    UserData(UserData),
}
```

### 3.3 String Interning

- All strings are interned in a global `StringPool` (hash set).
- Short strings (≤40 bytes) are always interned; long strings are interned lazily (on comparison/table-key use).
- Strings store a precomputed hash for fast table lookup.
- String equality reduces to pointer comparison for interned strings.

```rust
struct LuaString {
    hash: u64,
    len: usize,
    data: Box<[u8]>,  // or inline for short strings
}
```

## 4. Lexer

### 4.1 Design

- Hand-written lexer operating on `&[u8]` (byte slice).
- Returns a stream of `Token` values with source location info (line, column).
- Handles all Lua 5.5 lexical elements:
  - Reserved words: `and break do else elseif end false for function global goto if in local nil not or repeat return then true until while`
  - Operators and punctuation (all standard ones)
  - Short strings (single/double quoted with escape sequences)
  - Long strings (`[[...]]`, `[=[...]=]`, etc.)
  - Numerals (decimal int, hex int, decimal float, hex float)
  - Comments (short `--` and long `--[[...]]`)

### 4.2 Token

```rust
enum TokenKind {
    // Literals
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    
    // Keywords
    And, Break, Do, Else, ElseIf, End, False, For,
    Function, Global, Goto, If, In, Local, Nil, Not,
    Or, Repeat, Return, Then, True, Until, While,
    
    // Symbols
    Plus, Minus, Star, Slash, Percent, Caret, Hash,
    Ampersand, Tilde, Pipe, LtLt, GtGt, SlashSlash,
    EqEq, TildeEq, LtEq, GtEq, Lt, Gt, Eq,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    ColonColon, Semicolon, Colon, Comma, Dot, DotDot, DotDotDot,
    
    // Other
    Name(String),
    Eof,
}

struct Token {
    kind: TokenKind,
    line: u32,
    column: u32,
}
```

## 5. Parser

### 5.1 Strategy

- **Recursive descent** parser (Pratt parsing for expressions).
- Produces an **AST** that closely mirrors the Lua grammar.
- Single-pass is possible but a separate AST makes debugging and optimization easier.

### 5.2 AST Nodes

```rust
enum Stat {
    Assign { targets: Vec<Var>, values: Vec<Expr> },
    LocalDecl { attribs: Vec<(String, Option<Attrib>)>, values: Vec<Expr> },
    GlobalDecl { attribs: Vec<(String, Option<Attrib>)>, values: Vec<Expr> },
    GlobalStar { attrib: Option<Attrib> },
    FunctionCall(FunctionCall),
    DoBlock(Block),
    WhileLoop { cond: Expr, body: Block },
    RepeatLoop { body: Block, cond: Expr },
    IfStat { cond: Expr, then_block: Block, elseif_clauses: Vec<(Expr, Block)>, else_block: Option<Block> },
    NumericFor { name: String, init: Expr, limit: Expr, step: Option<Expr>, body: Block },
    GenericFor { names: Vec<String>, iterators: Vec<Expr>, body: Block },
    ReturnStat(Vec<Expr>),
    Break,
    Goto(String),
    Label(String),
    FuncDef { name: FuncName, body: FuncBody },
    LocalFuncDef { name: String, body: FuncBody },
    GlobalFuncDef { name: String, body: FuncBody },
}

enum Expr {
    Nil, True, False,
    Integer(i64), Float(f64), String(Vec<u8>),
    VarArg,
    Var(Var),
    FunctionCall(FunctionCall),
    FunctionDef(FuncBody),
    TableConstructor(Vec<Field>),
    BinOp { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },
    UnOp { op: UnOp, operand: Box<Expr> },
}

enum Var {
    Name(String),
    Index { table: Box<Expr>, key: Box<Expr> },
    Field { table: Box<Expr>, name: String },
}
```

### 5.3 Operator Precedence (Pratt Parsing)

Precedence levels (low → high):
1. `or`
2. `and`
3. `< > <= >= ~= ==`
4. `|`
5. `~`
6. `&`
7. `<< >>`
8. `..` (right-assoc)
9. `+ -`
10. `* / // %`
11. unary: `not # - ~`
12. `^` (right-assoc)

## 6. Bytecode Design

### 6.1 Instruction Format

Use a **fixed 32-bit instruction** format, similar to PUC-Rio Lua but with our own opcode set:

```
Format A:  [opcode:8][A:8][B:8][C:8]
Format AB: [opcode:8][A:8][Bx:16]       (unsigned 16-bit)
Format AS: [opcode:8][A:8][sBx:16]      (signed 16-bit, biased)
Format AX: [opcode:8][Ax:24]            (unsigned 24-bit, for large constants)
```

- **A** usually refers to a destination register.
- **B, C** are source registers or constants (if MSB set, index into constant table).
- **sBx** is a signed offset, used for jumps.

### 6.2 Register Allocation

- Each function has a fixed register window (max 250 registers, matching Lua's practical limits).
- Locals map directly to registers.
- Temporaries are allocated on top of locals.
- The compiler tracks the current "top of stack" for register allocation.

### 6.3 Opcode Table

```
Category: Loading
  LOADNIL     A B       -- R[A], R[A+1], ..., R[A+B] := nil
  LOADBOOL    A B C     -- R[A] := (Bool)B; if C then PC++
  LOADINT     A sBx     -- R[A] := sBx (small integer)
  LOADK       A Bx      -- R[A] := K[Bx]
  LOADKX      A         -- R[A] := K[extra_arg] (next instruction is EXTRAARG)

Category: Upvalues & Globals  
  GETUPVAL    A B       -- R[A] := UpValue[B]
  SETUPVAL    A B       -- UpValue[B] := R[A]
  GETTABUP    A B C     -- R[A] := UpValue[B][K[C]]  (global access via _ENV)
  SETTABUP    A B C     -- UpValue[A][K[B]] := R[C]

Category: Table Operations
  GETTABLE    A B C     -- R[A] := R[B][R[C]]
  SETTABLE    A B C     -- R[A][R[B]] := R[C]
  GETI        A B C     -- R[A] := R[B][C]  (integer key)
  SETI        A B C     -- R[A][B] := R[C]  (integer key)
  GETFIELD    A B C     -- R[A] := R[B][K[C]]  (string key)
  SETFIELD    A B C     -- R[A][K[B]] := R[C]  (string key)
  NEWTABLE    A B C     -- R[A] := {} (size hints: B=array, C=hash)
  SETLIST     A B C     -- R[A][(C-1)*FPF+i] := R[A+i], 1 <= i <= B

Category: Arithmetic & Bitwise
  ADD         A B C     -- R[A] := R[B] + R[C]
  SUB         A B C     -- R[A] := R[B] - R[C]
  MUL         A B C     -- R[A] := R[B] * R[C]
  DIV         A B C     -- R[A] := R[B] / R[C]  (float division)
  IDIV        A B C     -- R[A] := R[B] // R[C]  (floor division)
  MOD         A B C     -- R[A] := R[B] % R[C]
  POW         A B C     -- R[A] := R[B] ^ R[C]
  UNM         A B       -- R[A] := -R[B]
  BAND        A B C     -- R[A] := R[B] & R[C]
  BOR         A B C     -- R[A] := R[B] | R[C]
  BXOR        A B C     -- R[A] := R[B] ~ R[C]
  BNOT        A B       -- R[A] := ~R[B]
  SHL         A B C     -- R[A] := R[B] << R[C]
  SHR         A B C     -- R[A] := R[B] >> R[C]

  // fused constant-operand versions for common patterns
  ADDK        A B C     -- R[A] := R[B] + K[C]
  SUBK        A B C     -- R[A] := R[B] - K[C]
  MULK        A B C     -- R[A] := R[B] * K[C]
  DIVK        A B C     -- R[A] := R[B] / K[C]
  IDIVK       A B C     -- R[A] := R[B] // K[C]
  MODK        A B C     -- R[A] := R[B] % K[C]
  POWK        A B C     -- R[A] := R[B] ^ K[C]
  ADDI        A B sC    -- R[A] := R[B] + sC (small int immediate)
  
Category: String
  CONCAT      A B C     -- R[A] := R[B] .. R[B+1] .. ... .. R[C]
  LEN         A B       -- R[A] := #R[B]  (length)

Category: Comparison & Logic
  EQ          A B C     -- if (R[B] == R[C]) ~= A then PC++
  LT          A B C     -- if (R[B] <  R[C]) ~= A then PC++
  LE          A B C     -- if (R[B] <= R[C]) ~= A then PC++
  EQK         A B C     -- if (R[B] == K[C]) ~= A then PC++
  EQI         A B sC    -- if (R[B] == sC) ~= A then PC++
  LTI         A B sC    -- if (R[B] <  sC) ~= A then PC++
  LEI         A B sC    -- if (R[B] <= sC) ~= A then PC++
  GTI         A B sC    -- if (R[B] >  sC) ~= A then PC++
  GEI         A B sC    -- if (R[B] >= sC) ~= A then PC++
  TEST        A C       -- if (not R[A]) == C then PC++
  TESTSET     A B C     -- if (not R[B]) == C then PC++ else R[A] := R[B]
  NOT         A B       -- R[A] := not R[B]

Category: Control Flow
  JMP         sBx       -- PC += sBx
  FORLOOP     A sBx     -- numeric for loop step
  FORPREP     A sBx     -- numeric for loop init
  TFORPREP    A sBx     -- generic for loop init
  TFORCALL    A C       -- call iterator: R[A+4],... := R[A](R[A+1], R[A+2])
  TFORLOOP    A sBx     -- generic for loop step
  
Category: Function
  CLOSURE     A Bx      -- R[A] := closure(Proto[Bx])
  CALL        A B C     -- R[A],...,R[A+C-2] := R[A](R[A+1],...,R[A+B-1])
  TAILCALL    A B C     -- return R[A](R[A+1],...,R[A+B-1])
  RETURN      A B       -- return R[A],...,R[A+B-2]
  RETURN0               -- return (no values)
  RETURN1     A         -- return R[A]
  VARARG      A C       -- R[A],R[A+1],...,R[A+C-2] := vararg
  VARARGPREP  A         -- adjust varargs (A = num fixed params)

Category: Miscellaneous
  MOVE        A B       -- R[A] := R[B]
  CLOSE       A         -- close upvalues >= R[A]; close to-be-closed vars
  TBC         A         -- mark R[A] as to-be-closed
  EXTRAARG    Ax        -- extra argument for previous instruction
```

### 6.4 Function Prototype (Proto)

```rust
struct Proto {
    /// Bytecode instructions
    code: Vec<u32>,
    /// Constants pool
    constants: Vec<Value>,
    /// Nested function prototypes
    protos: Vec<Proto>,
    /// Upvalue descriptors
    upvalues: Vec<UpvalueDesc>,
    /// Source location for debugging (line numbers per instruction)
    line_info: Vec<u32>,
    /// Local variable debug info
    local_vars: Vec<LocalVarInfo>,
    /// Source file name
    source: Option<LuaString>,
    /// Number of fixed parameters
    num_params: u8,
    /// Is variadic
    is_vararg: bool,
    /// Maximum stack size (registers needed)
    max_stack_size: u8,
}

struct UpvalueDesc {
    name: Option<LuaString>,
    /// true = upvalue is in the enclosing function's register;
    /// false = upvalue is in enclosing function's upvalue list
    in_stack: bool,
    /// Register index or upvalue index in parent
    index: u8,
}
```

## 7. Virtual Machine

### 7.1 Call Stack

```rust
struct CallFrame {
    /// Function being executed (closure)
    func: GcRef<Closure>,
    /// Base register index in the VM's register array
    base: usize,
    /// Program counter (index into Proto.code)
    pc: usize,
    /// Number of expected results
    num_results: i32,
    /// Previous frame's top (for restoring)
    prev_top: usize,
}
```

The VM uses a **single contiguous register array** (`Vec<Value>`) shared across all call frames. Each `CallFrame` has a `base` offset into this array.

### 7.2 Upvalue Implementation

```rust
enum Upvalue {
    /// Points to a register in the stack (open upvalue)
    Open { stack_index: usize },
    /// The value has been captured (closed upvalue)
    Closed(Value),
}
```

- Open upvalues are tracked in a linked list per thread, sorted by stack index.
- When a scope closes (CLOSE instruction, function return, etc.), open upvalues pointing into that scope are "closed" — their value is copied from the stack into the `Closed` variant.

### 7.3 Main Dispatch Loop

```rust
fn execute(&mut self) -> Result<(), LuaError> {
    loop {
        let instruction = self.fetch();
        let opcode = decode_opcode(instruction);
        match opcode {
            OP_MOVE => { /* ... */ }
            OP_LOADK => { /* ... */ }
            OP_ADD => { /* ... with metamethod fallback */ }
            // ... all opcodes
            OP_RETURN => {
                // restore previous call frame
                // return values
            }
        }
    }
}
```

**Performance considerations:**
- Use computed goto equivalent where possible (in Rust, a well-optimized `match` compiles to a jump table).
- Fast paths for common operations (integer arithmetic, string indexing, table access with no metamethods).
- Inline caching hints for method dispatch (considered for later optimization phase).

### 7.4 Metamethod Dispatch

For each operation that may trigger a metamethod:
1. Attempt the fast path (e.g., both operands are numbers for arithmetic).
2. If fast path fails, look up the metamethod in the metatable.
3. If found, call the metamethod (push a new `CallFrame`).
4. If not found, raise a type error.

Metamethods supported:
- Arithmetic: `__add, __sub, __mul, __div, __mod, __pow, __unm, __idiv`
- Bitwise: `__band, __bor, __bxor, __bnot, __shl, __shr`
- Comparison: `__eq, __lt, __le`
- Other: `__concat, __len, __index, __newindex, __call`
- Lifecycle: `__gc, __close, __mode, __name, __tostring, __metatable, __pairs`

### 7.5 Tail Call Optimization

When `TAILCALL` is executed:
1. Move arguments to the current frame's base.
2. Replace the current `CallFrame`'s function and prototype.
3. Reset PC to 0.
4. Continue execution without pushing a new frame.

This ensures proper tail recursion — the stack does not grow.

## 8. Table Implementation

### 8.1 Hybrid Array + Hash Map

```rust
struct Table {
    /// Array part: consecutive integer keys [1..n]
    array: Vec<Value>,
    /// Hash part: open-addressing hash map for other keys
    hash: HashMap,  // custom implementation for performance
    /// Metatable reference
    metatable: Option<GcRef<Table>>,
}
```

### 8.2 Hash Map Details

- **Open addressing** with linear probing or Robin Hood hashing.
- Power-of-2 table sizes for fast modulo (bitwise AND).
- Keys are `Value` (using the value's hash; strings use precomputed hash).
- Float keys that are integers are canonicalized to integer keys.
- `NaN` keys are rejected.

### 8.3 Length Operator

- For sequence tables (array part has no holes), `#t` = array length.
- For non-sequence tables, binary search for a border.
- O(log n) guaranteed per spec.

### 8.4 `table.create` (Lua 5.5 new)

Pre-allocate array and hash parts based on hints.

## 9. Closure and Coroutine

### 9.1 Closure

```rust
struct Closure {
    proto: GcRef<Proto>,
    upvalues: Vec<GcRef<Upvalue>>,
}
```

### 9.2 Coroutine (Thread)

```rust
struct Thread {
    /// Thread's own register stack
    stack: Vec<Value>,
    /// Thread's call frames
    call_stack: Vec<CallFrame>,
    /// Open upvalues in this thread
    open_upvalues: Vec<GcRef<Upvalue>>,
    /// Coroutine status
    status: CoroutineStatus,
}

enum CoroutineStatus {
    Suspended,
    Running,
    Normal,
    Dead,
}
```

**Yield/Resume mechanism:**
- `coroutine.yield()`: Save current VM state (PC, registers, call stack) in the `Thread`. Return control to the resumer.
- `coroutine.resume()`: Restore the target thread's state. Switch the active thread.
- Each coroutine has its **own stack and call frames**, so switching is simply swapping which `Thread` the VM operates on.

### 9.3 To-be-closed Variables

- `TBC` instruction marks a register as to-be-closed.
- When the scope exits (via `CLOSE`, `RETURN`, or error unwinding), the VM calls `__close` metamethod on the TBC value.
- Close in reverse declaration order.
- Errors during close are handled (warn, continue closing others).

## 10. Garbage Collector

### 10.1 Strategy

Implement a **tri-color mark-and-sweep** GC with both **incremental** and **generational** modes, matching Lua 5.5 semantics.

### 10.2 GC Object Header

```rust
struct GcHeader {
    /// Next pointer for intrusive linked list of all GC objects
    next: Option<NonNull<GcHeader>>,
    /// GC color: White (0/1), Gray, Black
    color: GcColor,
    /// Object type tag
    obj_type: GcType,
    /// Generation (for generational mode): Young, Old, Survival
    generation: Generation,
    /// Marked for finalization
    has_finalizer: bool,
}

enum GcColor {
    White0,  // current white
    White1,  // other white (for flip)
    Gray,
    Black,
}
```

### 10.3 Incremental Mode

1. **Mark phase:** Start from roots (global table, registry, stack, open upvalues). Mark objects gray → traverse them (mark children) → turn black.
2. **Sweep phase:** Walk the allocation list. Free white objects. Flip white bit for next cycle.
3. **Interleaving:** GC work is done in small steps, interleaved with VM execution. Controlled by `pause`, `stepmul`, `stepsize` parameters.
4. **Write barrier:** When a black object is mutated to point to a white object, either:
   - *Barrier forward*: Mark the white child gray (push to gray list).
   - *Barrier back*: Mark the black parent gray again (for tables that are frequently written to).

### 10.4 Generational Mode

- Objects start as **Young**.
- **Minor collection:** Only traverse young objects. Survivors promoted to **Survival** → **Old** after surviving multiple collections.
- **Major collection:** Full mark-and-sweep of all objects. Triggered when old-object memory exceeds threshold.
- Parameters: `minormul`, `minormajor`, `majorminor`.

### 10.5 Weak Tables

- Tables with `__mode` = "k", "v", or "kv".
- During sweep/finalization, clear entries where weak keys/values have been collected.
- Ephemeron tables ("k" mode): Value is reachable only if key is reachable.

### 10.6 Finalizers (__gc)

- Objects with `__gc` at metatable-set time are marked for finalization.
- When dead, moved to a finalization queue instead of freed.
- Finalizers called in reverse marking order.
- Object is resurrected (accessible during finalizer). Freed in next cycle if unreachable.

### 10.7 GcRef Smart Pointer

```rust
/// A safe reference to a GC-managed object.
/// This is essentially a raw pointer with a phantom lifetime.
/// The GC guarantees the object is alive as long as it's reachable.
#[derive(Clone, Copy)]
struct GcRef<T> {
    ptr: NonNull<GcManaged<T>>,
}

struct GcManaged<T> {
    header: GcHeader,
    data: T,
}
```

**Write barrier integration:** Every `GcRef` write to a GC-managed field must trigger the write barrier. This is enforced through a mutation API:
```rust
impl Table {
    fn raw_set(&mut self, gc: &mut Gc, key: Value, value: Value) {
        // ... set the value ...
        gc.write_barrier(self, value); // barrier for GC
    }
}
```

## 11. Standard Libraries

### 11.1 Implementation Plan

All standard libraries implemented as Rust native functions registered into the global environment.

| Library | Module | Priority | Notes |
|---------|--------|----------|-------|
| basic | (global) | P0 | `print`, `type`, `error`, `pcall`, `xpcall`, `assert`, `tonumber`, `tostring`, etc. |
| string | `string` | P0 | Pattern matching, `format`, `byte`/`char`, `pack`/`unpack` |
| table | `table` | P0 | `insert`, `remove`, `sort`, `concat`, `move`, `pack`, `unpack`, `create` |
| math | `math` | P0 | Direct mapping to Rust `f64` methods |
| coroutine | `coroutine` | P0 | `create`, `resume`, `yield`, `wrap`, `status`, `close`, `isyieldable`, `running` |
| io | `io` | P1 | File I/O using Rust `std::fs` / `std::io` |
| os | `os` | P1 | `clock`, `time`, `date`, `execute`, `getenv`, `remove`, `rename`, `tmpname` |
| package | `package` | P1 | `require`, `searchers` (Lua-file loading only, no C modules) |
| utf8 | `utf8` | P1 | Light wrapper around Rust's UTF-8 facilities |
| debug | `debug` | P2 | `getinfo`, `getlocal`, `traceback`, `sethook`, etc. |

### 11.2 Native Function Interface

```rust
type NativeFn = fn(&mut VmState, &[Value]) -> Result<Vec<Value>, LuaError>;
```

## 12. Error Handling

### 12.1 Lua Errors → Rust

- Lua `error()` maps to Rust `Result::Err(LuaError)`.
- `pcall` / `xpcall` are implemented by catching `LuaError` at the corresponding call frame.
- Error objects can be any `Value` (not just strings).

```rust
struct LuaError {
    value: Value,          // the error object
    traceback: Vec<TraceEntry>,  // optional stack trace
}
```

### 12.2 Protected Calls

- `pcall(f, ...)`: Execute `f` in a protected context. Catch `LuaError`, return `(false, err)`. On success, return `(true, results...)`.
- `xpcall(f, msgh, ...)`: Same, but call `msgh(err)` before unwinding.

### 12.3 Message Handler

- Called before stack unwinding so `debug.traceback` can inspect the stack.
- If the handler itself errors, count nesting depth and abort with `LUA_ERRERR` equivalent.

## 13. Scoping & Variable Declarations (Lua 5.5 New)

### 13.1 `global` keyword

Lua 5.5 introduces `global` declarations and `global *` / `global<const> *`.

- **Compile-time only:** `global` declarations affect name resolution but don't generate special bytecode (globals are still `_ENV[name]`).
- The compiler tracks a **declaration context**:
  - Default: `global *` (all undeclared names are globals, read-write).
  - After `global<const> *`: Undeclared names are read-only globals.
  - After `global SomeName`: Only explicitly declared names are valid.
- A `global` declaration for a name simply tells the compiler "this name is a global, compile it as `_ENV[name]`".
- With `<const>` attribute, the compiler disallows assignment to that name.

### 13.2 Implementation

In the compiler's scope tracking:
```rust
struct Scope {
    locals: Vec<LocalVar>,
    /// The active global declaration mode
    global_mode: GlobalMode,
    /// Explicitly declared globals in this scope
    declared_globals: HashSet<String>,
}

enum GlobalMode {
    /// Implicit `global *` — all free names OK
    ImplicitStar,
    /// Explicit `global *` — all free names OK
    ExplicitStar { read_only: bool },
    /// No star — only declared names allowed
    Restricted,
}
```

## 14. Module Structure (Rust Crate Layout)

```
src/
├── lib.rs           -- Public API: LuaState, load, execute
├── lexer.rs         -- Lexer / Tokenizer
├── token.rs         -- Token types
├── parser.rs        -- Recursive descent parser
├── ast.rs           -- AST node definitions
├── compiler.rs      -- AST → Bytecode compiler
├── bytecode.rs      -- Instruction encoding/decoding, Proto
├── vm.rs            -- Main VM dispatch loop
├── value.rs         -- Value representation
├── table.rs         -- Table implementation (array + hash)
├── string.rs        -- LuaString, interning pool
├── gc.rs            -- Garbage collector
├── closure.rs       -- Closure, Upvalue
├── coroutine.rs     -- Coroutine / Thread
├── error.rs         -- Error types
├── stdlib/
│   ├── mod.rs       -- Library registration
│   ├── base.rs      -- Basic library (print, type, error, etc.)
│   ├── string.rs    -- String library
│   ├── table.rs     -- Table library
│   ├── math.rs      -- Math library
│   ├── io.rs        -- I/O library
│   ├── os.rs        -- OS library
│   ├── coroutine.rs -- Coroutine library
│   ├── package.rs   -- Package/require library
│   ├── utf8.rs      -- UTF-8 library
│   └── debug.rs     -- Debug library
└── main.rs          -- Standalone interpreter (REPL + file execution)
```

## 15. Implementation Phases

### Phase 1: Core Language (MVP)
- Lexer + Parser (full grammar)
- Compiler (basic opcodes)
- VM (basic dispatch, no metamethods)
- Value types: nil, bool, int, float, string, table, closure
- Basic GC (simple mark-and-sweep, stop-the-world)
- Stdlib: `print`, `type`, `tostring`, `tonumber`, `error`, `pcall`, `assert`
- **Goal:** Can run simple Lua programs (fibonacci, loops, recursion, tables)

### Phase 2: Complete Language Semantics
- All metamethods
- Full operator semantics (coercions, integer/float rules)
- `local <const>`, `local <close>` (to-be-closed variables)
- `global` declarations (5.5 specific)
- Vararg functions and `...` expressions
- Multiple return values, adjustment rules
- Tail call optimization
- `goto` / labels (scope validation)
- Stdlib: string library (including pattern matching), table library, math library

### Phase 3: Coroutines & Advanced Features
- Coroutine implementation (yield/resume)
- Coroutine library
- Full error handling (xpcall, message handler, stack traceback)
- Weak tables
- Finalizers (__gc)
- I/O library, OS library
- Package/require (Lua file loading)
- Debug library

### Phase 4: Performance & GC
- Incremental GC
- Generational GC
- NaN-boxing value representation (optional)
- Instruction specialization (ADDI, GETI, SETI, EQK, etc.)
- String interning optimization
- Table access caching
- Benchmarking against PUC-Rio Lua

## 16. Testing Strategy

- **Unit tests:** Per-module tests (lexer token streams, parser AST output, compiler bytecode output, VM instruction execution).
- **Integration tests (`tests/` directory):** Lua source files that exercise language features. Compare output against PUC-Rio Lua 5.5.
- **Lua test suite:** Run the official Lua test suite (adapted for non-FFI).
- **Benchmark suite:** Standard Lua benchmarks (fibonacci, n-body, binary-trees, spectral-norm, etc.) for performance comparison.
- **Fuzzing:** Property-based testing with random Lua programs for crash/correctness.

## 17. Key Design Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Value repr | Enum (phase 1), NaN-boxing (phase 4) | Correctness first, optimize later |
| Parser output | AST (not single-pass compile) | Easier debugging, potential for AST-level optimization |
| Instruction format | Fixed 32-bit, 4 formats | Matches proven Lua design, simple decoding |
| GC | Tri-color mark-sweep (incremental + generational) | Required by Lua 5.5 spec semantics |
| Hash table | Open addressing, power-of-2 | Cache-friendly, fast for typical Lua workloads |
| String interning | Global pool, lazy for long strings | Fast equality, memory dedup |
| Coroutines | Separate stack per thread | Clean yield/resume, no stack-copying needed |
| Error handling | Rust Result<T, LuaError> | Natural mapping from Lua's longjmp-style errors |
| No unsafe | Minimize; only for GC pointers, NaN-boxing | Safety-first Rust philosophy |
