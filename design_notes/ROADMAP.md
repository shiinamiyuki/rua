# Rua Roadmap

> Lua 5.5 interpreter in Rust. See `rua-design.md` for detailed design.

---

## Phase 1: Core Language (MVP)

Goal: Run simple Lua programs — arithmetic, control flow, functions, tables, recursion.

### M1.1 — Foundation & Value System
- [ ] Crate skeleton: `lib.rs`, module stubs, `main.rs` (CLI entry)
- [ ] `Value` enum: `Nil`, `Boolean(bool)`, `Integer(i64)`, `Float(f64)`, `Object(GcRef)`
- [ ] `GcRef` / `GcObject` basics: `String`, `Table`, `Closure`
- [ ] `LuaString`: byte-buffer backed, precomputed hash, `PartialEq` by content
- [ ] String interning pool (short strings ≤40 bytes)

### M1.2 — Lexer
- [ ] Hand-written lexer on `&[u8]`
- [ ] All tokens: keywords (incl. `global`), operators, punctuation
- [ ] Numerals: decimal int, hex int, float, hex float
- [ ] Short strings with all escape sequences (`\n`, `\xXX`, `\u{XXX}`, `\z`, etc.)
- [ ] Long strings (`[[...]]`, `[=[...]=]`)
- [ ] Comments: short (`--`) and long (`--[[...]]`)
- [ ] Source location tracking (line, column)
- [ ] Unit tests: token stream for representative inputs

### M1.3 — Parser
- [ ] Recursive descent parser producing AST
- [ ] AST node types (`ast.rs`): `Block`, `Stat`, `Expr`, `Var`, `FuncBody`, etc.
- [ ] Statement parsing: assignment, `do..end`, `while`, `repeat..until`, `if..elseif..else`, `for` (numeric + generic), `return`, `break`, `goto`, label, function call as statement
- [ ] Expression parsing: Pratt parser with correct precedence & associativity
  - 12 precedence levels, right-assoc for `^` and `..`
- [ ] Function definitions: `function f()`, `local function`, `global function`, method syntax `:`
- [ ] Table constructors: `[exp]=exp`, `name=exp`, positional
- [ ] Variable declarations: `local`, `global`, `global *`, attributes `<const>`, `<close>`
- [ ] Vararg param (`...`, `... name`)
- [ ] Unit tests: parse + round-trip for all statement/expression types

### M1.4 — Bytecode Compiler
- [ ] `Proto` struct: code, constants, upvalue descriptors, nested protos, debug info
- [ ] 32-bit instruction encoding/decoding (formats A, AB, AS, AX)
- [ ] Scope / local variable tracking, register allocation
- [ ] Upvalue resolution (in_stack vs. parent upvalue chain)
- [ ] Compile expressions → register-targeted code
  - Constants: `LOADNIL`, `LOADBOOL`, `LOADINT`, `LOADK`
  - Arithmetic: `ADD`, `SUB`, `MUL`, `DIV`, `IDIV`, `MOD`, `POW`, `UNM`
  - Bitwise: `BAND`, `BOR`, `BXOR`, `BNOT`, `SHL`, `SHR`
  - Comparison + conditional jumps: `EQ`, `LT`, `LE`, `TEST`, `TESTSET`
  - Concatenation: `CONCAT`
  - Length: `LEN`
  - Logic: short-circuit `and`/`or`, `NOT`
- [ ] Compile statements → bytecode
  - Assignment (local, global via `GETTABUP`/`SETTABUP` on `_ENV`)
  - Control flow: `JMP`, `FORLOOP`, `FORPREP`, `TFORPREP`, `TFORCALL`, `TFORLOOP`
  - Function call: `CALL`, `TAILCALL`, `RETURN`, `RETURN0`, `RETURN1`
  - Table: `NEWTABLE`, `SETTABLE`, `GETTABLE`, `SETLIST`
  - Upvalues: `GETUPVAL`, `SETUPVAL`, `GETTABUP`, `SETTABUP`
  - Closures: `CLOSURE`
  - Vararg: `VARARG`, `VARARGPREP`
  - Misc: `MOVE`, `CLOSE`, `TBC`
- [ ] `_ENV` as upvalue[0] of every chunk
- [ ] Bytecode disassembler (for debugging)
- [ ] Unit tests: compile simple programs, verify instruction sequences

### M1.5 — Virtual Machine
- [ ] VM state: register stack (`Vec<Value>`), call stack (`Vec<CallFrame>`)
- [ ] Main dispatch loop (`match` on opcode)
- [ ] Implement all Phase 1 opcodes (no metamethod fallback yet)
- [ ] Function calls: push `CallFrame`, argument adjustment, result adjustment
- [ ] Upvalue open/close mechanics
- [ ] Numeric for loop
- [ ] Generic for loop
- [ ] Multiple return values
- [ ] Error handling: `LuaError`, `pcall` (basic)

### M1.6 — Basic GC
- [ ] Stop-the-world mark-and-sweep
- [ ] GC object header (type tag, mark bit, next pointer)
- [ ] Allocation: all GC objects go through `Gc::alloc()`
- [ ] Root scanning: stack, global table, open upvalues
- [ ] Mark phase: trace from roots
- [ ] Sweep phase: free unmarked objects
- [ ] Trigger: allocate N bytes → run GC cycle

### M1.7 — Table
- [ ] Hybrid array + hash map
- [ ] Array part: `Vec<Value>`, integer keys `[1..n]`
- [ ] Hash part: open-addressing, power-of-2 sizing
- [ ] `rawget`, `rawset`, `next` traversal
- [ ] Length operator (binary search for border)
- [ ] Float-key-to-integer canonicalization, NaN rejection

### M1.8 — Minimal Stdlib & CLI
- [ ] Basic library: `print`, `type`, `tostring`, `tonumber`, `error`, `pcall`, `assert`, `ipairs`, `pairs`, `next`, `select`, `rawget`, `rawset`, `rawequal`, `rawlen`, `setmetatable`, `getmetatable`
- [ ] `_VERSION` = `"Lua 5.5"`
- [ ] CLI (`main.rs`): `rua script.lua`, `rua -e "code"`, basic REPL
- [ ] Integration tests: fibonacci, factorial, table ops, closures, scoping

**Milestone check:** `rua tests/fib.lua` prints correct output.

---

## Phase 2: Complete Language Semantics

Goal: Pass the majority of the Lua test suite (non-coroutine, non-IO parts).

### M2.1 — Metamethods
- [ ] Metatable storage on `Table` (and per-type metatables for string)
- [ ] Arithmetic metamethods: `__add`, `__sub`, `__mul`, `__div`, `__mod`, `__pow`, `__unm`, `__idiv`
- [ ] Bitwise metamethods: `__band`, `__bor`, `__bxor`, `__bnot`, `__shl`, `__shr`
- [ ] Comparison: `__eq`, `__lt`, `__le`
- [ ] String/length: `__concat`, `__len`
- [ ] Table access: `__index` (function or table chain), `__newindex` (function or table chain)
- [ ] `__call` metamethod (with 15-level chain limit)
- [ ] `__tostring`, `__name`, `__metatable`
- [ ] VM fast path: check metatable only on type mismatch / table miss

### M2.2 — Full Operator Semantics
- [ ] Integer/float coercion rules (arithmetic, bitwise, comparison)
- [ ] String-to-number coercion (string library metamethods)
- [ ] Float-to-integer exact conversion checks
- [ ] Integer overflow wrapping (two's complement)
- [ ] Division/modulo edge cases (floor division semantics)
- [ ] Exponentiation always returns float

### M2.3 — Variable Declarations (Lua 5.5)
- [ ] `global` name declarations (compiler-level name resolution)
- [ ] `global *` / `global<const> *` (implicit global mode tracking per scope)
- [ ] `local <const>` (compile-time assignment check)
- [ ] `local <close>` → `TBC` instruction, `__close` metamethod on scope exit
- [ ] To-be-closed: reverse-order close, error handling during close, interaction with `break`/`goto`/`return`

### M2.4 — Vararg & Multi-return
- [ ] `VARARGPREP` / `VARARG` opcodes
- [ ] Named vararg table (`... args` syntax)
- [ ] Vararg table optimization (avoid table creation when possible)
- [ ] Multi-return adjustment in all contexts: assignment, call args, table constructor, return, for-in
- [ ] `select('#', ...)` and `select(n, ...)`

### M2.5 — Tail Calls & Goto
- [ ] `TAILCALL`: reuse current frame, move args
- [ ] Proper tail recursion (no stack growth)
- [ ] `goto` / labels: compile to `JMP`
- [ ] Scope validation: can't jump into a local's scope, can't shadow label in same block

### M2.6 — String Library
- [ ] `string.byte`, `string.char`, `string.len`, `string.sub`, `string.rep`, `string.reverse`
- [ ] `string.lower`, `string.upper`
- [ ] `string.format` (all specifiers: `d`, `i`, `u`, `f`, `e`, `g`, `x`, `o`, `s`, `c`, `q`, `p`)
- [ ] Pattern matching engine: `string.find`, `string.match`, `string.gmatch`, `string.gsub`
  - Character classes (`%a`, `%d`, `%w`, etc.), quantifiers (`*`, `+`, `-`, `?`), captures, `%bxy`, `%f[set]`
- [ ] `string.dump` (serialize `Proto` to binary)
- [ ] `string.pack`, `string.unpack`, `string.packsize`
- [ ] String metatable with `__index = string`

### M2.7 — Table Library
- [ ] `table.insert`, `table.remove`, `table.sort` (with custom comparator)
- [ ] `table.concat`, `table.move`
- [ ] `table.pack`, `table.unpack`
- [ ] `table.create` (Lua 5.5: pre-allocate array + hash)

### M2.8 — Math Library
- [ ] All math functions mapped to Rust `f64` operations
- [ ] `math.random` / `math.randomseed` (xoshiro256**)
- [ ] `math.maxinteger`, `math.mininteger`, `math.huge`, `math.pi`
- [ ] `math.type`, `math.tointeger`

**Milestone check:** Lua test suite `strings.lua`, `math.lua`, `sort.lua`, `attrib.lua` pass.

---

## Phase 3: Coroutines & Advanced Features

Goal: Full language coverage except FFI. All standard libraries functional.

### M3.1 — Coroutines
- [ ] `Thread` struct: own stack, call frames, open upvalues, status
- [ ] `coroutine.create` → allocate new `Thread`
- [ ] `coroutine.resume` → save current thread state, switch to target thread
- [ ] `coroutine.yield` → save target thread state, return to resumer
- [ ] Status tracking: suspended / running / normal / dead
- [ ] `coroutine.wrap` → returns iterator function
- [ ] `coroutine.status`, `coroutine.running`, `coroutine.isyieldable`
- [ ] `coroutine.close` → close pending TBC variables, set dead
- [ ] Yield across pcall / metamethod boundaries
- [ ] Error propagation from coroutine to resumer

### M3.2 — Error Handling (Complete)
- [ ] `xpcall` with message handler
- [ ] Message handler nesting limit (prevent infinite recursion)
- [ ] Stack traceback generation
- [ ] Error position info (source:line in error messages)
- [ ] `error(msg, level)` — level-based position annotation
- [ ] Nil error object → string conversion

### M3.3 — Weak Tables & Finalizers
- [ ] `__mode` = "k" / "v" / "kv" support in GC
- [ ] Ephemeron table semantics (key-reachability determines value-reachability)
- [ ] Clear dead entries during sweep
- [ ] `__gc` finalizer: finalization queue, reverse-order execution
- [ ] Resurrection: finalized objects survive one cycle
- [ ] Re-marking for finalization

### M3.4 — I/O Library
- [ ] `io.open`, `io.close`, `io.read`, `io.write`, `io.lines`
- [ ] File handle object with methods: `file:read`, `file:write`, `file:seek`, `file:close`, `file:lines`, `file:flush`, `file:setvbuf`
- [ ] `io.stdin`, `io.stdout`, `io.stderr`
- [ ] `io.input`, `io.output` (default file handles)
- [ ] `io.tmpfile`, `io.type`, `io.popen`
- [ ] File handle `__gc` / `__close` metamethods

### M3.5 — OS Library
- [ ] `os.clock`, `os.time`, `os.date`, `os.difftime`
- [ ] `os.execute`, `os.exit`, `os.getenv`
- [ ] `os.remove`, `os.rename`, `os.tmpname`
- [ ] `os.setlocale`

### M3.6 — Package / Require
- [ ] `require(modname)` — searcher chain
- [ ] `package.loaded`, `package.preload`, `package.path`
- [ ] `package.searchpath`
- [ ] `package.searchers`: preload searcher + Lua file searcher
- [ ] No C module loading (by design — no FFI)
- [ ] `package.config`

### M3.7 — UTF-8 Library
- [ ] `utf8.char`, `utf8.codepoint`, `utf8.codes`
- [ ] `utf8.len`, `utf8.offset`
- [ ] `utf8.charpattern`

### M3.8 — Debug Library
- [ ] `debug.getinfo` (function info: source, line, name, nparams, etc.)
- [ ] `debug.getlocal`, `debug.setlocal`
- [ ] `debug.getupvalue`, `debug.setupvalue`
- [ ] `debug.getmetatable`, `debug.setmetatable`
- [ ] `debug.traceback`
- [ ] `debug.sethook`, `debug.gethook` (call/return/line/count hooks)
- [ ] `debug.upvalueid`, `debug.upvaluejoin`
- [ ] `debug.getuservalue`, `debug.setuservalue`
- [ ] `debug.getregistry`

**Milestone check:** Full Lua 5.5 test suite passes (excluding C-API tests).

---

## Phase 4: Performance & Production Quality

Goal: Performance comparable to PUC-Rio Lua. Production-ready interpreter.

### M4.1 — Incremental GC
- [ ] Tri-color marking: white → gray → black
- [ ] Write barrier (forward barrier + backward barrier for tables)
- [ ] Incremental mark: step-based, controlled by `stepmul` / `stepsize`
- [ ] Incremental sweep
- [ ] GC `pause` parameter
- [ ] `collectgarbage` API: all options (`"collect"`, `"stop"`, `"restart"`, `"step"`, `"count"`, `"isrunning"`, `"incremental"`, `"generational"`, `"param"`)

### M4.2 — Generational GC
- [ ] Object generations: Young → Survival → Old
- [ ] Minor collection (young objects only)
- [ ] Major collection (full mark-sweep)
- [ ] Mode switching: minor→major (`minormajor` param), major→minor (`majorminor` param)
- [ ] `minormul` parameter

### M4.3 — Instruction Specialization
- [ ] Constant-operand variants: `ADDK`, `SUBK`, `MULK`, `DIVK`, `IDIVK`, `MODK`, `POWK`
- [ ] Immediate integer variants: `ADDI`, `EQI`, `LTI`, `LEI`, `GTI`, `GEI`
- [ ] Indexed access: `GETI`, `SETI`, `GETFIELD`, `SETFIELD`, `EQK`
- [ ] Compiler: emit specialized opcodes when operands are known constants/integers

### M4.4 — Value Representation Optimization
- [ ] NaN-boxing: pack all values into 8 bytes
- [ ] Benchmark enum vs NaN-boxing; adopt if measurably faster
- [ ] Small string optimization (inline short strings in value)

### M4.5 — Table & String Optimization
- [ ] Table inline caching (monomorphic/polymorphic) for field access
- [ ] Array-part auto-resize heuristics
- [ ] String interning: lazy intern for long strings, dedup on table key use
- [ ] String hash seed randomization

### M4.6 — Benchmarking & Profiling
- [ ] Benchmark harness: criterion.rs
- [ ] Standard benchmarks: fibonacci, n-body, binary-trees, spectral-norm, mandelbrot, fannkuch
- [ ] Profile: flamegraph, memory usage tracking
- [ ] Compare against PUC-Rio Lua 5.5
- [ ] Regression test for performance (CI check)

### M4.7 — Polish
- [ ] Precompiled chunk load/dump (binary format compatible or custom)
- [ ] `string.dump` / `load` binary chunk support
- [ ] Complete CLI: `rua -i` interactive REPL, `rua -l lib`, `rua -W` warnings
- [ ] `warn()` function and control messages (`@on`, `@off`)
- [ ] Comprehensive error messages with source locations
- [ ] Documentation: README, usage guide, architecture docs

**Milestone check:** All benchmarks within 2x of PUC-Rio Lua. Zero panics on fuzz testing.

---

## Status Legend

- [ ] Not started
- [x] Completed
- [~] In progress
