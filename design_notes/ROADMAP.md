# Rua Roadmap

> Lua 5.5 interpreter in Rust. See `rua-design.md` for detailed design.

---

## Phase 1: Core Language (MVP)

Goal: Run simple Lua programs — arithmetic, control flow, functions, tables, recursion.

### M1.1 — Foundation & Value System
- [x] Crate skeleton: `lib.rs`, module stubs, `main.rs` (CLI entry)
- [x] `Value` enum: `Nil`, `Boolean(bool)`, `Integer(i64)`, `Float(f64)`, `Object(GcRef)`
- [x] `GcRef` / `GcObject` basics: `String`, `Table`, `Closure`
- [x] `LuaString`: byte-buffer backed, precomputed hash, `PartialEq` by content
- [x] String interning pool (short strings ≤40 bytes)

### M1.2 — Lexer
- [x] Hand-written lexer on `&[u8]`
- [x] All tokens: keywords (incl. `global`), operators, punctuation
- [x] Numerals: decimal int, hex int, float, hex float
- [x] Short strings with all escape sequences (`\n`, `\xXX`, `\u{XXX}`, `\z`, etc.)
- [x] Long strings (`[[...]]`, `[=[...]=]`)
- [x] Comments: short (`--`) and long (`--[[...]]`)
- [x] Source location tracking (line, column)
- [x] Unit tests: token stream for representative inputs

### M1.3 — Parser
- [x] Recursive descent parser producing AST
- [x] AST node types (`ast.rs`): `Block`, `Stat`, `Expr`, `Var`, `FuncBody`, etc.
- [x] Statement parsing: assignment, `do..end`, `while`, `repeat..until`, `if..elseif..else`, `for` (numeric + generic), `return`, `break`, `goto`, label, function call as statement
- [x] Expression parsing: Pratt parser with correct precedence & associativity
  - 12 precedence levels, right-assoc for `^` and `..`
- [x] Function definitions: `function f()`, `local function`, `global function`, method syntax `:`
- [x] Table constructors: `[exp]=exp`, `name=exp`, positional
- [x] Variable declarations: `local`, `global`, `global *`, attributes `<const>`, `<close>`
- [x] Vararg param (`...`, `... name`)
- [x] Unit tests: parse + round-trip for all statement/expression types

### M1.4 — Bytecode Compiler
- [x] `Proto` struct: code, constants, upvalue descriptors, nested protos, debug info
- [x] 32-bit instruction encoding/decoding (3 formats: ABC, ABx, AsBx) — see [`bytecode.md`](bytecode.md)
- [x] 50 generic opcodes (no specialized variants — deferred to M4.3)
- [x] Scope / local variable tracking, register allocation
- [x] Upvalue resolution (in_stack vs. parent upvalue chain)
- [x] Compile expressions → register-targeted code
  - Constants: `LOADNIL`, `LOADBOOL`, `LOADI`, `LOADK`
  - Arithmetic: `ADD`, `SUB`, `MUL`, `DIV`, `IDIV`, `MOD`, `POW`, `UNM`
  - Bitwise: `BAND`, `BOR`, `BXOR`, `BNOT`, `SHL`, `SHR`
  - Comparison + conditional jumps: `EQ`, `LT`, `LE`, `TEST`, `TESTSET`
  - Concatenation: `CONCAT`
  - Length: `LEN`
  - Logic: short-circuit `and`/`or`, `NOT`
- [x] Compile statements → bytecode
  - Assignment (local, global via `GETTABUP`/`SETTABUP` on `_ENV`)
  - Control flow: `JMP`, `FORLOOP`, `FORPREP`, `TFORPREP`, `TFORLOOP`
  - Function call: `CALL`, `TAILCALL`, `RETURN`
  - Table: `NEWTABLE`, `SETTABLE`, `GETTABLE`, `SETLIST`
  - Upvalues: `GETUPVAL`, `SETUPVAL`, `GETTABUP`, `SETTABUP`
  - Closures: `CLOSURE`
  - Vararg: `VARARG`, `VARARGPREP`
  - Misc: `MOVE`, `CLOSE`, `TBC`
- [x] `_ENV` as upvalue[0] of every chunk
- [x] Bytecode disassembler (for debugging)
- [x] Unit tests: compile simple programs, verify instruction sequences

### M1.5 — Virtual Machine
- [x] VM state: register stack (`Vec<Value>`), call stack (`Vec<CallFrame>`)
- [x] Main dispatch loop (`match` on opcode)
- [x] Implement all Phase 1 opcodes (no metamethod fallback yet)
- [x] Function calls: push `CallFrame`, argument adjustment, result adjustment
- [x] Upvalue open/close mechanics
- [x] Numeric for loop
- [x] Generic for loop
- [x] Multiple return values
- [x] Error handling: `LuaError`, `pcall` (basic)

### M1.6 — Basic GC
- [x] Stop-the-world mark-and-sweep
- [x] GC object header (type tag, mark bit, next pointer) — intrusive linked list
- [x] Allocation: all GC objects go through `Gc::alloc()`
- [x] Root scanning: stack, global table, open upvalues
- [x] Mark phase: trace from roots (gray worklist, iterative)
- [x] Sweep phase: free unmarked objects (intrusive list pointer surgery)
- [x] Trigger: allocate N bytes → run GC cycle (threshold-based, checked at NEWTABLE/CLOSURE/CONCAT)

### M1.7 — Table
- [x] Hybrid array + open-addressing hash map
- [x] Array part: `Vec<Value>`, integer keys `[1..n]`
- [x] Hash part: open-addressing, power-of-2 sizing, linear probing, backward-shift deletion
- [x] `rawget`, `rawset`, `next` traversal
- [x] Length operator (array part length)
- [x] Float-key-to-integer canonicalization, NaN rejection

### M1.8 — Minimal Stdlib & CLI
- [x] Basic library: `print`, `type`, `tostring`, `tonumber`, `error`, `pcall`, `assert`, `ipairs`, `pairs`, `next`, `select`, `rawget`, `rawset`, `rawequal`, `rawlen`, `setmetatable`, `getmetatable`
- [x] `_VERSION` = `"Lua 5.5"`
- [x] CLI (`main.rs`): `rua script.lua`, `rua -e "code"`, basic REPL
- [x] Integration tests: fibonacci, factorial, table ops, closures, scoping

**Milestone check:** `rua tests/fib.lua` prints correct output.

---

## Phase 2: Complete Language Semantics

Goal: Pass the majority of the Lua test suite (non-coroutine, non-IO parts).

### M2.1 — Metamethods
- [x] Metatable storage on `Table` (and per-type metatables for string)
- [x] Arithmetic metamethods: `__add`, `__sub`, `__mul`, `__div`, `__mod`, `__pow`, `__unm`, `__idiv`
- [x] Bitwise metamethods: `__band`, `__bor`, `__bxor`, `__bnot`, `__shl`, `__shr`
- [x] Comparison: `__eq`, `__lt`, `__le`
- [x] String/length: `__concat`, `__len`
- [x] Table access: `__index` (function or table chain), `__newindex` (function or table chain)
- [x] `__call` metamethod (with 16-level chain limit)
- [x] `__tostring`, `__name`, `__metatable`
- [x] VM fast path: check metatable only on type mismatch / table miss

### M2.2 — Full Operator Semantics
- [x] Integer/float coercion rules (arithmetic, bitwise, comparison)
- [x] String-to-number coercion (string library metamethods)
- [x] Float-to-integer exact conversion checks
- [x] Integer overflow wrapping (two's complement)
- [x] Division/modulo edge cases (floor division semantics)
- [x] Exponentiation always returns float

### M2.3 — Variable Declarations (Lua 5.5)
- [x] `global` name declarations (compiler-level name resolution)
- [x] `global *` / `global<const> *` (implicit global mode tracking per scope)
- [x] `local <const>` (compile-time assignment check)
- [x] `local <close>` → `TBC` instruction, `__close` metamethod on scope exit
- [x] To-be-closed: reverse-order close, error handling during close, interaction with `break`/`goto`/`return`

### M2.4 — Vararg & Multi-return
- [x] `VARARGPREP` / `VARARG` opcodes
- [x] Named vararg table (`... args` syntax)
- [x] Vararg table optimization (avoid table creation when possible)
- [x] Multi-return adjustment in all contexts: assignment, call args, table constructor, return, for-in
- [x] `select('#', ...)` and `select(n, ...)`

### M2.5 — Tail Calls & Goto
- [x] `TAILCALL`: reuse current frame, move args
- [x] Proper tail recursion (no stack growth)
- [x] `goto` / labels: compile to `JMP`
- [x] Scope validation: can't jump into a local's scope, can't shadow label in same block

### M2.6 — String Library
- [x] `string.byte`, `string.char`, `string.len`, `string.sub`, `string.rep`, `string.reverse`
- [x] `string.lower`, `string.upper`
- [x] `string.format` (all specifiers: `d`, `i`, `u`, `f`, `e`, `g`, `x`, `o`, `s`, `c`, `q`, `p`)
- [x] Pattern matching engine: `string.find`, `string.match`, `string.gmatch`, `string.gsub`
  - Character classes (`%a`, `%d`, `%w`, etc.), quantifiers (`*`, `+`, `-`, `?`), captures, `%bxy`, `%f[set]`
- [ ] `string.dump` (serialize `Proto` to binary)
- [ ] `string.pack`, `string.unpack`, `string.packsize`
- [x] String metatable with `__index = string`

### M2.7 — Table Library
- [x] `table.insert`, `table.remove`, `table.sort` (default comparator; custom comparator deferred)
- [x] `table.concat`, `table.move`
- [x] `table.pack`, `table.unpack`
- [x] `table.create` (Lua 5.5: pre-allocate array + hash)

### M2.8 — Math Library
- [x] All math functions mapped to Rust `f64` operations
- [x] `math.random` / `math.randomseed` (xoshiro256**)
- [x] `math.maxinteger`, `math.mininteger`, `math.huge`, `math.pi`
- [x] `math.type`, `math.tointeger`

**Milestone check:** Lua test suite `strings.lua`, `math.lua`, `sort.lua`, `attrib.lua` pass.

---

## Phase 3: Coroutines & Advanced Features

Goal: Full language coverage except FFI. All standard libraries functional.

### M3.1 — Coroutines
- [x] `Thread` struct: own stack, call frames, open upvalues, status
- [x] `coroutine.create` → allocate new `Thread`
- [x] `coroutine.resume` → save current thread state, switch to target thread
- [x] `coroutine.yield` → save target thread state, return to resumer
- [x] Status tracking: suspended / running / normal / dead
- [x] `coroutine.wrap` → returns iterator function
- [x] `coroutine.status`, `coroutine.running`, `coroutine.isyieldable`
- [x] `coroutine.close` → close pending TBC variables, set dead
- [x] Yield across pcall / metamethod boundaries
- [x] Error propagation from coroutine to resumer

### M3.2 — Error Handling (Complete)
- [x] `xpcall` with message handler
- [x] Message handler nesting limit (handler errors return original error)
- [x] Stack traceback generation
- [x] Error position info (source:line in error messages)
- [x] `error(msg, level)` — level-based position annotation
- [x] Nil error object → string conversion

### M3.3 — Weak Tables & Finalizers
- [x] `__mode` = "k" / "v" / "kv" support in GC
- [x] Ephemeron table semantics (key-reachability determines value-reachability)
- [x] Clear dead entries during sweep
- [x] `__gc` finalizer: finalization queue, reverse-order execution
- [x] Resurrection: finalized objects survive one cycle
- [x] Re-marking for finalization
- [x] `collectgarbage` core options (`"collect"`, `"count"`, `"stop"`, `"restart"`, `"isrunning"`, `"step"`)
- [x] Precise GC stack rooting via `Proto::nactvar_at` + `CallFrame.runtime_top`
      (so weak references actually become collectable)

### M3.4 — I/O Library
- [x] `io.open`, `io.close`, `io.read`, `io.write`, `io.lines`
- [x] File handle object with methods: `file:read`, `file:write`, `file:seek`, `file:close`, `file:lines`, `file:flush`, `file:setvbuf`
- [x] `io.stdin`, `io.stdout`, `io.stderr`
- [x] `io.input`, `io.output` (default file handles)
- [x] `io.tmpfile`, `io.type`, `io.popen`
- [x] File handle `__gc` / `__close` metamethods

### M3.5 — OS Library
- [x] `os.clock`, `os.time`, `os.date`, `os.difftime`
- [x] `os.execute`, `os.exit`, `os.getenv`
- [x] `os.remove`, `os.rename`, `os.tmpname`
- [x] `os.setlocale`

### M3.6 — Package / Require
- [x] `require(modname)` — searcher chain
- [x] `package.loaded`, `package.preload`, `package.path`
- [x] `package.searchpath`
- [x] `package.searchers`: preload searcher + Lua file searcher
- [x] No C module loading (by design — no FFI)
- [x] `package.config`
- [x] Companion loaders: `load`, `loadfile`, `dofile` (VM-special so chunks
      get a fresh `_ENV` upvalue)

### M3.7 — UTF-8 Library
- [x] `utf8.char`, `utf8.codepoint`, `utf8.codes`
- [x] `utf8.len`, `utf8.offset`
- [x] `utf8.charpattern`

### M3.8 — Debug Library
- [x] `debug.getinfo` (function info: source, line, name, nparams, etc.)
- [x] `debug.getlocal`, `debug.setlocal`
- [x] `debug.getupvalue`, `debug.setupvalue`
- [x] `debug.getmetatable`, `debug.setmetatable`
- [x] `debug.traceback`
- [x] `debug.sethook`, `debug.gethook` (stubs — no-op / returns nil)
- [x] `debug.upvalueid`, `debug.upvaluejoin`
- [x] `debug.getuservalue`, `debug.setuservalue`
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
- [ ] Constant-operand arithmetic: `ADDK`, `SUBK`, `MULK`, `DIVK`, `IDIVK`, `MODK`, `POWK`
- [ ] Immediate integer arithmetic: `ADDI`
- [ ] Specialized table access: `GETI`, `SETI`, `GETFIELD`, `SETFIELD`
- [ ] Specialized comparisons: `EQK`, `EQI`, `LTI`, `LEI`, `GTI`, `GEI`
- [ ] Compiler: peephole pass or direct emit of specialized opcodes when operands are constants/integers
- [ ] Benchmark before/after to validate each specialization is worth keeping

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
