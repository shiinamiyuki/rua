# Dev Notes



## 2026-04-11: Initial Design Document

- Read and analyzed the complete Lua 5.5 reference manual (4400+ lines).
- Key Lua 5.5 differences from 5.4 identified: `global` keyword (reserved word), read-only for-loop control variables, `global *` / `global<const> *` declarations, `table.create`, `__call` chain limited to 15, named vararg tables (`... args`), `utf8.offset` returns start+end.
- Wrote the first version of `design_notes/rua-design.md` covering:
  - Architecture: Lexer → Parser (AST) → Bytecode Compiler → Register-based VM + GC
  - Value representation: Rust enum (phase 1), NaN-boxing (phase 4)
  - Full bytecode instruction set (~60 opcodes, 32-bit fixed width, 4 formats)
  - Table: hybrid array + open-addressing hash map
  - GC: tri-color mark-sweep with incremental and generational modes
  - Coroutines: separate stack per thread, yield/resume by swapping active thread
  - All standard libraries planned (except C module loading)
  - 4-phase implementation plan: Core MVP → Full semantics → Coroutines → Performance
  - Rust crate layout with ~20 source modules

## 2026-04-11: Roadmap Created

- Created `design_notes/ROADMAP.md` with detailed task-level breakdown.
- 4 phases, 23 milestones, ~150 individual tasks.
- Phase 1 (MVP): 8 milestones — foundation, lexer, parser, compiler, VM, GC, table, stdlib+CLI.
- Phase 2 (Semantics): 8 milestones — metamethods, operators, 5.5 declarations, vararg, tail calls, string/table/math libs.
- Phase 3 (Advanced): 8 milestones — coroutines, error handling, weak tables, IO/OS/package/utf8/debug libs.
- Phase 4 (Performance): 7 milestones — incremental GC, generational GC, instruction specialization, NaN-boxing, benchmarking, polish.

## 2026-04-11: M1.1 — Foundation & Value System

- Implemented the crate skeleton with 15 modules matching the design doc layout:
  - Fully implemented: `value.rs`, `gc.rs`, `string.rs`, `table.rs`, `closure.rs`, `error.rs`
  - Stubs for future milestones: `lexer.rs`, `token.rs`, `parser.rs`, `ast.rs`, `compiler.rs`, `bytecode.rs`, `vm.rs`, `coroutine.rs`, `stdlib/mod.rs`
  - `main.rs` CLI entry point (placeholder)
- **Value enum** (`value.rs`): `Nil`, `Boolean(bool)`, `Integer(i64)`, `Float(f64)`, `Object(GcRef)` with type checking, truthiness, display, and PartialEq (including integer-float cross-comparison)
- **GC system** (`gc.rs`): `GcRef` (NonNull pointer), `GcObject` (header + kind), `Gc` allocator (Vec<Box<GcObject>> for stable heap addresses). No collection yet (M1.6).
- **LuaString** (`string.rs`): Immutable byte buffer with precomputed hash using Lua-style sampling hash (O(1) for long strings). PartialEq compares hash first, then content.
- **String interning**: Short strings (≤40 bytes) automatically interned in `Gc::string_pool` (HashMap<Vec<u8>, GcRef>). Duplicate short strings return the same GcRef for pointer-equality fast path.
- **Table stub** (`table.rs`): Basic struct with array Vec and metatable slot. Full implementation in M1.7.
- **Closure stub** (`closure.rs`): Empty struct placeholder. Full implementation in M1.4.
- **Error stub** (`error.rs`): Simple string-based LuaError. Will be upgraded to Value-based in M1.5.
- All 41 unit tests passing: 11 for string, 12 for gc, 18 for value.
- Key design decisions:
  - `GcRef` uses raw `NonNull<GcObject>` pointers with Box for stable heap addresses
  - `GcObjectKind` enum for type dispatch (String/Table/Closure)
  - Value::PartialEq handles integer↔float comparison and string content comparison for non-interned strings
  - Hash seed is fixed constant for now; randomization deferred to Phase 4


## Session: M1.2 Lexer Implementation (2026-04-11)

### Completed
- Implemented full Lua 5.5 lexer in `src/lexer.rs` (~850 lines) and token types in `src/token.rs` (~200 lines)
- **Token types**: All 23 keywords (incl. `global`), all operators/punctuation, literals (Integer, Float, String, Name), Eof
- **Lexer features**:
  - Hand-written on `&[u8]`, single-pass
  - Short strings with all escape sequences: `\a \b \f \n \r \t \v \\ \' \" \xXX \ddd \u{XXX} \z` and `\<newline>`
  - Long strings `[[...]]` / `[=[...]=]` with proper newline normalization and first-newline stripping
  - Short comments `--` and long comments `--[[...]]` / `--[=[...]=]`
  - Decimal/hex integers, decimal/hex floats with exponents
  - Hex integer overflow wraps (two's complement)
  - Decimal integer overflow promotes to float
  - Source location tracking (line, column)
- **Tests**: 42 unit tests covering all token types, numerals, strings, comments, locations, error cases
- **Integration test** (`tests/lex_upstream.rs`): Successfully lexes all 34 upstream Lua test files

### Design Decisions
- Kept `hexf-parse` out — manual hex float parser handles Lua's lenient hex float syntax (no required exponent, arbitrary precision mantissa)
- `TokenKind::keyword()` static method for keyword lookup
- `TokenKind::describe()` for human-readable token descriptions in error messages
- `LexError` separate from `LuaError` — may unify later
- Numeral underscores supported (Lua 5.5 may allow them in implementation)

## M1.3 — Parser (April 2026)

### What was done
- Implemented complete AST in `src/ast.rs`: `Block`, `Stat` (14 variants), `Expr` (11 variants), `Var`, `FuncBody`, `FuncName`, `AttName`, `Field`, `FunctionCall`, `CallArgs`, `BinOp`/`UnOp` with binding power for Pratt parsing.
- Implemented recursive descent parser in `src/parser.rs` (~600 lines):
  - Pratt parsing for expressions with 12 precedence levels
  - Right-associative `^` and `..` operators
  - All statement types: assignment, function call, do/while/repeat/if/for blocks, function defs, local/global declarations, goto/label/break/return
  - Lua 5.5 features: `global *`, `global <const> *`, `global function`, `... name` vararg syntax, attribute lists `<const>`/`<close>`
  - Suffix expression chain: `.field`, `[index]`, `:method(args)`, `(args)`, `{table}`, `"string"`
  - Table constructors with indexed, named, and positional fields, trailing separators
- Added 30 unit tests covering all statement/expression types, operator precedence, right-associativity, chained calls, etc.
- Created `tests/parse_upstream.rs` integration test that parses all 34 upstream Lua test files without error.

### Test results
- 116 unit tests passing (86 lexer/value/string/gc + 30 parser)
- 2 integration tests passing (lex_upstream + parse_upstream on 34 files)

## 2025-04-11: Bytecode Design Overhaul

Redesigned the bytecode ISA with a simplicity-first approach:

- **Reduced from ~83 opcodes (PUC-Rio style) to 50 opcodes.**
- **3 instruction formats** (ABC, ABx, AsBx) instead of 7. Dropped AX and special formats.
- **Deferred all specialized opcodes to Phase 4** (M4.3): `ADDK`, `ADDI`, `GETI`, `SETI`, `GETFIELD`, `SETFIELD`, `EQK`, `EQI`, `LTI`, `LEI`, `GTI`, `GEI`. These will only be added if benchmarks justify them.
- **Merged TFORCALL + TFORLOOP** into a single `TFORLOOP` opcode for generic for loops.
- **Single RETURN** opcode (no RETURN0/RETURN1) with fast-path branches inside the handler.
- **No RK-encoding** (constant-in-operand bit flag). Constants always loaded into registers first, except `GETTABUP`/`SETTABUP` which embed constant keys for global access frequency.
- Renamed `LOADINT` → `LOADI` for consistency.

Files created/updated:
- `design_notes/bytecode.md` (new): Full opcode reference, encoding, register layout, compilation examples, Phase 4 plan.
- `design_notes/rua-design.md`: Section 6 condensed to summary + reference to bytecode.md. Key decisions table updated.
- `design_notes/ROADMAP.md`: M1.4 updated (3 formats, 50 opcodes, no TFORCALL/RETURN0/RETURN1). M4.3 updated (benchmark-guided specialization).


## M1.4 — Bytecode Compiler (completed)

### Summary
Implemented the full bytecode compiler for M1.4, transforming AST → bytecode `Proto` structs.

### What was built
- **`src/bytecode.rs`** (~400 lines): `OpCode` enum with 50 opcodes, 3 instruction formats (ABC/ABx/AsBx), encoding/decoding functions, `Proto`/`Constant`/`UpvalueDesc`/`LocalVarInfo` structs, bytecode disassembler, 5 unit tests.
- **`src/compiler.rs`** (~2000 lines): Full compiler with `FuncState` per-function state, scope/local tracking, register allocation (locals map 1:1 to registers, temporaries above), constant pool management, upvalue resolution (in_stack + parent chain), `_ENV` as upvalue[0]. Compiles all statement types (assignment, if/while/repeat/for/goto/label/break/return/function-def/local-decl) and all expression types (constants, arithmetic, bitwise, comparison, concatenation, length, logic with short-circuit, unary ops, table constructors, function calls, closures, vararg). 16 unit tests.

### Key design decisions
- Register allocation: locals start at R[0], temporaries allocated above via `free_reg` bump pointer, max 250 per function.
- Comparisons compile to: `CMP invert left right; LOADBOOL dest 1 1; LOADBOOL dest 0 0` (3-instruction boolean materialization).
- Short-circuit `and`/`or` use `TESTSET` with jump to skip the second operand.
- Concat flattening: `a..b..c` compiles all operands contiguously then one `CONCAT` instruction.
- Generic for uses `TFORPREP`/`TFORLOOP` pair with iterator state in 3 hidden registers.
- `SETLIST` uses `FIELDS_PER_FLUSH=50` for large table constructors, with `EXTRAARG` for flush counts > 511.
- Nested function compilation temporarily detaches parent state to resolve upvalues across nesting levels.

### Test results
All 138 tests pass (136 unit + 1 lex upstream + 1 parse upstream). No regressions.


## Session – VM Bug Fixes (FORPREP & Multi-Return)

### Bugs Found & Fixed

1. **FORPREP pre-subtract step** (`vm.rs`): `ForPrep` handler only validated and jumped to `ForLoop` without pre-subtracting step from init. Since `ForLoop` first adds step then checks the range, the first iteration got `init+step` instead of `init`. Fix: added `Self::arith_sub(init, step)` and `set_reg` in `ForPrep` handler before jumping.

2. **Multi-return nil-fill in `compile_local_decl`** (`compiler.rs`): For `local a, b, c = multi()`, the compiler correctly called `compile_expr_multi` which emits `CALL R0, 1, 4` (wanting 3 results). But then the post-loop nil-fill `if nvalues < nnames` ran and emitted `LoadNil` over R1/R2, overwriting the CALL results. Fix: changed condition to `if nvalues == 0 && nnames > 0` — nil-fill only when no value expressions at all, since `compile_expr_multi` already handles remaining slots (both for multi-value and single-value expressions).

### Test Results
- All 152 tests pass (150 unit + 2 integration)
- `test_basic.lua` runs end-to-end correctly: arithmetic, variables, control flow, while loops, numeric for (1..5), functions (factorial, fibonacci), closures, tables, type checking, multi-return, boolean logic

## Session – M1.6 GC Implementation (Mark-and-Sweep)

### Design Decisions
- **Intrusive linked list** instead of `Vec<Box<GcObject>>`: O(1) allocation (prepend to head), O(1) unlinking during sweep (pointer surgery), no Vec realloc spikes, one fewer indirection level. Same approach as PUC-Rio Lua.
- **Stop-the-world mark-and-sweep** with gray worklist (iterative, no recursion) to avoid stack overflow on deep object graphs.
- **String pool swept** after mark phase: `HashMap::retain` removes dead interned strings, so short strings can be collected.
- **GC trigger**: checked before NEWTABLE, CLOSURE, and CONCAT instructions. Fires when `bytes_allocated > gc_threshold`. Threshold = `max(32KB, 2 * bytes_after_last_collection)`.
- **Root gathering**: VM builds root list from stack values, call frame closures/varargs/upvalues, and open upvalues.
- **Ownership**: `Box::new` + `mem::forget` transfers ownership to the intrusive list. Sweep recovers via `Box::from_raw`. `Drop` impl frees all objects on Gc teardown.

### Changes
- `gc.rs`: Complete rewrite — `GcHeader` now has `next: Option<NonNull<GcObject>>`, `Gc` stores `all_objects` head pointer instead of `Vec<Box<GcObject>>`. Added `collect()`, `mark_object()`, `trace_object()`, `sweep()`, `should_collect()`, `object_size()`, `Drop` impl. 6 new GC-specific unit tests.
- `vm.rs`: Added `collect_garbage()` (root gathering), `maybe_collect()` (threshold check), GC trigger points at NEWTABLE, CLOSURE, CONCAT opcodes.
- `tests/test_gc_stress.lua`: Stress test with 10K table allocs, 100 string concats, 1K closure allocs, nested table chains.

### Test Results
- All 158 tests pass (156 unit + 2 integration)
- `test_basic.lua` and `test_gc_stress.lua` both pass end-to-end

## Session: M2.1 + M2.2 — Metamethods & Full Operator Semantics (Final)

### Completed
- Created comprehensive metamethod test suite (`tests/test_metamethods.lua`) covering:
  - `__index` (function, table chain, multi-level inheritance)
  - `__newindex` (function, table delegation)
  - All arithmetic metamethods: `__add`, `__sub`, `__mul`, `__div`, `__mod`, `__pow`, `__unm`, `__idiv`
  - All bitwise metamethods: `__band`, `__bor`, `__bxor`, `__bnot`, `__shl`, `__shr`
  - `__concat`, `__len`, `__call`
  - Comparison metamethods: `__eq`, `__lt`, `__le`
  - `__metatable` protection (getmetatable returns field, setmetatable errors)
  - String-to-number coercion for arithmetic and bitwise
- All 158 existing tests pass + new metamethod test suite passes
- Updated ROADMAP.md: M2.1 and M2.2 marked complete

### Test Coverage Summary
- 156 unit tests (cargo test)
- Integration tests: test_basic.lua, test_gc_stress.lua, test_metamethods.lua

## APPEND HERE

## Session 5 — Phase 1 Completion (pcall + Open-Addressing Hash Table)

### M1.5 — pcall / Error Handling
- `LuaError` now carries `value: Option<Value>` so Lua errors can be any value type (not just strings)
- Added `LuaError::with_value(Value)`, `to_value(&mut Gc) -> Value` methods
- pcall is VM-level (not a native function) — identified by GcRef pointer comparison in the CALL opcode handler
- Refactored `execute()` → `execute_to_depth(min_depth)` to support recursive protected calls
- `handle_pcall()` saves frame depth + open upvalue count, sets up inner call, runs `execute_to_depth(saved_depth)`, catches errors
- `recover_from_error()` unwinds frames and upvalues on error
- Returns `(true, results...)` on success, `(false, err)` on failure
- Tested: basic error, multi-return, arg forwarding, nil call, error(42), nested pcall — all working

### M1.7 — Open-Addressing Hash Table
- Replaced `Vec<(Value, Value)>` linear-scan hash with proper open-addressing hash map
- Power-of-2 sizing with `hash_log2` tracking, linear probing
- `HashEntry { key: Value, val: Value }` stored as `Vec<Option<HashEntry>>`
- Hash function: Fibonacci multiplicative hash for integers, bit reinterpretation for floats, precomputed hash for strings, pointer hash for tables/closures
- Backward-shift deletion (no tombstones needed) — maintains probe chain integrity
- Grows at 75% load factor, initial capacity of 4 (1 << 2) on first insert
- `migrate_hash_to_array()` moves sequential integer keys to array part on extension
- Updated GC tracing in `gc.rs` to iterate `Option<HashEntry>` slots
- All 156 unit tests + integration tests (test_basic.lua, test_gc_stress.lua) pass

### Phase 1 Status
All Phase 1 milestones (M1.1–M1.8) are now **complete**. Ready for Phase 2 (metamethods, full operator semantics, etc.).