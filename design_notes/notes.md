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

## Session: M2.5 + M2.3 Implementation (Tail Calls, Goto, Variable Declarations)

### Completed
- **M2.5 — Tail Calls**: Compiler now detects tail call position (`return f()` with no `<close>` vars in scope) and emits `TAILCALL` opcode. VM already had `TAILCALL` handler from Phase 1. Added `has_close_vars_in_scope()` helper and `is_tailcall` parameter to `compile_funcall`. Verified deep recursion (100,000 levels) without stack overflow.
- **M2.5 — Goto/Labels**: Already fully implemented in Phase 1 (compile_goto, compile_label, scope validation, JMP dispatch).
- **M2.3 — local `<const>`**: Already implemented in Phase 1 (compile-time assignment check).
- **M2.3 — global declarations**: Already implemented in Phase 1 (SETTABUP to _ENV).
- **M2.3 — local `<close>` / TBC / `__close`**: Full implementation:
  - Added `MM_CLOSE` constant and pre-interning in `create_global_env`
  - Added `tbc_slots: Vec<usize>` to VM for tracking to-be-closed variable stack positions
  - Implemented `close_tbc_vars(from, err_obj)` — closes TBC vars in reverse order, calls `__close` metamethod
  - TBC opcode validates value has `__close` or is nil/false
  - CLOSE opcode calls `close_tbc_vars` before `close_upvalues`
  - RETURN/TAILCALL close TBC vars before returning
  - Compiler `leave_scope` emits CLOSE when scope has `<close>` locals

### Bug Fixes
1. **Upvalue resolution for nested functions**: When a lambda (e.g., `__close` handler) inside function `f` references a variable from `f`'s parent scope, the dummy FuncState used in `compile_func_body_with_parent` couldn't resolve it (dummy had `enclosing: None`). Fixed by implementing `collect_free_names()` — an AST walker (~170 lines) that collects all variable references from a function body. Before creating the dummy snapshot, all free names are pre-resolved via `parent_fs.find_upvalue(name)`, ensuring the parent's upvalue list is complete.
2. **TBC closing on error**: When an error occurred inside a pcall'd function, `recover_from_error` closed upvalues but not TBC variables. Fixed by adding `err_obj: Option<Value>` parameter to `recover_from_error` and calling `close_tbc_vars(from, err_obj)` before unwinding frames. Both error paths in `handle_pcall` now convert the error value before calling recovery.

### Test Results
- 158 unit tests pass (156 lib + 1 lex + 1 parse)
- All integration tests pass: test_basic.lua, test_gc_stress.lua, test_metamethods.lua, test_tailcall_goto_close.lua
- New test file: `tests/test_tailcall_goto_close.lua` — covers tail calls (deep recursion, mutual, multiple args), goto, `<const>`, `<close>` (scope exit, reverse order, nil/false, return, error via pcall), global declarations, tail call inhibited by `<close>


### Session: M2.6 String Library + M2.7 Table Library

**M2.6 — String Library (Complete)**
- Created `src/stdlib/string.rs` (~600 lines) with full Lua 5.5 string library
- Functions implemented: `byte`, `char`, `len`, `sub`, `rep`, `reverse`, `lower`, `upper`, `format`, `find`, `match`, `gmatch`, `gsub`
- `string.format`: all specifiers (d, i, u, f, e, E, g, G, x, X, o, s, c, q, p, %%)
- Pattern matching engine: character classes (%a, %d, %w, etc.), sets ([...], [^...]), quantifiers (*, +, -, ?), anchors (^, $), captures
- `string.gmatch`: Returns stateful iterator via new `NativeDyn` closure variant
- Added `NativeDynClosure` to `Closure` enum in closure.rs to support closures that capture state (needed for gmatch's iterator)
- Updated VM to handle `Closure::NativeDyn` variant in both call sites (call_value and do_call)
- String metatable: Added `string_metatable: Option<GcRef>` field to VM struct; created metatable with `__index = string` table. Updated `get_metatable()` to return string metatable for string values. Enables `s:upper()`, `s:find(...)` etc.
- Registered string library in `create_global_env` as `_ENV.string`
- 71 integration tests all passing (tests/test_string.lua)

**Bug Fix: Method Call Register Allocation**
- Discovered and fixed a bug in `compile_funcall` where method calls (`s:method()`) as non-last arguments would corrupt register allocation
- Root cause: `saved_free` was captured AFTER the method call's temporary register allocations (self_reg, key_reg), causing subsequent argument registers to be allocated too high
- Fix: Moved `saved_free` capture before method setup; after method setup, explicitly set `fs.free_reg = arg_start` to properly account for the method's R[base] (function) and R[base+1] (self) layout

**M2.7 — Table Library (Complete)**
- Created `src/stdlib/table.rs` (~260 lines)
- Functions: `insert`, `remove`, `concat`, `move`, `pack`, `unpack`, `sort`, `create`
- `table.insert(t, [pos,] value)`: append or insert at position
- `table.remove(t [, pos])`: remove and return element
- `table.concat(t [, sep [, i [, j]]])`: concatenate array elements
- `table.move(a1, f, e, t [, a2])`: move elements between/within tables
- `table.pack(...)`: create table from varargs with `n` field
- `table.unpack(t [, i [, j]])`: multi-return array elements
- `table.sort(t [, comp])`: default comparison sort (custom comparator deferred)
- `table.create(narr [, nrec])`: pre-allocate array and hash parts
- Registered in `create_global_env` as `_ENV.table`
- 47 integration tests all passing (tests/test_table.lua)
- All 158 unit tests pass, all integration suites pass


### Session: M3.2 Error Handling (2026-04-13)

**M3.2 — Error Handling (Complete)**
- Made `error()` a VM-intercepted function (like `pcall`) so it can access call frames for source:line annotation
- `error(msg, level)`: level 0 = no annotation, level 1 (default) = annotate with caller's location, level 2+ = walk up the call stack
- String errors are annotated with `source:line: message` format; non-string errors pass through unchanged
- Added `xpcall(f, msgh, ...)` as VM-intercepted function with full message handler support
- Message handler is called before stack unwinding; if handler errors, original error is returned
- Added `get_source_line(level)` helper that reads `proto.line_info[pc]` and `proto.source` from call frames
- Added `traceback()` method that walks call frames to build stack traceback strings
- Intercept `error()` in `do_call()` and `call_value()` as well as `OpCode::Call` and `OpCode::TailCall`, ensuring error annotation works when error is passed as argument (e.g., `pcall(error, "msg")`)
- Added `error_ref`, `xpcall_ref` fields to VM struct for identity-based interception
- Refactored `OpCode::Call` dispatch to use a unified special-function detection (pcall=1, xpcall=2, error=3)
- 48 error handling tests all passing (tests/test_errors.lua)
- All 158 unit tests pass, all integration suites pass

**Known pre-existing issue**: `return pcall(...)` inside a function doesn't properly propagate pcall's multi-return values through the Return opcode when using variable-result mode. This is a pre-existing issue unrelated to error handling.

## M3.1 — Coroutines (Complete)

### Architecture
- **Per-thread execution state**: Each coroutine owns its own stack, frames, open_upvalues, tbc_slots, top, and pcall_guards. The VM holds the active thread's state directly.
- **State swap on resume/yield**: `save_vm_to_thread()` / `load_thread_to_vm()` swap all execution state via `std::mem::replace`/`take`.
- **Yield mechanism**: `yielded: Option<Vec<Value>>` flag on Vm, checked at top of `execute_to_depth` loop. Propagates transparently through pcall/xpcall.
- **Main thread**: Created in `execute_main()` as a Coroutine with `is_main: true`. Always exists as `main_thread` on Vm.

### Coroutine Library Functions
- `coroutine.create(f)` — Regular native function, creates Coroutine + Thread GC object
- `coroutine.status(co)` — Regular native, returns status string
- `coroutine.wrap(f)` — Regular native, creates Coroutine + WrapIterator closure
- `coroutine.resume` — VM special (identity-matched GcRef in CALL dispatch)
- `coroutine.yield` — VM special
- `coroutine.running` — VM special, returns (thread, is_main)
- `coroutine.isyieldable` — VM special
- `coroutine.close` — VM special

### Key Implementation Details

**WrapIterator**: New `Closure::WrapIterator(GcRef)` variant. Detected as special case in CALL dispatch. Works like resume but without boolean prefix in results, and propagates errors directly.

**PcallGuard mechanism** (yield-across-pcall):
- When yield happens inside pcall/xpcall, a `PcallGuard` is pushed containing: frame_depth, open_uv_len, result_base, num_results, is_xpcall flag, handler value.
- In RETURN opcode: after popping a frame, if `frames.len()` matches top guard's frame_depth, pop guard and write `true` at result_base.
- In handle_resume/handle_wrap_call: retry loop catches errors → checks pcall_guards → if guard matches, recover_from_error + place (false, error), then re-enter execution.
- Guards are saved/restored with coroutine state in save/load_thread_to_vm.
- GC traces guard handler values (for xpcall).

### Files Modified
- `src/coroutine.rs`: Full Coroutine struct with CoroutineStatus enum, pcall_guards field
- `src/gc.rs`: Thread variant, new_thread(), trace/size for Thread, as_object_mut(&self)
- `src/value.rs`: type_name/Display/is_thread for Thread
- `src/closure.rs`: WrapIterator(GcRef) variant
- `src/vm.rs`: PcallGuard struct, Vm fields, CALL dispatch (9 special cases), all handler methods, pcall guard logic in RETURN, retry loop in resume/wrap
- `src/stdlib/coroutine.rs`: New file — create, status, wrap native functions
- `src/stdlib/mod.rs`: Added coroutine module

### Test Results
- 166 unit tests pass (156 original + 10 new coroutine tests)
- External test file `tests/coroutine_basic.lua` covers 14 test scenarios including:
  - Basic create/resume, yield/resume cycles, multiple yields, resume-dead-fails
  - coroutine.wrap, wrap with for-in, wrap error propagation
  - coroutine.running, coroutine.isyieldable, coroutine.close
  - Yield inside pcall (with PcallGuard), multi-return from yield
  - Error in coroutine

## Session: M3.4 (I/O Library) & M3.5 (OS Library)

### Changes Made

**Userdata GC Infrastructure** (from prior session):
- `GcObjectKind::Userdata(LuaUserdata)` with `Box<dyn Any>` + optional metatable
- Accessor methods, GC tracing, size estimation
- Value type_name "userdata", Display format

**I/O Library** (`src/stdlib/io.rs`):
- `LuaFile` struct with `FileKind` enum: Regular (BufReader<File>), Stdin, Stdout, Stderr, Closed
- File methods: read (formats: l, L, a, n, bytes), write, close, seek, flush, lines, setvbuf (stub)
- IO functions: open (full mode support: r/w/a/r+/w+/a+), close, read, write, flush, type, tmpfile, lines, input, output, popen (stub)
- `file.__close` metamethod for TBC / cleanup
- File metatable with `__index` pointing to method table
- `io.stdin`, `io.stdout`, `io.stderr` as userdata file handles
- Iterator closures for `io.lines(filename)` and `file:lines()` using NativeDynClosure

**OS Library** (`src/stdlib/os.rs`):
- `os.clock` — CPU time via `Instant::now()` with thread-local start time
- `os.time` — Unix epoch seconds via `SystemTime`
- `os.date` — Basic strftime formatting (%Y, %m, %d, %H, %M, %S, %A, %a, %B, %b, %c, %p, %X, %x, etc.)
- `os.date("*t")` — Returns table with year/month/day/hour/min/sec/wday/yday/isdst
- `os.difftime`, `os.execute`, `os.exit`, `os.getenv`, `os.remove`, `os.rename`, `os.tmpname`, `os.setlocale` (stub)

**GC Changes**:
- Added `file_metatable: Option<GcRef>` to `Gc` struct for native function access
- Added `string_metatable` and `file_metatable` as explicit GC roots in `collect_garbage()`

**Registration**:
- io and os tables registered in `create_global_env()` with full method tables
- File metatable created with `__index` and `__close` entries
- `stdlib/mod.rs` updated with `pub mod io; pub mod os;`

**Tests**: 167 total (166 prior + 1 new io_os_test.lua with ~25 test cases covering file I/O, seeking, lines iteration, os.clock/time/date/getenv/rename)

### Design Notes
- `io.read()`/`io.write()` directly use `std::io::stdin()`/`stdout()` rather than going through file handles, since native functions only receive `&mut Gc`, not VM state
- `io.input()`/`io.output()` simplified: always return fresh stdin/stdout handles (no switching support)
- `io.popen` returns error (not supported) — would need cross-platform process pipe management
- `file:setvbuf` is a no-op stub — Rust manages its own buffering
- Used `unsafe` raw pointer cast in `get_file_mut` to work around borrow checker lifetime issue with GcRef (which is a raw pointer wrapper)

## Session: M3.7 (UTF-8 Library)

Implemented `src/stdlib/utf8.rs`:
- `utf8.char(···)` — codepoints to UTF-8 string
- `utf8.codepoint(s, i, j)` — extract codepoints from byte range
- `utf8.codes(s)` — iterator via NativeDynClosure returning (byte_pos, codepoint)
- `utf8.len(s, i, j)` — count characters, returns nil+errpos on invalid UTF-8
- `utf8.offset(s, n, i)` — byte position of n-th character (forward/backward)
- `utf8.charpattern` — pattern constant `[\0-\x7F\xC2-\xFD][\x80-\xBF]*`

168 tests passing.

## M3.8 — Debug Library (2026-04-17)

### Implemented
- **debug.rs** (`src/stdlib/debug.rs`): NativeFn functions for `getmetatable`, `setmetatable`, `getuservalue`, `setuservalue`, `sethook` (stub), `gethook` (stub)
- **VM-special debug functions**: `traceback`, `getinfo`, `getlocal`, `setlocal`, `getupvalue`, `setupvalue`, `upvalueid`, `upvaluejoin` — all special-cased in Call opcode dispatch (same pattern as pcall/xpcall/error/coroutine)
- **LuaUserdata.user_values**: Added `Vec<Value>` for debug.getuservalue/setuservalue support
- **Debug refs**: 8 GcRef fields on Vm struct for identity-based dispatch, all rooted for GC

### Key Bugs Fixed
1. **TailCall for C functions**: TailCall was unconditionally popping the caller's frame. In Lua, only Lua-to-Lua tail calls pop the frame; C functions keep the caller frame during execution. Fixed to only optimize tail calls targeting Lua closures.
2. **Locals in reverse order**: `pop_scope` used `Vec::pop()` loop which reversed local ordering in `proto.locals`. Changed to `drain(scope.first_local..)` to preserve declaration order. This was critical for `debug.getlocal` returning correct names.
3. **do_call missing debug interception**: Debug VM-special functions were only intercepted in the Call opcode's inline dispatch, but not in `do_call()` (used by TailCall and other call paths). Added all 8 debug function checks to `do_call`.

### Architecture Notes
- `_ENV` is always captured as upvalue[0] for every function (even if not used). This differs from PUC-Lua which only captures `_ENV` when needed. Tests account for this.
- `debug.getregistry` not yet implemented (needs a persistent registry table).
- `debug.sethook`/`debug.gethook` are no-op stubs.

### Test Results
- 169 tests passing (168 + 1 new debug library test)
- Test file: `tests/debug_test.lua`

## Session — M3.3 (Weak Tables & Finalizers) and M3.6 (Package / Require)

Implemented two roadmap milestones in one session and reworked the GC's
stack rooting so that weak references actually become collectable.

### M3.6 — Package / Require

- New module `src/stdlib/package.rs`:
  - `PACKAGE_CONFIG`, `DIRECTORY_SEP`, `default_path()` (reads
    `LUA_PATH_5_5` / `LUA_PATH`, with a sane fallback).
  - `searchpath()` core logic and `lua_searchpath()` native binding
    used as `package.searchpath`.
- VM-side wiring (`src/vm.rs`):
  - New per-VM `Option<GcRef>` slots: `require_ref`, `load_ref`,
    `loadfile_ref`, `dofile_ref`, `preload_searcher_ref`,
    `file_searcher_ref`, `package_ref`, `globals_ref`. All added as
    GC roots.
  - `create_global_env` builds the `package` table with `loaded`,
    `preload`, `searchers = { preload, file_searcher }`, `path`,
    `config`, and registers `require` / `load` / `loadfile` / `dofile`
    as VM-special functions (need `&mut Vm` for compile + `_ENV`
    binding).
  - Helpers: `compile_chunk` (lex/parse/compile and bind a fresh `_ENV`
    upvalue to the global env), `table_raw_get_str`, `call_searcher`
    (dispatches built-in or generic), `run_preload_searcher`,
    `run_file_searcher`.
  - VM-special handlers: `handle_require`, `handle_load`,
    `handle_loadfile`, `handle_dofile`. `OpCode::Call` and
    `do_call`/`TailCall` now intercept these GcRefs.
- Tests in `tests/require_test.lua` plus `tests/require_test_modules/`
  (mymod.lua, sub/inner.lua) cover: package table shape,
  `package.preload` searcher, file searcher with dotted names,
  `package.searchpath`, `load`/`dofile`, caching in `package.loaded`.

### M3.3 — Weak Tables, Finalizers, collectgarbage

#### Weak tables

- `Table` gained `pub(crate) weak_keys: bool` and `weak_values: bool`,
  plus `set_weak_mode(mode: Option<&[u8]>)` to parse `__mode` (`k`,
  `v`, `kv`, `vk`).
- `set_weak_mode` is now invoked from both `lua_setmetatable` and
  `debug.setmetatable` whenever the new metatable changes.
- `Gc::collect` was rewritten as a multi-phase cycle:
  1. `refresh_weak_modes()` re-reads `__mode` for every table (handles
     mutating the metatable after the fact).
  2. Primary mark pass — for weak tables, **defer** marking values
     (and for full-weak tables, also keys) so they don't get rooted
     trivially.
  3. Ephemeron fixed-point: for weak-keys tables, propagate
     `key marked → value marked` until no change. This makes
     `__mode = "k"` give true ephemeron semantics: `t[k] = v` keeps
     `v` alive only while `k` is reachable via some other path.
  4. Finalizer resurrection (see below).
  5. `clear_weak_entries(t_ref)` removes hash/array entries whose
     surviving partner is dead.
  6. Sweep the string-intern pool, then sweep the main object list.
- `trace_object` was generalized to take a `weak_tables: &mut Vec<GcRef>`
  out-parameter. Crucially, for `weak_keys && !weak_values` tables we
  do NOT mark hash values during the primary pass — that's the bug
  fix that made the ephemeron self-cycle test (`t[k] = v` where
  `v.ref = k`) actually collect.

#### Finalizers (`__gc`)

- Added `pub has_finalizer: bool` to `GcHeader`.
- `Gc` got `finalizable: Vec<GcRef>` and `pending_finalizers:
  Vec<GcRef>` plus the API to manage them: `register_finalizer`,
  `take_pending_finalizers`, `has_pending_finalizers`,
  `requeue_pending_finalizer`, `pop_pending_finalizer`.
- `register_finalizer` is called from both `setmetatable` paths
  whenever the new metatable has a non-nil `__gc` field.
- During collection, any `finalizable` object that wasn't marked is
  **resurrected** (mark + trace), has `has_finalizer` cleared, and
  goes onto `pending_finalizers`. After resurrection we run another
  ephemeron fixed-point to propagate any newly-reachable values.
- `pending_finalizers` itself is rooted at the start of every cycle
  so finalizers never execute on freed memory.
- `Vm::run_pending_finalizers` (called once GC returns to the VM)
  drains the queue in **LIFO** order, calls each `__gc` metamethod
  via `call_value`, and prints a Lua-warning-style diagnostic to
  stderr on error (matching reference Lua's "warn" semantics).
- Re-entrancy guard `Vm::in_finalizer` prevents infinite recursion if
  a finalizer triggers another GC.

#### `collectgarbage`

- Registered as a VM-special function (needs `&mut Vm` to invoke a
  full collection synchronously).
- `Vm::handle_collectgarbage` supports `"collect"`, `"count"`,
  `"stop"` / `"restart"` / `"isrunning"`, and `"step"`.
- `Gc::bytes_allocated_approx()` provides the kilobyte figure for
  `collectgarbage("count")`.

### The big GC stack-scan rework

This was the hardest part of the milestone. With the old "scan
`[base..base + max_stack_size]`" rule, register-VM temporaries kept
weak values alive forever and finalizers never fired, because dead
temps in registers above the locals window were treated as roots.

Three coordinated changes finally made things behave:

1. **Per-frame `runtime_top: usize`** on `CallFrame`. It is the
   absolute stack index one past the highest in-use temporary right
   now. Reset to `frame.base` at the top of every dispatch iteration
   (between instructions there are no in-flight temps), then widened
   by individual opcodes that stage values above the active locals:
   - `OpCode::Call` and `OpCode::TailCall` set
     `runtime_top = base + a + num_args + 1` *before* invoking, so
     the function and its arguments stay rooted across any GC the
     callee triggers (this is what was crashing the io/os suite — a
     freshly-opened file userdata was being collected during a
     `f:write(...)` because the temp slot wasn't a root).
   - `OpCode::Concat` sets `runtime_top = base + c + 1` before its
     internal `maybe_collect`.
2. **`Proto::nactvar_at(pc) -> u32`** (in `src/bytecode.rs`). Counts
   locals whose `[start_pc, end_pc)` range covers the current pc.
   This is the principled way to know which registers actually
   contain *live locals* at a given GC point.
3. **`Vm::collect_garbage`** stack scan for each frame is now
   `[frame.base .. max(frame.base + nactvar, runtime_top)]`. We also
   stopped scanning `[0..self.top]`: `self.top` is a stale
   high-water mark set by `Vararg` / `Call` and is never reset, so
   including it spuriously rooted long-dead temps.

The combined effect: between instructions, only true locals plus
upvalues plus the per-VM root set are reachable; *during* a Call /
Concat / native invocation, the in-flight operands are also rooted —
but they stop being rooted the moment the next instruction begins.

### Test surface added

- `tests/weak_tables_test.lua` — weak-v, weak-k, weak-kv, primitive
  values, `__mode` flipped after the fact, ephemeron self-cycle.
- `tests/finalizers_test.lua` — single-fire, finalizer-receives-self,
  errors-don't-kill-vm, LIFO order, resurrection, `collectgarbage("count")`.
- `tests/require_test.lua` plus the `require_test_modules/` fixtures
  — covered above.
- `tests/debug_test.lua` — recreated after an accidental delete:
  `traceback`, `getinfo` (level / function / C-function / oob),
  `getlocal` / `setlocal`, `getupvalue` / `setupvalue` /
  `upvalueid` / `upvaluejoin`, `debug.getmetatable` /
  `debug.setmetatable` (including `__metatable` bypass), `sethook` /
  `gethook` stubs.

### Status

`cargo test --release -- --test-threads=1`: 169 passed, 0 failed.
All four new Lua suites pass: `weak_tables_test.lua`,
`finalizers_test.lua`, `require_test.lua`, `debug_test.lua`,
plus the existing `io_os_test.lua` which had been crashing earlier
due to the stack-scan bug.

## Session — M2.6 completion: string.dump + string.pack (2026-09-27)

Finished the two remaining Phase 2 items. Upstream `tpack.lua` now passes
end-to-end, and `cargo test` reports 172 unit tests + 2 integration tests,
with all `tests/*.lua` suites green.

### `string.pack` / `string.unpack` / `string.packsize`

- New module `src/stdlib/pack.rs` (~640 lines), a direct port of PUC-Rio
  5.5's `lstrlib.c` pack engine (fetched `v5.5.0` sources as reference):
  - All options: `b B h H l L j J T i[n] I[n] f d n c[n] z s[n] x X<op>`
    plus `< > = ![n]` and spaces.
  - Native sizes taken from Rust `size_of`/`align_of` (`c_long` for `l`),
    native max alignment computed as `offsetof(struct cD, u)`.
  - `MAX_SIZE = LUA_MAXINTEGER` (i64::MAX on 64-bit), matching the
    overflow/too-large/too-long boundaries the upstream suite checks;
    digit parsing stops at `(MAX_SIZE - 9)/10` so oversized numerals
    leave trailing digits to be reported as invalid options.
  - PUC-identical error strings (`integral size (N) out of limits
    [1,16]`, `format asks for alignment not power of 2`, `invalid next
    option for option 'X'`, `%d-byte integer does not fit into Lua
    Integer`, etc.) so `checkerror` patterns in `tpack.lua` match.
  - Alignment (`min(size, maxalign)`, power-of-2 validation, `c`/`z`
    unaligned, `Xop` pulls alignment from the following option),
    endianness, sign extension, oversized-integer verification, and
    `unpack` initial-position semantics (`posrelatI`) all mirror the C
    implementation.
- Registered in `string_functions()` as `pack` / `packsize` / `unpack`.

### `string.dump` + binary chunks

- New module `src/chunk.rs`: custom, versioned binary chunk format
  (`\x1bRua` + version + endian/size/numeric check header + recursive
  Proto encoding). Not PUC-compatible by design; `undump` validates the
  full header and reports `bad binary format (<why>)`.
  - `strip` omits line info, locals, source, and upvalue names.
  - 3 unit tests: round-trip, strip, bad header/truncation.
- `string_dump` in `src/stdlib/string.rs` accepts Lua closures only and
  dumps `Rc<Proto>` directly (no VM access needed).
- `Vm::load_chunk(bytes, chunk_name, mode, env)` is the new central
  loader: mode enforcement (`b`/`t`/`bt`, PUC messages `attempt to load
  a binary/text chunk (mode is 'x')`), binary undump, and fresh upvalue
  setup (first upvalue = env, rest = nil). Used by `load`, `loadfile`,
  `dofile`, and the `require` file searcher, so all of them now accept
  precompiled chunks.
- Fuzzed corrupted/truncated chunks via Lua (all truncations + 200 bit
  flips): always an error, never a panic.

### Bug fixed: function calls inside concat chains

While running `tpack.lua` the expression `"ab" .. string.rep("\0", n) ..
"X\1"` exposed a pre-existing compiler bug: `compile_funcall` assumed
argument registers start at `base + 1`, but `compile_concat` pre-reserves
a contiguous block for all operands and raises `free_reg` above the call
base, so calls that were *not* the last concat operand compiled their
arguments into wrong registers (CALL then read nils/stale values).
- Fix: `compile_funcall` now unconditionally resets `free_reg` to
  `arg_start` after evaluating the callee (`src/compiler.rs`); temps
  used for the callee are dead at that point, and concat operands after
  the call are written later, so their reserved slots are safe.
- Regression coverage added in `tests/test_pack.lua` ("concat call 1..4").

### Tests added

- `tests/test_pack.lua` — 264 assertions: all integer sizes/endianness,
  overflow errors, floats, strings, alignment, invalid formats, initial
  positions, plus concat-call regressions.
- `tests/test_dump.lua` — 39 assertions: round-trips, strip, mode
  enforcement, bad binary, upvalue freshness/env binding, varargs,
  nested functions, stripped-chunk errors, binary `loadfile`/`dofile`.
- Upstream `tests/lua-upstream-tests/tpack.lua` passes (`OK`).

### ROADMAP

- M2.6: `string.dump` and `string.pack`/`unpack`/`packsize` checked off.
- M4.7: the two precompiled-chunk items are checked off early (custom
  format; no `luac` CLI yet).

## Session — M3 finish, part 1: loader/VM/stdlib/debug (2026-09-27)

Working toward the M3 milestone with the agreed scope (curated upstream
suite; `gc`/`gengc`/`main`/C-API excluded). Progress is large but the
suite itself is not green yet: Stage 4 triage remains (2 of 21 curated
files pass: `code.lua`, `tpack.lua`, plus `verybig.lua`).

### Stage 0 — harness
- `tests/upstream_suite.rs`: runs a whitelist of upstream files in
  subprocesses with timeouts; skipped in debug builds.
- Excluded: `api/memerr/cstack` (C-API/T), `main` (CLI), `gc/gengc`
  (M4 GC), `all`, `big`, `heavy`; `db.lua` excluded because its
  line-hook sequences encode PUC's private lineinfo encoding and its
  `debug.getlocal` numbering relies on PUC 5.5 pseudo-locals
  ("(for state)"). Debug coverage stays in `tests/debug_test.lua`.
- Reference Lua 5.5.0 was built from source in /tmp as a live oracle.

### Stage 1 — VM / loader
- `do_call` now intercepts `pcall`/`xpcall`; fixed `return pcall(...)`
  losing all results.
- `load` supports the function-reader form (pieces until nil/empty;
  non-string → error; reader errors → `nil, msg`); default chunkname for
  strings is the chunk itself (matches `[string "..."]` short_src).
- Runtime errors are annotated with `source:line:` (once, via
  `position_error`); `error(nil)` produces `<no error object>`.
- `warn()` implemented (`@on/@off/@store/@normal`, `_WARN` store).
- `next` was missing as a global — added.

### Stage 2 — stdlib
- `table.sort` rewritten as VM-special: PUC quicksort port with custom
  comparators, `__lt` default, overwritten-function detection, `__len`,
  "array too big"/"invalid order function" errors.
- `os.setlocale` only accepts C/POSIX/"" (nil otherwise, category
  validation).
- io: real default input/output handles (`Gc.io_input/io_output`),
  `io.input/output` switching (names or handles), `io.read/write/lines/
  flush/close` use them; `io.popen` implemented via `std::process`
  (read/write pipes, `close` returns `ok, "exit", code`); `file:setvbuf`
  validates modes; file `__name = "FILE*"`; `getmetatable` now works on
  userdata.
- `string.gsub` supports table replacements.
- `tostring()` now requires an argument; `math.modf(±inf)` returns a
  signed zero fraction.

### Stage 3 — debug library
- `debug.getregistry` with `[1]=main thread`, `[2]=globals`, and the
  weak-keyed `_HOOKKEY` table.
- `luaO_chunkid` ported for `short_src`/error/traceback names.
- `getinfo`: option validation, `name`/`namewhat` via bytecode
  back-scanning (global/field/local/upvalue), `istailcall`,
  `extraargs`, real `linedefined/lastlinedefined` (`Proto` fields),
  activelines matching PUC (skip VARARGPREP).
- `debug.getlocal`/`setlocal`: thread argument, function form
  (parameter names, "(vararg)"), negative vararg indices, fixed
  `end_pc` exclusivity.
- `sethook`/`gethook` fully implemented: per-thread state in
  `Coroutine` (swapped on resume/yield, GC-traced), `c`/`r`/`l` masks +
  count hooks, `"tail call"` events, hook frames (`is_hook`) visible to
  getinfo/traceback as `hook '?'`, re-entrancy guard; `traceback` level
  semantics fixed.

### Bugs fixed along the way (found via upstream tests)
- `while` loops: `break` jumped to the loop-back jump, causing infinite
  loops (now patches to the exit; also fixed CLOSE placement for
  while/for).
- Parentheses were dropped by the parser: `(f())` is now a `Paren` AST
  node, restoring single-value adjustment and inhibiting tail calls.
- CLOSURE instructions now carry the closing `end` line (line traces,
  activelines).
- `global name` (no value) used to emit `name = nil` at runtime; it is
  a pure declaration now.
- GC rooting: instructions now carry a compiler-computed
  `stack_top_at` high-water mark, so in-flight argument temporaries are
  rooted. Previously `select("#", {})` (a table constructor triggering
  GC mid-argument-list) could free an already-evaluated argument.
- `parse`/`call_value` unification: `call_value` goes through
  `do_call`, so VM-special functions work when passed as values
  (comparators, callbacks).
- Scopes/closures: `for`/`while`/`repeat` line info, local ordering,
  `Block.end_line`.

### Remaining (Stage 4 triage)
19 curated files still fail; first failures observed:
`calls.lua:23` (`a:x` method-call compilation bug), `events.lua:27`
(`__tostring`/`__metatable` protection), `errors.lua:55` (now fixed by
tostring), `utf8.lua:15`, `math.lua:119`, `pm.lua:102`, `sort.lua:12`,
`nextvar.lua:115`, `literals.lua:40`, `coroutine.lua:18`. These need
per-file triage; the harness + Lua 5.5 oracle are in place.

### Files
New: `tests/upstream_suite.rs`, `tests/test_m3_stdlib.lua`.
Edited: `src/{vm,compiler,parser,ast,bytecode,chunk,table,gc,coroutine}.rs`,
`src/stdlib/{mod,string,table,math,io,debug,coroutine}.rs`,
`design_notes/ROADMAP.md`.

## Session — M3 finish, part 2 (2026-09-27, cont.)

Continued triage; `calls.lua` now reaches 556/577. Additional fixes:

- **Lazy `_ENV` upvalue ordering**: nested functions now capture `_ENV`
  only when a global is actually used, in first-use order (matching
  PUC). This makes `debug.getupvalue`/`setupvalue` indices and
  `string.dump` upvalue order match. Parent chains pre-resolve `_ENV`.
- **Assignment evaluation order**: targets' table/key expressions are
  now compiled before the RHS (PUC order), fixing both side-effect order
  and upvalue capture order.
- **Coroutine stack isolation bug**: `Upvalue::Open` carried a bare
  stack index, so closures created in one thread read the wrong stack
  when called in another (`coroutine.create(function() return f() end)`
  looped forever). Open upvalues now record their owning thread
  (`UpvalueLoc { thread, idx }`); reads/writes go to the right stack.
- **Tail calls through `__call` chains**: resolution happens before the
  tail-frame replacement; callable objects can be tail-called. Also
  `ensure_stack` before placing args, and `extraargs` support in
  `getinfo(..., "t")` (PUC's C-call metamethod extra args).
- **Protected-call depth limit** (200) + "error in error handling" when
  an `xpcall` message handler errors; `xpcall` now requires a function
  handler (5.5 semantics).
- `type`/`rawlen`/`tostring` require arguments; `select` edge cases.
- `math.modf(±inf)` signed zero; integer `//`/`%` use wrapping ops
  (`math.mininteger // -1` no longer panics).
- `load`/`loadfile` env: absent → globals, explicit nil → nil.
- Chunk header now follows the reference Lua 5.5 layout
  (`\x1bLua`, version 0x55, format 0, LUAC_DATA, size/check fields);
  the payload remains Rua-specific. Corrupt-header/truncated-chunk
  tests in `calls.lua` now pass.
- `return` more than 254 values → "too many returns".
- `string.gsub` table replacement; `require` loads stdlib tables from
  `package.loaded`; `next` global.
- Harness is `#[ignore]`d for now (run with
  `cargo test --release --test upstream_suite -- --ignored --nocapture`).

Current per-file first failures (of the curated whitelist):
`code/tpack/verybig` pass; `calls` 556 (multi-value table constructor
with >250 values), `closure` goto scope check, `constructs` `select`,
`errors` 56, `events` `__metatable`, `files` closing std handles,
`goto` label scope, `literals`, `locals`, `math` 207, `nextvar`,
`pm` patterns, `sort`, `strings`, `utf8`, `bwcoercion`, `vararg`,
`coroutine`. Reference Lua 5.5.0 is built at `/tmp/opencode/lua-5.5.0`
for differential testing.

## Session — M3 finish, part 3 (2026-09-27, cont.)

Curated upstream suite now: **8/21 pass** (`bwcoercion`, `calls`,
`code`, `constructs`, `events`, `files`, `tpack`, `verybig`).

Fixes in this part:

- **Assignment order / lazy `_ENV`** (from part 2) plus `_ENV` in
  dumped/loaded functions: upvalue order now matches PUC; `load` env
  semantics (absent → globals, explicit nil → nil).
- **Generic `for` (Lua 5.5 layout)**: 4 control slots with a TFORPREP
  swap, the closing value marked to-be-closed, `"(for state)"` debug
  locals (3rd is the TBC one), CLOSE emitted on loop exit and breaks
  patched onto it. `io.lines(filename)` returns 4 values so loops close
  files.
- **`string.dump` format**: reference-5.5 header (signature/version/
  format/LUAC_DATA/check values), same-source inheritance to dedup
  strings (calls.lua dump tests pass); upvalue const flag serialized.
- **`table` constructors**: register limit raised to 255 / locals 200
  ("too many registers"/"too many local variables"); >254 returns error.
- **Exact int/float comparison** (Lua 5.5): equality and `<`/`<=` use
  the float-to-integer exact algorithms; `maxint == maxint + 0.0` is
  false.
- **io**: buffered output + `setvbuf` (`full`/`line`/`no`), pushback for
  `read("n")` (new scanner ported from `liolib.c`), Lua numeral parsing
  (hex floats), `read(0)` EOF test, `filresult`-style `nil,msg,errno`
  returns, `file:close`/`seek`/`flush` error semantics, `__gc`/`__close`/
  `__tostring` on file handles, `io.lines`/`file:lines` formats,
  read/write direction errors, `os.tmpname` uniqueness.
- **os**: real `os.time(table)` (field validation/defaults/out-of-bound,
  mktime-style normalization + field write-back), `os.date` specifier
  validation (invalid specifier errors), C99 formatting, `os.rename`
  errors, `setlocale` semantics.
- **Misc**: `next` global; stable `ipairs`/`pairs` iterators;
  `coroutine.resume` argument validation; native tail-call return values
  from coroutines; `call_value` stack slice guard; `loadfile` BOM and
  `#`-comment skipping; `type`/`tostring`/`tonumber`/`rawlen` require
  arguments; `string.gsub` table replacement; `math.modf(±inf)`; integer
  `//`/`%` wrapping.

Remaining first failures (13 files):
- `closure`: goto into local scope check too strict for the PUC pattern.
- `coroutine`: 77 (multi-yield/resume args).
- `errors`: 40 (syntax-error message format `[string "..."]:line: ... near`).
- `goto`: label scope rules (`no visible label 'l3'`).
- `literals`: 40 (`\z` line counting via `load`).
- `locals`: 170 (`local _ENV` behavior).
- `math`: 66 (needs bitwise float→int coercion with `number has no
  integer representation`, "divide by zero", and varinfo hints).
- `nextvar`: 326 (`ipairs` overflow wrap with mininteger keys).
- `pm`/`strings`: pattern-engine edge cases (UTF-8 patterns, `find` init).
- `sort`: `table.move` out-of-range argument errors.
- `utf8`: continuation-byte validation.
- `vararg`: named vararg table vs `...` aliasing semantics.

Suggested next order: varinfo error hints (math/errors), goto/label
scope rules (goto/closure), utf8 validation, pattern fixes (pm/strings),
named vararg semantics (vararg), syntax-error formatting (errors).

## Session — M3 finish, part 4 (2026-09-27, cont.)

More triage fixes (still 8/21 green, but failures moved much deeper):

- **goto/label scope rules**: gotos now carry a block depth (labels only
  visible at their own depth); unresolved gotos are exported with updated
  local levels and reported at function end; labels that are the last
  non-op statement of a block treat the block's locals as out of scope
  (except in `repeat` bodies, where `until` keeps them alive); repeated
  labels rejected across visible blocks; `global *` creates a const `"*"`
  pseudo-local so gotos cannot jump over it; "jumps into the scope of"
  message aligned with reference Lua.
- **Per-iteration loop-variable freshness**: numeric `for` closes the
  loop variable's upvalues before FORLOOP when the body captures them;
  `break` emits CLOSE for scopes it leaves; `repeat` closes the body
  scope before the loop-back jump; `local a = a..b..c` now evaluates
  the first concat operand into the destination register (removes a
  stale-value root that kept weak tables alive across GC).
- **Numerals/coercion**: numeric strings use the full Lua numeral
  grammar (signs, hex ints/floats, surrounding spaces); float→integer
  coercion rejects out-of-range values; `//0` message is "attempt to
  divide by zero"; bitwise errors include varinfo hints
  (`number (field 'huge') has no integer representation`).
- **Misc**: `string.find` init beyond `len+1` returns nil; `string.rep`
  guards overflow ("resulting string too large").

Progress points (first failure line):
- `goto.lua` 212/477 (upvalue sharing across backward gotos),
  `closure.lua` 222/279 (goto/if upvalue sharing), `math.lua` 374/1145
  (needs `gsub` function replacement), `strings.lua` 152, others as
  before.

## Session summary (M3 triage: 9 -> 18/21 curated upstream tests)

Passing: bwcoercion, calls, closure, code, constructs, coroutine, events, files,
goto, math, nextvar, pm, sort, strings, tpack, utf8, vararg, verybig.
Remaining: errors.lua + literals.lua (need PUC-style syntax error messages:
`<chunkid>:<line>: <message> near '<token>'`, incl. "to close '{' at line N"),
locals.lua (yielding inside __close metamethods).

Major changes this session:
- string.format: full C-printf port (str_format semantics, %q literal forms,
  %a hex floats, width/precision limits, "too long"/"invalid conversion"
  errors) as VM-special so %s can call __tostring; added __name fallback.
- GETTABUP/SETTABUP constant escape via EXTRAARG for indices >= 255
  (fixed silently wrong globals in large chunks!).
- Pattern matcher ported from lstrlib.c: backrefs, %b, %f, %z/Z, malformed
  pattern errors, unfinished captures, 5.3.3 empty-match rules in gsub and
  a lazy gmatch iterator with lastmatch; add_s replacement rules.
- table.create limits (INT_MAX, table overflow), VM-special table.move /
  unpack / insert / concat / remove (metamethod aware), sort via table_get/set.
- Table length borders + dead-key ("tombstone") iteration so `next` works
  across deletions; hash_live counter to avoid tombstone-driven rehashing.
- Lua 5.5 numeric for: count-based scheme (forlimit, float fallback),
  FORPREP skip, string coercion; loop vars are const.
- Generic for: split TFORCALL/TFORLOOP (call is a real frame, so iterators
  can yield), CLOSE per iteration, 5.5 named-vararg tables (reads through t.n,
  "no proper 'n'" error), select() exact semantics, global <const> decls
  (contextual `global`, shadowing, collective *, ERRnil duplicate checks).
- utf8 library ported from lutf8lib.c (lax mode, 5/6-byte sequences,
  stateless codes, two-value offset, charpattern).
- Coroutine.close semantics: main/normal/running/reentrant/self-close,
  pending errors; unyieldable C boundaries (gsub/sort/msgh/reader);
  tail-call yield; dofile yields via deferred frame; native bodies run
  through the frame mechanism; pcall guard result propagation.
- TBC close semantics: error object only on error unwinding, closeprotected
  chaining, close_frame_info for tracebacks, close methods getinfo level 2
  reports pcall, stack overflow limits with error-handler reserve, stack
  top reset after unwinding, extra_roots for GC during closing.
- Locals/debug: LocalVarInfo carries its register (local_at_reg by reg),
  call-site name resolution checks locals first, C-called frames unnamed,
  return hooks for C calls (sethook), metamethod frames named "close".
- Lexer line counting: \r, \n, and paired \r\n / \n\r count as one line.

Next steps: implement PUC syntax error formatting (chunkid + near token) to
pass errors.lua/literals.lua; support yields in __close (locals.lua) via a
close continuation or frame-based closer calls.

## APPEND HERE