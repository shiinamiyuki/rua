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

## APPEND HERE