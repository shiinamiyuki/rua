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

## APPEND HERE