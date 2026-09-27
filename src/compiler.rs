//! AST → Bytecode compiler.
//!
//! Compiles AST nodes into bytecode (Proto). Each function (including the
//! top-level chunk) gets its own Proto. The compiler tracks scopes, locals,
//! upvalues, and registers.

use crate::ast::*;
use crate::bytecode::*;
use crate::error::LuaError;

// ── Compiler public API ────────────────────────────────────────────

/// Compile a parsed block (chunk) into a top-level Proto.
///
/// The chunk is treated as a variadic function with `_ENV` as upvalue[0].
pub fn compile(block: &Block, source: Option<String>) -> Result<Proto, LuaError> {
    let mut fs = FuncState::new(source.clone(), None);
    // Top-level chunk is variadic
    fs.proto.is_vararg = true;
    // _ENV is upvalue[0], not in stack (for the outermost chunk)
    fs.proto.upvalues.push(UpvalueDesc {
        name: Some("_ENV".to_string()),
        in_stack: true,
        index: 0,
        is_const: false,
    });
    // Emit VARARGPREP for the top-level chunk (0 fixed params). Line 0
    // means "no line information" (it must not produce a line event).
    fs.emit_abc(OpCode::VarArgPrep, 0, 0, 0, 0);

    compile_block(&mut fs, block)?;
    fs.check_pending_gotos()?;

    // Ensure the function ends with RETURN
    let last_line = block.end_line;
    fs.emit_abc(OpCode::Return, 0, 1, 0, last_line);
    fs.proto.last_line_defined = last_line;

    Ok(fs.finish())
}

// ── Compiler internal state ────────────────────────────────────────

/// Tracks a local variable in the current function.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    /// Register index where this local lives.
    reg: u8,
    /// PC at which this local becomes active.
    start_pc: u32,
    /// Is this local a `<const>` local?
    is_const: bool,
    /// Is this local a `<close>` (to-be-closed) local?
    #[allow(dead_code)]
    is_close: bool,
}

/// A pending goto that needs to be patched.
#[derive(Debug, Clone)]
struct PendingGoto {
    name: String,
    /// Index into the code array of the JMP instruction.
    patch_pc: usize,
    /// Index into the code array of the CLOSE instruction emitted just
    /// before the JMP (patched when the goto leaves a local's scope).
    close_pc: usize,
    /// Number of locals active at the goto site.
    num_locals: usize,
    /// Block depth where the goto appears (0 = function body).
    depth: usize,
    /// Line of the goto for error messages.
    line: u32,
}

/// A label that has been defined.
#[derive(Debug, Clone)]
struct Label {
    name: String,
    /// PC of the instruction after the label.
    pc: usize,
    /// Number of locals active at the label.
    #[allow(dead_code)]
    num_locals: usize,
}

/// Scope information for block tracking.
#[derive(Debug)]
struct BlockScope {
    /// Number of locals before this block started.
    first_local: usize,
    /// Index into pending_gotos at block entry.
    first_goto: usize,
    /// Number of labels before this block started.
    first_label: usize,
    /// Whether any upvalues reference locals in this block.
    has_upvalues: bool,
    /// Whether this block is a loop body (for break).
    is_loop: bool,
    /// Break jump patches: indices into code array of JMP instructions.
    break_jumps: Vec<usize>,
}

/// State for compiling a single function.
struct FuncState {
    proto: Proto,
    /// All locals in the current function (across all scopes).
    locals: Vec<Local>,
    /// Active scope stack.
    scopes: Vec<BlockScope>,
    /// Next available register.
    free_reg: u8,
    /// Const global declarations visible in the current scope:
    /// (scope depth, name).
    const_globals: Vec<(usize, String)>,
    /// Pending goto statements that haven't been resolved yet.
    pending_gotos: Vec<PendingGoto>,
    /// Labels defined in the function.
    labels: Vec<Label>,
    /// The enclosing function (for upvalue resolution).
    enclosing: Option<Box<FuncState>>,
    /// Current line number for debug info.
    current_line: u32,
}

impl FuncState {
    fn new(source: Option<String>, enclosing: Option<Box<FuncState>>) -> Self {
        FuncState {
            proto: Proto::new(source),
            locals: Vec::new(),
            scopes: Vec::new(),
            free_reg: 0,
            const_globals: Vec::new(),
            pending_gotos: Vec::new(),
            labels: Vec::new(),
            enclosing,
            current_line: 1,
        }
    }

    fn finish(mut self) -> Proto {
        // Patch end_pc for all remaining locals. Sorting by start_pc keeps
        // `proto.locals` in declaration order even when locals from nested
        // scopes were recorded earlier (on scope exit).
        let end_pc = self.proto.code.len() as u32;
        for local in &self.locals {
            self.proto.locals.push(LocalVarInfo {
                name: local.name.clone(),
                start_pc: local.start_pc,
                end_pc,
            });
        }
        self.proto.locals.sort_by_key(|l| l.start_pc);
        self.proto.max_stack_size = self.proto.max_stack_size.max(self.free_reg).max(2);
        self.proto
    }

    // ── Instruction emission ───────────────────────────────────────

    fn emit(&mut self, inst: u32, line: u32) -> usize {
        let pc = self.proto.code.len();
        self.proto.code.push(inst);
        self.proto.line_info.push(line);
        // Record the current temporary high-water mark so the GC can root
        // in-flight operands if this instruction triggers a collection.
        self.proto.stack_top_at.push(self.free_reg);
        pc
    }

    fn emit_abc(&mut self, op: OpCode, a: u8, b: u8, c: u8, line: u32) -> usize {
        self.emit(encode_abc(op, a, b, c), line)
    }

    fn emit_abx(&mut self, op: OpCode, a: u8, bx: u16, line: u32) -> usize {
        self.emit(encode_abx(op, a, bx), line)
    }

    /// Emit `GETTABUP`, escaping to an EXTRAARG for constant indices that
    /// do not fit in the 8-bit C field.
    fn emit_gettabup(&mut self, dest: u8, env: u8, k: u16, line: u32) -> usize {
        if (k as usize) < 255 {
            self.emit_abc(OpCode::GetTabUp, dest, env, k as u8, line)
        } else {
            self.emit_abc(OpCode::GetTabUp, dest, env, 255, line);
            self.emit(encode_extraarg(k as u32), line)
        }
    }

    /// Emit `SETTABUP`, escaping to an EXTRAARG for constant indices that
    /// do not fit in the 8-bit B field.
    fn emit_settabup(&mut self, env: u8, k: u16, src: u8, line: u32) -> usize {
        if (k as usize) < 255 {
            self.emit_abc(OpCode::SetTabUp, env, k as u8, src, line)
        } else {
            self.emit_abc(OpCode::SetTabUp, env, 255, src, line);
            self.emit(encode_extraarg(k as u32), line)
        }
    }

    fn emit_asbx(&mut self, op: OpCode, a: u8, sbx: i16, line: u32) -> usize {
        self.emit(encode_asbx(op, a, sbx), line)
    }

    fn emit_jmp(&mut self, line: u32) -> usize {
        // Emit a placeholder jump (offset 0), to be patched later
        self.emit_asbx(OpCode::Jmp, 0, 0, line)
    }

    /// Patch a jump instruction at `pc` to jump to `target`.
    fn patch_jmp(&mut self, pc: usize, target: usize) {
        let offset = target as i32 - (pc as i32 + 1);
        let inst = encode_asbx(OpCode::Jmp, 0, offset as i16);
        self.proto.code[pc] = inst;
    }

    /// Patch any AsBx instruction at `pc` to have the given sBx offset.
    fn patch_sbx(&mut self, pc: usize, target: usize) {
        let offset = target as i32 - (pc as i32 + 1);
        let old = self.proto.code[pc];
        let op = decode_op(old);
        let a = decode_a(old);
        self.proto.code[pc] = encode_asbx(
            OpCode::from_u8(op).unwrap(),
            a,
            offset as i16,
        );
    }

    fn current_pc(&self) -> usize {
        self.proto.code.len()
    }

    fn last_line(&self) -> u32 {
        self.proto.line_info.last().copied().unwrap_or(1)
    }

    // ── Register allocation ────────────────────────────────────────

    fn alloc_reg(&mut self) -> Result<u8, LuaError> {
        if self.free_reg >= 255 {
            return Err(LuaError::new("too many registers"));
        }
        let reg = self.free_reg;
        self.free_reg += 1;
        if self.free_reg > self.proto.max_stack_size {
            self.proto.max_stack_size = self.free_reg;
        }
        Ok(reg)
    }

    fn alloc_regs(&mut self, n: u8) -> Result<u8, LuaError> {
        let base = self.free_reg;
        for _ in 0..n {
            self.alloc_reg()?;
        }
        Ok(base)
    }

    fn free_reg_to(&mut self, reg: u8) {
        debug_assert!(reg <= self.free_reg);
        self.free_reg = reg;
    }

    // ── Constant pool ──────────────────────────────────────────────

    fn add_constant(&mut self, k: Constant) -> u16 {
        // Check if constant already exists
        for (i, existing) in self.proto.constants.iter().enumerate() {
            if *existing == k {
                return i as u16;
            }
        }
        let idx = self.proto.constants.len();
        self.proto.constants.push(k);
        idx as u16
    }

    fn string_constant(&mut self, s: &[u8]) -> u16 {
        self.add_constant(Constant::String(s.to_vec()))
    }

    // ── Scope management ───────────────────────────────────────────

    fn enter_scope(&mut self, is_loop: bool) {
        self.scopes.push(BlockScope {
            first_local: self.locals.len(),
            first_goto: self.pending_gotos.len(),
            first_label: self.labels.len(),
            has_upvalues: false,
            is_loop,
            break_jumps: Vec::new(),
        });
    }

    fn leave_scope(&mut self, line: u32) -> Result<(), LuaError> {
        let breaks = self.leave_scope_unpatched(line)?;
        let target = self.current_pc();
        for pc in breaks {
            self.patch_jmp(pc, target);
        }
        Ok(())
    }

    /// Pop the current scope, emit its CLOSE if needed, resolve its gotos,
    /// and return the list of break jumps to be patched by the caller.
    fn leave_scope_unpatched(&mut self, line: u32) -> Result<Vec<usize>, LuaError> {
        let scope = self.scopes.pop().expect("unbalanced scopes");
        let depth = self.scopes.len() + 1;
        self.const_globals.retain(|(d, _)| *d < depth);

        // Check if any locals in this scope are <close> or capture upvalues
        let has_close = self.locals[scope.first_local..]
            .iter()
            .any(|l| l.is_close);

        // Emit CLOSE if there are upvalues or to-be-closed vars in this scope
        if scope.has_upvalues || has_close {
            let first_reg = if scope.first_local < self.locals.len() {
                self.locals[scope.first_local].reg
            } else {
                self.free_reg
            };
            self.emit_abc(OpCode::Close, first_reg, 0, 0, line);
        }

        // Break jumps are returned to the caller for patching.

        // Export unresolved gotos to the enclosing block, updating their
        // local level (locals of this block are now out of scope). The
        // recorded index can be stale after gotos were resolved elsewhere,
        // so clamp it defensively.
        let start = scope.first_goto.min(self.pending_gotos.len());
        let remaining_gotos: Vec<_> = self.pending_gotos.drain(start..).collect();
        for mut goto in remaining_gotos {
            let mut resolved = false;
            for label in &self.labels {
                if label.name == goto.name {
                    resolved = true;
                    break;
                }
            }
            if !resolved {
                goto.num_locals = goto.num_locals.min(scope.first_local);
                goto.depth = goto.depth.saturating_sub(1);
                self.pending_gotos.push(goto);
            }
        }

        // Remove labels from this scope
        self.labels.truncate(scope.first_label);

        // Remove locals from this scope (drain preserves declaration order)
        let end_pc = self.proto.code.len() as u32;
        for local in self.locals.drain(scope.first_local..) {
            self.proto.locals.push(LocalVarInfo {
                name: local.name.clone(),
                start_pc: local.start_pc,
                end_pc,
            });
        }
        if scope.first_local < self.locals.len() + 1 {
            let first_reg = if !self.locals.is_empty() {
                self.locals.last().unwrap().reg + 1
            } else {
                0
            };
            self.free_reg_to(first_reg);
        } else {
            let first_reg = self.locals.len() as u8;
            self.free_reg_to(first_reg);
        }

        Ok(scope.break_jumps)
    }

    /// Error if any goto could not be resolved within the function.
    fn check_pending_gotos(&self) -> Result<(), LuaError> {
        if let Some(goto) = self.pending_gotos.first() {
            return Err(LuaError::new(format!(
                "no visible label '{}' for goto at line {}",
                goto.name, goto.line
            )));
        }
        Ok(())
    }

    fn add_local(&mut self, name: String) -> Result<u8, LuaError> {
        if self.locals.len() >= 200 {
            return Err(LuaError::new("too many local variables"));
        }
        let reg = self.alloc_reg()?;
        self.locals.push(Local {
            name,
            reg,
            start_pc: self.proto.code.len() as u32,
            is_const: false,
            is_close: false,
        });
        Ok(reg)
    }

    fn find_local(&self, name: &str) -> Option<u8> {
        for local in self.locals.iter().rev() {
            if local.name == name {
                return Some(local.reg);
            }
        }
        None
    }

    // ── Upvalue resolution ─────────────────────────────────────────

    fn find_upvalue(&mut self, name: &str) -> Option<u8> {
        // First, check if we already have this upvalue
        for (i, uv) in self.proto.upvalues.iter().enumerate() {
            if uv.name.as_deref() == Some(name) {
                return Some(i as u8);
            }
        }

        // Try to find in enclosing function
        if let Some(ref mut enclosing) = self.enclosing {
            // Check enclosing locals
            if let Some(reg) = enclosing.find_local(name) {
                // Mark the enclosing scope as having upvalues
                for scope in enclosing.scopes.iter_mut().rev() {
                    if scope.first_local <= enclosing.locals.len() {
                        for local in &enclosing.locals[scope.first_local..] {
                            if local.name == name {
                                scope.has_upvalues = true;
                                break;
                            }
                        }
                    }
                }
                let mut captured_const = false;
                for local in enclosing.locals.iter().rev() {
                    if local.name == name {
                        captured_const = local.is_const;
                        break;
                    }
                }
                let idx = self.proto.upvalues.len() as u8;
                self.proto.upvalues.push(UpvalueDesc {
                    name: Some(name.to_string()),
                    in_stack: true,
                    index: reg,
                    is_const: captured_const,
                });
                return Some(idx);
            }

            // Check enclosing upvalues (recursive)
            if let Some(uv_idx) = enclosing.find_upvalue(name) {
                let captured_const = enclosing
                    .proto
                    .upvalues
                    .get(uv_idx as usize)
                    .map(|u| u.is_const)
                    .unwrap_or(false);
                let idx = self.proto.upvalues.len() as u8;
                self.proto.upvalues.push(UpvalueDesc {
                    name: Some(name.to_string()),
                    in_stack: false,
                    index: uv_idx,
                    is_const: captured_const,
                });
                return Some(idx);
            }
        }

        None
    }

    // ── Break scope lookup ─────────────────────────────────────────

    fn find_loop_scope(&mut self) -> Option<usize> {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].is_loop {
                return Some(i);
            }
        }
        None
    }

    /// Check if there are any `<close>` locals in any active scope.
    fn has_close_vars_in_scope(&self) -> bool {
        for local in &self.locals {
            if local.is_close {
                return true;
            }
        }
        false
    }
}

// ── Expression result descriptor ───────────────────────────────────

// ExprResult may be used in future optimization passes
#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum ExprResult {
    /// Value is in register `reg`.
    Reg(u8),
    /// Value is a constant (can be loaded with LOADK).
    Const(u16),
    /// Value is nil.
    Nil,
    /// Value is a boolean.
    Bool(bool),
    /// Value is an integer that fits in sBx.
    SmallInt(i16),
    /// Value is from a function call or vararg (multi-value, top-of-stack).
    /// The base register where results start.
    Call(u8),
}

// ── Block & statement compilation ──────────────────────────────────

fn compile_block(fs: &mut FuncState, block: &Block) -> Result<(), LuaError> {
    compile_block_with(fs, block, true)
}

/// `labels_can_be_last` is false for `repeat` bodies, where a label before
/// `until` still has the body's locals in scope (reference Lua's
/// `block_follow` without `until`).
fn compile_block_with(
    fs: &mut FuncState,
    block: &Block,
    labels_can_be_last: bool,
) -> Result<(), LuaError> {
    for (i, stat) in block.stmts.iter().enumerate() {
        if let StatKind::Label(name) = &stat.node {
            // Reference Lua skips following no-op statements (labels and
            // empty statements) when deciding whether a label is last.
            let is_last = labels_can_be_last
                && block.ret.is_none()
                && block.stmts[i + 1..]
                    .iter()
                    .all(|st| matches!(st.node, StatKind::Label(_) | StatKind::Empty));
            compile_label(fs, name, is_last)?;
        } else {
            compile_stat(fs, stat)?;
        }
    }
    if let Some(ret) = &block.ret {
        compile_return(fs, ret)?;
    }
    Ok(())
}

fn compile_stat(fs: &mut FuncState, stat: &Stat) -> Result<(), LuaError> {
    let line = stat.location.line;
    fs.current_line = line;

    match &stat.node {
        StatKind::Empty => {}
        StatKind::Assign { targets, values } => {
            compile_assign(fs, targets, values, line)?;
        }
        StatKind::FunctionCall(call) => {
            compile_funcall_stat(fs, call, line)?;
        }
        StatKind::DoBlock(block) => {
            fs.enter_scope(false);
            compile_block(fs, block)?;
            fs.leave_scope(line)?;
        }
        StatKind::While { cond, body } => {
            compile_while(fs, cond, body, line)?;
        }
        StatKind::Repeat { body, cond } => {
            compile_repeat(fs, body, cond, line)?;
        }
        StatKind::If {
            cond,
            then_block,
            elseif_clauses,
            else_block,
        } => {
            compile_if(fs, cond, then_block, elseif_clauses, else_block, line)?;
        }
        StatKind::NumericFor {
            name,
            init,
            limit,
            step,
            body,
        } => {
            compile_numeric_for(fs, name, init, limit, step.as_ref(), body, line)?;
        }
        StatKind::GenericFor {
            names,
            iterators,
            body,
        } => {
            compile_generic_for(fs, names, iterators, body, line)?;
        }
        StatKind::Goto(label) => {
            compile_goto(fs, label, line)?;
        }
        StatKind::Label(label) => {
            compile_label(fs, label, false)?;
        }
        StatKind::Break => {
            compile_break(fs, line)?;
        }
        StatKind::FuncDef { name, body } => {
            compile_func_def(fs, name, body, line)?;
        }
        StatKind::LocalFuncDef { name, body } => {
            compile_local_func_def(fs, name, body, line)?;
        }
        StatKind::GlobalFuncDef { name, body } => {
            compile_global_func_def(fs, name, body, line)?;
        }
        StatKind::LocalDecl { names, values } => {
            compile_local_decl(fs, names, values, line)?;
        }
        StatKind::GlobalDecl { names, values } => {
            compile_global_decl(fs, names, values, line)?;
        }
        StatKind::GlobalStar { .. } => {
            // `global *` introduces a pseudo-variable "*" so that gotos
            // cannot jump over it (reference Lua behavior).
            let reg = fs.alloc_reg()?;
            fs.locals.push(Local {
                name: "*".to_string(),
                reg,
                start_pc: fs.proto.code.len() as u32,
                is_const: true,
                is_close: false,
            });
        }
    }
    Ok(())
}

// ── Assignment ─────────────────────────────────────────────────────

fn compile_assign(
    fs: &mut FuncState,
    targets: &[Var],
    values: &[Expr],
    line: u32,
) -> Result<(), LuaError> {
    let ntargets = targets.len();
    let nvalues = values.len();

    // Reference Lua evaluates the target expressions (and resolves the
    // names they use) before the right-hand side. This matters for both
    // side effects and the order in which upvalues are captured.
    enum Prepared {
        Name {
            local_reg: Option<u8>,
            upval: Option<u8>,
            env: Option<u8>,
            key: Option<u16>,
        },
        Table {
            tab: u8,
            key: u8,
        },
    }

    let base = fs.free_reg;
    let mut prepared: Vec<Prepared> = Vec::with_capacity(ntargets);
    for target in targets {
        match target {
            Var::Name(name) => {
                if let Some(local_reg) = fs.find_local(name) {
                    // Check const
                    for local in fs.locals.iter().rev() {
                        if local.name == *name {
                            if local.is_const {
                                return Err(LuaError::new(format!(
                                    "{}:{}: attempt to assign to const variable '{name}'",
                                    fs.proto.source.as_deref().unwrap_or("?"),
                                    line
                                )));
                            }
                            break;
                        }
                    }
                    prepared.push(Prepared::Name {
                        local_reg: Some(local_reg),
                        upval: None,
                        env: None,
                        key: None,
                    });
                } else if let Some(uv) = fs.find_upvalue(name) {
                    if fs
                        .proto
                        .upvalues
                        .get(uv as usize)
                        .map(|u| u.is_const)
                        .unwrap_or(false)
                    {
                        return Err(LuaError::new(format!(
                            "{}:{}: attempt to assign to const variable '{name}'",
                            fs.proto.source.as_deref().unwrap_or("?"),
                            line
                        )));
                    }
                    prepared.push(Prepared::Name {
                        local_reg: None,
                        upval: Some(uv),
                        env: None,
                        key: None,
                    });
                } else {
                    if fs
                        .const_globals
                        .iter()
                        .rev()
                        .any(|(_, n)| n == name)
                    {
                        return Err(LuaError::new(format!(
                            "{}:{}: attempt to assign to const variable '{name}'",
                            fs.proto.source.as_deref().unwrap_or("?"),
                            line
                        )));
                    }
                    let env = env_upvalue(fs)?;
                    let k = fs.string_constant(name.as_bytes());
                    prepared.push(Prepared::Name {
                        local_reg: None,
                        upval: None,
                        env: Some(env),
                        key: Some(k),
                    });
                }
            }
            Var::Index { table, key } => {
                let tab = fs.alloc_reg()?;
                compile_expr_to_reg(fs, table, tab)?;
                let key_reg = fs.alloc_reg()?;
                compile_expr_to_reg(fs, key, key_reg)?;
                prepared.push(Prepared::Table { tab, key: key_reg });
            }
            Var::Field { table, name } => {
                let tab = fs.alloc_reg()?;
                compile_expr_to_reg(fs, table, tab)?;
                let key_reg = fs.alloc_reg()?;
                let k = fs.string_constant(name.as_bytes());
                fs.emit_abx(OpCode::LoadK, key_reg, k, line);
                prepared.push(Prepared::Table { tab, key: key_reg });
            }
        }
    }

    // Evaluate all values into consecutive temp registers.
    let temps_base = fs.free_reg;
    for (i, val) in values.iter().enumerate() {
        let is_last = i == nvalues - 1;
        if is_last && ntargets > nvalues {
            // Last value in a multi-value context: may produce multiple results
            let reg = fs.alloc_reg()?;
            compile_expr_multi(fs, val, reg, (ntargets - i) as u8)?;
            // Allocate remaining registers for the extra values
            for _ in 1..(ntargets - i) {
                fs.alloc_reg()?;
            }
        } else {
            let reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, val, reg)?;
        }
    }

    // Fill remaining targets with nil if needed
    if nvalues < ntargets && nvalues == 0 {
        let reg = fs.alloc_regs(ntargets as u8)?;
        fs.emit_abc(OpCode::LoadNil, reg, (ntargets - 1) as u8, 0, line);
    }

    // Assign from temps to targets.
    for (i, prep) in prepared.iter().enumerate() {
        let src_reg = temps_base + i as u8;
        match prep {
            Prepared::Name {
                local_reg,
                upval,
                env,
                key,
            } => {
                if let Some(local_reg) = local_reg {
                    if *local_reg != src_reg {
                        fs.emit_abc(OpCode::Move, *local_reg, src_reg, 0, line);
                    }
                } else if let Some(uv) = upval {
                    fs.emit_abc(OpCode::SetUpval, src_reg, *uv, 0, line);
                } else {
                    fs.emit_settabup(env.unwrap(), key.unwrap(), src_reg, line);
                }
            }
            Prepared::Table { tab, key } => {
                fs.emit_abc(OpCode::SetTable, *tab, *key, src_reg, line);
            }
        }
    }

    fs.free_reg_to(base);
    Ok(())
}

// ── Function call as statement ─────────────────────────────────────

fn compile_funcall_stat(
    fs: &mut FuncState,
    call: &FunctionCall,
    line: u32,
) -> Result<(), LuaError> {
    let base = fs.free_reg;
    let func_reg = fs.alloc_reg()?;
    compile_funcall(fs, call, func_reg, 1, false, line)?; // C=1 means discard results
    fs.free_reg_to(base);
    Ok(())
}

// ── Control flow ───────────────────────────────────────────────────

fn compile_while(
    fs: &mut FuncState,
    cond: &Expr,
    body: &Block,
    line: u32,
) -> Result<(), LuaError> {
    let loop_start = fs.current_pc();

    // Evaluate condition
    let base = fs.free_reg;
    let cond_reg = fs.alloc_reg()?;
    compile_expr_to_reg(fs, cond, cond_reg)?;
    let cond_line = fs.last_line();
    // TEST cond_reg, 0 — skip next if falsy
    fs.emit_abc(OpCode::Test, cond_reg, 0, 0, cond_line);
    let exit_jmp = fs.emit_jmp(cond_line);
    fs.free_reg_to(base);

    // Body
    fs.enter_scope(true);
    compile_block(fs, body)?;
    // Close the scope before looping back; break jumps must land *after*
    // the back-jump, so patch them explicitly below.
    let breaks = fs.leave_scope_unpatched(line)?;

    // Jump back to condition. The jump carries the line of the last body
    // instruction (no line event); the line event is produced when the
    // backward jump reaches the condition.
    let back_line = fs.last_line();
    let loop_jmp = fs.emit_jmp(back_line);
    fs.patch_sbx(loop_jmp, loop_start.wrapping_sub(1)); // -1 because PC is post-increment
    // Patch to jump back: the offset should be loop_start - (loop_jmp + 1)
    let offset = loop_start as i32 - (loop_jmp as i32 + 1);
    fs.proto.code[loop_jmp] = encode_asbx(OpCode::Jmp, 0, offset as i16);

    // Patch exit jump and all breaks to the loop exit.
    let exit = fs.current_pc();
    fs.patch_jmp(exit_jmp, exit);
    for pc in breaks {
        fs.patch_jmp(pc, exit);
    }

    Ok(())
}

fn compile_repeat(
    fs: &mut FuncState,
    body: &Block,
    cond: &Expr,
    line: u32,
) -> Result<(), LuaError> {
    let loop_start = fs.current_pc();

    fs.enter_scope(true);
    compile_block_with(fs, body, false)?;

    // Evaluate condition (still inside the body's scope).
    let base = fs.free_reg;
    let cond_reg = fs.alloc_reg()?;
    compile_expr_to_reg(fs, cond, cond_reg)?;

    // Close the body scope before the loop-back jump so locals captured by
    // closures are fresh each iteration. `breaks` must land after the jump.
    let breaks = fs.leave_scope_unpatched(line)?;

    let cond_line = fs.last_line();
    // TEST cond_reg, 0 — skip if falsy (i.e., repeat body)
    fs.emit_abc(OpCode::Test, cond_reg, 0, 0, cond_line);
    // If falsy, jump back to loop_start
    let back_jmp = fs.emit_jmp(cond_line);
    let offset = loop_start as i32 - (back_jmp as i32 + 1);
    fs.proto.code[back_jmp] = encode_asbx(OpCode::Jmp, 0, offset as i16);

    fs.free_reg_to(base);
    let exit = fs.current_pc();
    for pc in breaks {
        fs.patch_jmp(pc, exit);
    }

    Ok(())
}

fn compile_if(
    fs: &mut FuncState,
    cond: &Expr,
    then_block: &Block,
    elseif_clauses: &[(Expr, Block)],
    else_block: &Option<Block>,
    line: u32,
) -> Result<(), LuaError> {
    let mut end_jumps: Vec<usize> = Vec::new();

    // Main if
    let base = fs.free_reg;
    let cond_reg = fs.alloc_reg()?;
    compile_expr_to_reg(fs, cond, cond_reg)?;
    let cond_line = fs.last_line();
    fs.emit_abc(OpCode::Test, cond_reg, 0, 0, cond_line);
    let false_jmp = fs.emit_jmp(cond_line);
    fs.free_reg_to(base);

    fs.enter_scope(false);
    compile_block(fs, then_block)?;
    fs.leave_scope(cond_line)?;

    if !elseif_clauses.is_empty() || else_block.is_some() {
        end_jumps.push(fs.emit_jmp(fs.last_line()));
    }
    fs.patch_jmp(false_jmp, fs.current_pc());

    // Elseif clauses
    for (elseif_cond, elseif_body) in elseif_clauses {
        let elseif_line = elseif_cond.location.line;
        let base = fs.free_reg;
        let cond_reg = fs.alloc_reg()?;
        compile_expr_to_reg(fs, elseif_cond, cond_reg)?;
        let cond_line = fs.last_line();
        fs.emit_abc(OpCode::Test, cond_reg, 0, 0, cond_line);
        let false_jmp = fs.emit_jmp(cond_line);
        fs.free_reg_to(base);

        fs.enter_scope(false);
        compile_block(fs, elseif_body)?;
        fs.leave_scope(cond_line)?;

        end_jumps.push(fs.emit_jmp(fs.last_line()));
        fs.patch_jmp(false_jmp, fs.current_pc());
    }

    // Else block
    if let Some(else_blk) = else_block {
        fs.enter_scope(false);
        compile_block(fs, else_blk)?;
        fs.leave_scope(fs.last_line())?;
    }

    // Patch all end jumps to here
    let end_pc = fs.current_pc();
    for jmp in end_jumps {
        fs.patch_jmp(jmp, end_pc);
    }

    Ok(())
}

fn compile_numeric_for(
    fs: &mut FuncState,
    name: &str,
    init: &Expr,
    limit: &Expr,
    step: Option<&Expr>,
    body: &Block,
    line: u32,
) -> Result<(), LuaError> {
    fs.enter_scope(true);

    // Lua 5.5 layout: R[base] = iteration count (integer loops) / limit
    // (float loops), R[base+1] = step, R[base+2] = control variable.
    let base = fs.alloc_regs(3)?;

    compile_expr_to_reg(fs, init, base)?;
    compile_expr_to_reg(fs, limit, base + 1)?;
    if let Some(step_expr) = step {
        compile_expr_to_reg(fs, step_expr, base + 2)?;
    } else {
        fs.emit_asbx(OpCode::LoadI, base + 2, 1, line);
    }

    // FORPREP: validates, sets up the counter, or jumps past FORLOOP.
    let forprep_pc = fs.emit_asbx(OpCode::ForPrep, base, 0, line);

    // Internal "(for state)" pseudo-locals plus the const control variable.
    let body_start_pc = fs.proto.code.len() as u32;
    for i in 0..2 {
        fs.locals.push(Local {
            name: "(for state)".to_string(),
            reg: base + i as u8,
            start_pc: body_start_pc,
            is_const: false,
            is_close: false,
        });
    }
    fs.locals.push(Local {
        name: name.to_string(),
        reg: base + 2,
        start_pc: body_start_pc,
        is_const: true,
        is_close: false,
    });

    // Compile body
    let body_start = fs.current_pc();
    compile_block(fs, body)?;

    // The control variable gets a fresh instance every iteration: if the
    // body captured it as an upvalue, close it at the end of each iteration.
    let body_has_upvalues = fs.scopes.last().map(|s| s.has_upvalues).unwrap_or(false);

    // Close the scope before the loop-back test; breaks exit past FORLOOP.
    let breaks = fs.leave_scope_unpatched(line)?;
    if body_has_upvalues {
        fs.emit_abc(OpCode::Close, base + 2, 0, 0, line);
    }

    // FORLOOP: decrement counter (or add float step) and branch back.
    let forloop_pc = fs.emit_asbx(OpCode::ForLoop, base, 0, line);

    // No iterations? FORPREP jumps past FORLOOP.
    fs.patch_sbx(forprep_pc, forloop_pc + 1);
    // FORLOOP branches back to the body start.
    fs.patch_sbx(forloop_pc, body_start);

    let exit = fs.current_pc();
    for pc in breaks {
        fs.patch_jmp(pc, exit);
    }

    Ok(())
}

fn compile_generic_for(
    fs: &mut FuncState,
    names: &[String],
    iterators: &[Expr],
    body: &Block,
    line: u32,
) -> Result<(), LuaError> {
    fs.enter_scope(true);

    // Layout (matches reference Lua 5.5):
    //   R[base]     iterator function
    //   R[base+1]   state
    //   R[base+2]   closing value (after the TFORPREP swap), to-be-closed
    //   R[base+3..] loop variables (first one is the control value)
    let num_vars = names.len();
    let base = fs.free_reg;
    fs.alloc_regs(4)?; // reserve base..base+3

    // Compile iterator expressions into the four control slots.
    let n_iter = iterators.len().min(4);
    let mut filled = 0usize;
    for i in 0..n_iter {
        let is_last = i == n_iter - 1;
        if is_last && i < 3 && is_multi_value_expr(&iterators[i]) {
            // Last expression expands to fill the remaining slots.
            let wanted = (4 - i) as u8;
            compile_expr_multi(fs, &iterators[i], base + i as u8, wanted)?;
            filled = 4;
            break;
        }
        compile_expr_to_reg(fs, &iterators[i], base + i as u8)?;
        filled = i + 1;
    }
    while filled < 4 {
        fs.emit_abc(OpCode::LoadNil, base + filled as u8, 0, 0, line);
        filled += 1;
    }

    // Ensure room for the loop variables (they overlap the control slot).
    if num_vars > 1 {
        fs.alloc_regs((num_vars - 1) as u8)?;
    }
    let needed = base + 3 + num_vars as u8;
    if fs.free_reg < needed {
        fs.free_reg = needed;
        if fs.free_reg > fs.proto.max_stack_size {
            fs.proto.max_stack_size = fs.free_reg;
        }
    }

    // Internal "(for state)" locals; the 3rd (closing) is to-be-closed.
    let ctrl_start_pc = fs.proto.code.len() as u32;
    for i in 0..3 {
        fs.locals.push(Local {
            name: "(for state)".to_string(),
            reg: base + i as u8,
            start_pc: ctrl_start_pc,
            is_const: false,
            is_close: i == 2,
        });
    }
    for (i, name) in names.iter().enumerate() {
        fs.locals.push(Local {
            name: name.clone(),
            reg: base + 3 + i as u8,
            start_pc: ctrl_start_pc,
            is_const: true,
            is_close: false,
        });
    }

    // TFORPREP: swap control/closing, mark closing TBC, jump to TFORCALL.
    let tforprep_pc = fs.emit_asbx(OpCode::TForPrep, base, 0, line);

    // Body
    let body_start = fs.current_pc();
    compile_block(fs, body)?;

    // Make sure the call frame slots (iterator, state, control) are within
    // the GC root window while the iterator runs.
    let needed = base + 6;
    if fs.free_reg < needed {
        fs.free_reg = needed;
        if fs.free_reg > fs.proto.max_stack_size {
            fs.proto.max_stack_size = fs.free_reg;
        }
    }

    // TFORCALL: call the iterator, leaving its results on the loop variables.
    let tforcall_pc = fs.emit_abc(OpCode::TForCall, base, 0, num_vars as u8, line);
    // TFORLOOP: test the first result and branch back to the body.
    let tforloop_pc = fs.emit_abc(OpCode::TForLoop, base, 0, num_vars as u8, line);

    // Patch TFORPREP to jump to TFORCALL
    fs.patch_sbx(tforprep_pc, tforcall_pc);

    // Patch TFORLOOP backward branch (PC is incremented before execution).
    let back_offset = tforloop_pc + 1 - body_start;
    fs.proto.code[tforloop_pc] =
        encode_abc(OpCode::TForLoop, base, back_offset as u8, num_vars as u8);

    // Emit the CLOSE for the to-be-closed control value and patch breaks to
    // land on it (so the closing value is closed when the loop is exited).
    let close_pc = fs.current_pc();
    let breaks = fs.leave_scope_unpatched(line)?;
    for pc in breaks {
        fs.patch_jmp(pc, close_pc);
    }

    Ok(())
}

fn compile_goto(fs: &mut FuncState, label: &str, line: u32) -> Result<(), LuaError> {
    // Try to resolve label immediately (backward goto).
    let resolved = fs
        .labels
        .iter()
        .find(|lbl| lbl.name == label)
        .map(|lbl| (lbl.pc, lbl.num_locals));
    if let Some((target_pc, label_locals)) = resolved {
        // Close upvalues of locals that are leaving scope.
        let first_reg = if fs.locals.len() > label_locals {
            fs.locals[label_locals].reg
        } else {
            fs.free_reg
        };
        fs.emit_abc(OpCode::Close, first_reg, 0, 0, line);
        let jmp_pc = fs.emit_jmp(line);
        fs.patch_jmp(jmp_pc, target_pc);
        return Ok(());
    }

    // Forward goto — add to pending list.  The CLOSE placeholder before the
    // JMP is patched when the label is resolved (reference Lua's closegoto).
    let close_pc = fs.emit_abc(OpCode::Close, fs.free_reg, 0, 0, line);
    let jmp_pc = fs.emit_jmp(line);
    fs.pending_gotos.push(PendingGoto {
        name: label.to_string(),
        patch_pc: jmp_pc,
        close_pc,
        num_locals: fs.locals.len(),
        depth: fs.scopes.len(),
        line,
    });
    Ok(())
}

fn compile_label(fs: &mut FuncState, label: &str, is_last: bool) -> Result<(), LuaError> {
    let pc = fs.current_pc();
    // A label that is the last (non-op) statement of its block behaves as
    // if the block's locals were already out of scope (reference Lua's
    // `createlabel` with `last`).
    let num_locals = if is_last {
        fs.scopes.last().map(|s| s.first_local).unwrap_or(0)
    } else {
        fs.locals.len()
    };

    // Labels cannot repeat while a previous definition is still visible
    // (including enclosing blocks; finished scopes were truncated).
    for lbl in &fs.labels {
        if lbl.name == label {
            return Err(LuaError::new(format!(
                "label '{label}' already defined"
            )));
        }
    }

    fs.labels.push(Label {
        name: label.to_string(),
        pc,
        num_locals,
    });

    // Resolve pending gotos that target this label. Only gotos from this
    // block (or exported from nested blocks into it) may see the label.
    let depth = fs.scopes.len();
    let mut i = 0;
    while i < fs.pending_gotos.len() {
        if fs.pending_gotos[i].name == label && fs.pending_gotos[i].depth == depth {
            let goto = fs.pending_gotos.remove(i);
            // Validate: can't jump into a local's scope
            if goto.num_locals < num_locals {
                return Err(LuaError::new(format!(
                    "goto '{}' at line {} jumps into the scope of '{}'",
                    label,
                    goto.line,
                    fs.locals[goto.num_locals].name,
                )));
            }
            // Patch the CLOSE placeholder: close the locals that go out of
            // scope at the target label.
            let first_reg = if goto.num_locals > num_locals {
                fs.locals[num_locals].reg
            } else {
                fs.free_reg
            };
            fs.proto.code[goto.close_pc] =
                encode_abc(OpCode::Close, first_reg, 0, 0);
            fs.patch_jmp(goto.patch_pc, pc);
        } else {
            i += 1;
        }
    }

    Ok(())
}

fn compile_break(fs: &mut FuncState, line: u32) -> Result<(), LuaError> {
    let scope_idx = fs
        .find_loop_scope()
        .ok_or_else(|| LuaError::new("break outside loop"))?;

    // Close upvalues captured by scopes we are leaving (so break behaves
    // like reaching the loop's end for closure freshness).
    let has_upvalues = fs.scopes[scope_idx..].iter().any(|s| s.has_upvalues);
    if has_upvalues {
        let first_reg = fs
            .locals
            .get(fs.scopes[scope_idx].first_local)
            .map(|l| l.reg)
            .unwrap_or(fs.free_reg);
        fs.emit_abc(OpCode::Close, first_reg, 0, 0, line);
    }

    let jmp_pc = fs.emit_jmp(line);
    fs.scopes[scope_idx].break_jumps.push(jmp_pc);
    Ok(())
}

// ── Function definitions ───────────────────────────────────────────

#[allow(dead_code)]
fn compile_func_body(
    fs: &mut FuncState,
    body: &FuncBody,
    source: Option<String>,
    line: u32,
) -> Result<Proto, LuaError> {
    let mut child_fs = FuncState::new(source, None);

    // Set up parameters
    child_fs.proto.num_params = body.params.len() as u8;
    child_fs.proto.is_vararg = body.has_varargs;

    // _ENV as upvalue[0]
    // Inherit _ENV from parent
    if let Some(uv_idx) = fs.find_upvalue("_ENV") {
        child_fs.proto.upvalues.push(UpvalueDesc {
            name: Some("_ENV".to_string()),
            in_stack: false,
            index: uv_idx,
            is_const: false,
        });
    } else {
        // Top-level: _ENV is upvalue[0] of the parent
        child_fs.proto.upvalues.push(UpvalueDesc {
            name: Some("_ENV".to_string()),
            in_stack: false,
            index: 0,
            is_const: false,
        });
    }

    // VARARGPREP if variadic
    if body.has_varargs {
        // Line 0: no line information for VARARGPREP.
        child_fs.emit_abc(OpCode::VarArgPrep, body.params.len() as u8, 0, 0, 0);
    }

    // Register parameters as locals
    child_fs.enter_scope(false);
    for param in &body.params {
        child_fs.add_local(param.clone())?;
    }

    // Named vararg table: `... name` creates a read-only local
    if let Some(ref va_name) = body.vararg_name {
        let reg = child_fs.add_local(va_name.clone())?;
        child_fs.proto.vararg_name_reg = Some(reg);
        if let Some(local) = child_fs.locals.last_mut() {
            local.is_const = true;
        }
    }

    // Compile body
    compile_block(&mut child_fs, &body.body)?;

    // Ensure function ends with RETURN
    let last_line = body.end_line;
    child_fs.emit_abc(OpCode::Return, 0, 1, 0, last_line);

    child_fs.leave_scope(line)?;

    Ok(child_fs.finish())
}

fn compile_func_def(
    fs: &mut FuncState,
    name: &FuncName,
    body: &FuncBody,
    line: u32,
) -> Result<(), LuaError> {
    // Compile the function body with self parameter for methods
    let mut func_body = body.clone();
    if name.method.is_some() {
        func_body.params.insert(0, "self".to_string());
    }

    let proto = compile_func_body_with_parent(fs, &func_body, line)?;
    let proto_idx = fs.proto.protos.len() as u16;
    fs.proto.protos.push(proto);

    let base = fs.free_reg;
    let dest = fs.alloc_reg()?;
    fs.emit_abx(OpCode::Closure, dest, proto_idx, body.end_line);

    // Assign to the name path: name.path[0].path[1]...
    if name.path.len() == 1 && name.method.is_none() {
        // Simple: function foo() ... end → _ENV["foo"] = closure
        let func_name = &name.path[0];
        if let Some(local_reg) = fs.find_local(func_name) {
            fs.emit_abc(OpCode::Move, local_reg, dest, 0, line);
        } else if let Some(uv) = fs.find_upvalue(func_name) {
            fs.emit_abc(OpCode::SetUpval, dest, uv, 0, line);
        } else {
            let env = env_upvalue(fs)?;
            let k = fs.string_constant(func_name.as_bytes());
            fs.emit_settabup(env, k, dest, line);
        }
    } else {
        // Dotted name: function a.b.c() ... end
        // First, load the base table
        let tab_reg = fs.alloc_reg()?;
        let first_name = &name.path[0];
        if let Some(local_reg) = fs.find_local(first_name) {
            fs.emit_abc(OpCode::Move, tab_reg, local_reg, 0, line);
        } else if let Some(uv) = fs.find_upvalue(first_name) {
            fs.emit_abc(OpCode::GetUpval, tab_reg, uv, 0, line);
        } else {
            let env = env_upvalue(fs)?;
            let k = fs.string_constant(first_name.as_bytes());
            fs.emit_gettabup(tab_reg, env, k, line);
        }

        // Chain through intermediate .path elements (the last segment, or
        // the method name, is the field being assigned below).
        let chain_end = if name.method.is_some() {
            name.path.len()
        } else {
            name.path.len().saturating_sub(1)
        };
        for seg in &name.path[1..chain_end] {
            let key_reg = fs.alloc_reg()?;
            let k = fs.string_constant(seg.as_bytes());
            fs.emit_abx(OpCode::LoadK, key_reg, k, line);
            fs.emit_abc(OpCode::GetTable, tab_reg, tab_reg, key_reg, line);
            fs.free_reg_to(key_reg);
        }

        // Set the final field (method name or last path segment)
        let field_name = name.method.as_ref().unwrap_or(name.path.last().unwrap());
        let key_reg = fs.alloc_reg()?;
        let k = fs.string_constant(field_name.as_bytes());
        fs.emit_abx(OpCode::LoadK, key_reg, k, line);
        fs.emit_abc(OpCode::SetTable, tab_reg, key_reg, dest, line);
    }

    fs.free_reg_to(base);
    Ok(())
}

fn compile_local_func_def(
    fs: &mut FuncState,
    name: &str,
    body: &FuncBody,
    line: u32,
) -> Result<(), LuaError> {
    // Register the local first (so the function can reference itself)
    let reg = fs.add_local(name.to_string())?;

    let proto = compile_func_body_with_parent(fs, body, line)?;
    let proto_idx = fs.proto.protos.len() as u16;
    fs.proto.protos.push(proto);

    fs.emit_abx(OpCode::Closure, reg, proto_idx, body.end_line);
    Ok(())
}

fn compile_global_func_def(
    fs: &mut FuncState,
    name: &str,
    body: &FuncBody,
    line: u32,
) -> Result<(), LuaError> {
    let base = fs.free_reg;
    let dest = fs.alloc_reg()?;

    let proto = compile_func_body_with_parent(fs, body, line)?;
    let proto_idx = fs.proto.protos.len() as u16;
    fs.proto.protos.push(proto);

    fs.emit_abx(OpCode::Closure, dest, proto_idx, body.end_line);

    // Assign to _ENV[name]
    let env = env_upvalue(fs)?;
    let k = fs.string_constant(name.as_bytes());
    fs.emit_settabup(env, k, dest, line);

    fs.free_reg_to(base);
    Ok(())
}

/// Compile a function body, temporarily linking the parent FuncState for upvalue resolution.
fn compile_func_body_with_parent(
    parent_fs: &mut FuncState,
    body: &FuncBody,
    line: u32,
) -> Result<Proto, LuaError> {
    // We need to do upvalue resolution against the parent.
    // Strategy: build the child, then fix up upvalues.

    let source = parent_fs.proto.source.clone();
    let mut child_fs = FuncState::new(source, None);

    child_fs.proto.num_params = body.params.len() as u8;
    child_fs.proto.is_vararg = body.has_varargs;
    child_fs.proto.line_defined = line;
    child_fs.proto.last_line_defined = body.end_line;

    if body.has_varargs {
        // Line 0: no line information for VARARGPREP.
        child_fs.emit_abc(OpCode::VarArgPrep, body.params.len() as u8, 0, 0, 0);
    }

    child_fs.enter_scope(false);
    for param in &body.params {
        child_fs.add_local(param.clone())?;
    }

    // Named vararg table: `... name` creates a read-only local
    if let Some(ref va_name) = body.vararg_name {
        let reg = child_fs.add_local(va_name.clone())?;
        child_fs.proto.vararg_name_reg = Some(reg);
        // Mark as const to prevent assignment
        if let Some(local) = child_fs.locals.last_mut() {
            local.is_const = true;
        }
    }

    // Pre-resolve: collect all variable names referenced in the body (including
    // nested functions) and ensure the parent resolves any that would become
    // upvalues. This is necessary because the dummy enclosing FuncState only
    // has one level of context, so deeply-nested references would otherwise
    // fail to resolve through the grandparent chain.
    let free_names = collect_free_names(&body.body, &body.params);
    // Ensure the parent chain has _ENV available for implicit global access.
    parent_fs.find_upvalue("_ENV");
    for name in &free_names {
        // Only resolve if the parent doesn't already have it as a local or upvalue
        if parent_fs.find_local(name).is_none() {
            parent_fs.find_upvalue(name);
        }
    }

    // Stash parent local info so the child can resolve upvalues.
    // Name resolution: child local → parent local (upvalue in_stack) →
    // parent upvalue → _ENV global.
    child_fs.enclosing = Some(Box::new(FuncState {
        proto: Proto {
            upvalues: parent_fs.proto.upvalues.clone(),
            ..Proto::new(None)
        },
        locals: parent_fs.locals.clone(),
        scopes: Vec::new(), // dummy — not used for scope tracking
        free_reg: parent_fs.free_reg,
        const_globals: parent_fs.const_globals.clone(),
        pending_gotos: Vec::new(),
        labels: Vec::new(),
        enclosing: None, // We only go one level deep here
        current_line: 0,
    }));

    compile_block(&mut child_fs, &body.body)?;
    child_fs.check_pending_gotos()?;

    let last_line = body.end_line;
    child_fs.emit_abc(OpCode::Return, 0, 1, 0, last_line);

    child_fs.leave_scope(last_line)?;

    // Now reconcile upvalues: the child may have added upvalues referencing
    // the parent. We need to mirror those additions into the real parent_fs.
    for uv in &child_fs.proto.upvalues {
        if uv.in_stack {
            // This upvalue captures a parent local. Mark the parent scope.
            let name = uv.name.as_deref().unwrap_or("");
            for scope in parent_fs.scopes.iter_mut().rev() {
                let scope_locals = &parent_fs.locals[scope.first_local..];
                if scope_locals.iter().any(|l| l.name == name) {
                    scope.has_upvalues = true;
                    break;
                }
            }
        } else {
            // This upvalue references a parent upvalue. Ensure the parent has it.
            // Since we cloned parent's upvalues into the dummy enclosing, the
            // indices should already be correct.
        }
    }

    Ok(child_fs.finish())
}

/// Collect all variable names referenced (potentially as upvalues) in a function
/// body, excluding names that are locally bound (params, locals defined inside).
/// This is used to pre-resolve upvalues in the parent before snapshotting.
fn collect_free_names(block: &Block, params: &[String]) -> Vec<String> {
    let mut names = Vec::new();
    let mut locals: Vec<String> = params.to_vec();
    collect_free_names_block(block, &mut locals, &mut names);
    // Deduplicate
    names.sort();
    names.dedup();
    names
}

fn collect_free_names_block(block: &Block, locals: &mut Vec<String>, names: &mut Vec<String>) {
    let saved_locals = locals.len();
    for stat in &block.stmts {
        collect_free_names_stat(&stat.node, locals, names);
    }
    if let Some(ref ret) = block.ret {
        for expr in &ret.values {
            collect_free_names_expr(&expr.node, locals, names);
        }
    }
    locals.truncate(saved_locals);
}

fn collect_free_names_stat(stat: &StatKind, locals: &mut Vec<String>, names: &mut Vec<String>) {
    match stat {
        StatKind::Assign { targets, values } => {
            for v in values {
                collect_free_names_expr(&v.node, locals, names);
            }
            for t in targets {
                collect_free_names_var(t, locals, names);
            }
        }
        StatKind::FunctionCall(call) => {
            collect_free_names_call(call, locals, names);
        }
        StatKind::DoBlock(block) => {
            collect_free_names_block(block, locals, names);
        }
        StatKind::While { cond, body } => {
            collect_free_names_expr(&cond.node, locals, names);
            collect_free_names_block(body, locals, names);
        }
        StatKind::Repeat { body, cond } => {
            collect_free_names_block(body, locals, names);
            collect_free_names_expr(&cond.node, locals, names);
        }
        StatKind::If { cond, then_block, elseif_clauses, else_block } => {
            collect_free_names_expr(&cond.node, locals, names);
            collect_free_names_block(then_block, locals, names);
            for (c, b) in elseif_clauses {
                collect_free_names_expr(&c.node, locals, names);
                collect_free_names_block(b, locals, names);
            }
            if let Some(b) = else_block {
                collect_free_names_block(b, locals, names);
            }
        }
        StatKind::NumericFor { name, init, limit, step, body } => {
            collect_free_names_expr(&init.node, locals, names);
            collect_free_names_expr(&limit.node, locals, names);
            if let Some(s) = step {
                collect_free_names_expr(&s.node, locals, names);
            }
            locals.push(name.clone());
            collect_free_names_block(body, locals, names);
            locals.pop();
        }
        StatKind::GenericFor { names: for_names, iterators, body } => {
            for iter in iterators {
                collect_free_names_expr(&iter.node, locals, names);
            }
            let saved = locals.len();
            for n in for_names {
                locals.push(n.clone());
            }
            collect_free_names_block(body, locals, names);
            locals.truncate(saved);
        }
        StatKind::LocalDecl { names: decl_names, values } => {
            for v in values {
                collect_free_names_expr(&v.node, locals, names);
            }
            for n in decl_names {
                locals.push(n.name.clone());
            }
        }
        StatKind::GlobalDecl { names: _, values } => {
            for v in values {
                collect_free_names_expr(&v.node, locals, names);
            }
        }
        StatKind::FuncDef { body, .. } | StatKind::GlobalFuncDef { body, .. } => {
            // Recurse into the body — free vars there might reference our scope
            collect_free_names_funcbody(body, locals, names);
        }
        StatKind::LocalFuncDef { name, body } => {
            locals.push(name.clone());
            collect_free_names_funcbody(body, locals, names);
        }
        StatKind::Empty | StatKind::Goto(_) | StatKind::Label(_)
        | StatKind::Break | StatKind::GlobalStar { .. } => {}
    }
}

fn collect_free_names_expr(expr: &ExprKind, locals: &mut Vec<String>, names: &mut Vec<String>) {
    match expr {
        ExprKind::Var(var) => {
            collect_free_names_var(var, locals, names);
        }
        ExprKind::FunctionCall(call) => {
            collect_free_names_call(call, locals, names);
        }
        ExprKind::FunctionDef(body) => {
            collect_free_names_funcbody(body, locals, names);
        }
        ExprKind::TableConstructor(fields) => {
            for f in fields {
                match &f.kind {
                    FieldKind::IndexedAssign { key, value } => {
                        collect_free_names_expr(&key.node, locals, names);
                        collect_free_names_expr(&value.node, locals, names);
                    }
                    FieldKind::NameAssign { value, .. } => {
                        collect_free_names_expr(&value.node, locals, names);
                    }
                    FieldKind::Positional(v) => {
                        collect_free_names_expr(&v.node, locals, names);
                    }
                }
            }
        }
        ExprKind::BinOp { lhs, rhs, .. } => {
            collect_free_names_expr(&lhs.node, locals, names);
            collect_free_names_expr(&rhs.node, locals, names);
        }
        ExprKind::UnOp { operand, .. } => {
            collect_free_names_expr(&operand.node, locals, names);
        }
        ExprKind::Paren(inner) => {
            collect_free_names_expr(&inner.node, locals, names);
        }
        ExprKind::Nil | ExprKind::True | ExprKind::False
        | ExprKind::Integer(_) | ExprKind::Float(_)
        | ExprKind::String(_) | ExprKind::VarArg => {}
    }
}

fn collect_free_names_var(var: &Var, locals: &mut Vec<String>, names: &mut Vec<String>) {
    match var {
        Var::Name(n) => {
            if !locals.contains(n) {
                names.push(n.clone());
            }
        }
        Var::Index { table, key } => {
            collect_free_names_expr(&table.node, locals, names);
            collect_free_names_expr(&key.node, locals, names);
        }
        Var::Field { table, .. } => {
            collect_free_names_expr(&table.node, locals, names);
        }
    }
}

fn collect_free_names_call(call: &FunctionCall, locals: &mut Vec<String>, names: &mut Vec<String>) {
    collect_free_names_expr(&call.callee.node, locals, names);
    match &call.args {
        CallArgs::Exprs(exprs) => {
            for e in exprs {
                collect_free_names_expr(&e.node, locals, names);
            }
        }
        CallArgs::Table(fields) => {
            for f in fields {
                match &f.kind {
                    FieldKind::IndexedAssign { key, value } => {
                        collect_free_names_expr(&key.node, locals, names);
                        collect_free_names_expr(&value.node, locals, names);
                    }
                    FieldKind::NameAssign { value, .. } => {
                        collect_free_names_expr(&value.node, locals, names);
                    }
                    FieldKind::Positional(v) => {
                        collect_free_names_expr(&v.node, locals, names);
                    }
                }
            }
        }
        CallArgs::String(_) => {}
    }
}

fn collect_free_names_funcbody(body: &FuncBody, locals: &mut Vec<String>, names: &mut Vec<String>) {
    // For nested functions, collect free names from their body too.
    // The nested function's params are local to it, not to us.
    let saved = locals.len();
    for p in &body.params {
        locals.push(p.clone());
    }
    collect_free_names_block(&body.body, locals, names);
    locals.truncate(saved);
}

/// Resolve an upvalue name against the parent FuncState.
fn resolve_parent_upvalue(parent_fs: &mut FuncState, name: &str) -> UpvalueDesc {
    // Check if parent has it as a local
    if let Some(reg) = parent_fs.find_local(name) {
        // Mark parent scope
        for scope in parent_fs.scopes.iter_mut().rev() {
            let scope_locals = &parent_fs.locals[scope.first_local..];
            if scope_locals.iter().any(|l| l.name == name) {
                scope.has_upvalues = true;
                break;
            }
        }
        let is_const = parent_fs
            .locals
            .iter()
            .rev()
            .find(|l| l.name == name)
            .map(|l| l.is_const)
            .unwrap_or(false);
        return UpvalueDesc {
            name: Some(name.to_string()),
            in_stack: true,
            index: reg,
            is_const,
        };
    }

    // Check if parent has it as an upvalue
    for (i, uv) in parent_fs.proto.upvalues.iter().enumerate() {
        if uv.name.as_deref() == Some(name) {
            return UpvalueDesc {
                name: Some(name.to_string()),
                in_stack: false,
                index: i as u8,
                is_const: uv.is_const,
            };
        }
    }

    // Not found in parent — fall back to _ENV (upvalue[0])
    UpvalueDesc {
        name: Some(name.to_string()),
        in_stack: false,
        index: 0,
        is_const: false,
    }
}

// ── Local/Global declarations ──────────────────────────────────────

fn compile_local_decl(
    fs: &mut FuncState,
    names: &[AttName],
    values: &[Expr],
    line: u32,
) -> Result<(), LuaError> {
    let nnames = names.len();
    let nvalues = values.len();
    let base = fs.free_reg;
    let depth = fs.scopes.len();
    for att_name in names {
        if att_name.attrib.as_deref() == Some("const") {
            fs.const_globals.push((depth, att_name.name.clone()));
        }
    }

    // Evaluate values
    for (i, val) in values.iter().enumerate() {
        let is_last = i == nvalues - 1;
        if is_last && nnames > nvalues {
            // Multi-value context for last expression
            let reg = fs.alloc_reg()?;
            compile_expr_multi(fs, val, reg, (nnames - i) as u8)?;
            for _ in 1..(nnames - i) {
                fs.alloc_reg()?;
            }
        } else if i < nnames {
            let reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, val, reg)?;
        } else {
            // More values than names — evaluate but discard
            let reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, val, reg)?;
            fs.free_reg_to(reg);
        }
    }

    // If no values at all, nil-fill all names.
    // (When nvalues > 0 and nnames > nvalues, compile_expr_multi already handled remaining slots)
    if nvalues == 0 && nnames > 0 {
        let fill_count = nnames as u8;
        fs.alloc_regs(fill_count)?;
        fs.emit_abc(OpCode::LoadNil, base, fill_count - 1, 0, line);
    }

    // Register locals
    for (i, att_name) in names.iter().enumerate() {
        let reg = base + i as u8;
        // Make sure registers are allocated
        if fs.free_reg <= reg {
            fs.free_reg = reg + 1;
            if fs.free_reg > fs.proto.max_stack_size {
                fs.proto.max_stack_size = fs.free_reg;
            }
        }
        fs.locals.push(Local {
            name: att_name.name.clone(),
            reg,
            start_pc: fs.proto.code.len() as u32,
            is_const: matches!(att_name.attrib.as_deref(), Some("const") | Some("close")),
            is_close: att_name.attrib.as_deref() == Some("close"),
        });

        // Emit TBC for <close> variables
        if att_name.attrib.as_deref() == Some("close") {
            fs.emit_abc(OpCode::Tbc, reg, 0, 0, line);
        }
    }

    Ok(())
}

fn compile_global_decl(
    fs: &mut FuncState,
    names: &[AttName],
    values: &[Expr],
    line: u32,
) -> Result<(), LuaError> {
    let nnames = names.len();
    let nvalues = values.len();
    let base = fs.free_reg;
    let depth = fs.scopes.len();
    for att_name in names {
        if att_name.attrib.as_deref() == Some("const") {
            fs.const_globals.push((depth, att_name.name.clone()));
        }
    }

    // Evaluate values into temp registers
    for (i, val) in values.iter().enumerate() {
        let is_last = i == nvalues - 1;
        if is_last && nnames > nvalues {
            let reg = fs.alloc_reg()?;
            compile_expr_multi(fs, val, reg, (nnames - i) as u8)?;
            for _ in 1..(nnames - i) {
                fs.alloc_reg()?;
            }
        } else {
            let reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, val, reg)?;
        }
    }

    // Assign to _ENV for each name. A declaration without a value
    // (`global foo`) emits no runtime code: it only declares the name.
    if nvalues == 0 {
        fs.free_reg_to(base);
        return Ok(());
    }
    let env = env_upvalue(fs)?;
    for (i, att_name) in names.iter().enumerate() {
        let k = fs.string_constant(att_name.name.as_bytes());
        if i < nvalues || (nvalues > 0 && nnames > nvalues) {
            // Has a value
            let src_reg = base + i as u8;
            fs.emit_settabup(env, k, src_reg, line);
        } else {
            // No value — assign nil
            let tmp = fs.alloc_reg()?;
            fs.emit_abc(OpCode::LoadNil, tmp, 0, 0, line);
            fs.emit_settabup(env, k, tmp, line);
            fs.free_reg_to(tmp);
        }
    }

    fs.free_reg_to(base);
    Ok(())
}

// ── Return ─────────────────────────────────────────────────────────

fn compile_return(fs: &mut FuncState, ret: &RetStat) -> Result<(), LuaError> {
    let line = ret.location.line;
    let nvals = ret.values.len();
    if nvals > 254 {
        return Err(LuaError::new("too many returns"));
    }

    if nvals == 0 {
        fs.emit_abc(OpCode::Return, 0, 1, 0, line);
    } else {
        // Tail call detection: return f(...) with exactly one function call
        // and no to-be-closed variables in scope
        if nvals == 1 {
            if let ExprKind::FunctionCall(ref call) = ret.values[0].node {
                if !fs.has_close_vars_in_scope() {
                    let base = fs.free_reg;
                    let func_reg = fs.alloc_reg()?;
                    compile_funcall(fs, call, func_reg, 0, true, line)?;
                    fs.free_reg_to(base);
                    return Ok(());
                }
            }
        }

        let base = fs.free_reg;
        for (i, val) in ret.values.iter().enumerate() {
            let is_last = i == nvals - 1;
            let reg = fs.alloc_reg()?;
            if is_last {
                // Check if it's a function call or vararg — multi-value
                if is_multi_value_expr(val) {
                    compile_expr_multi(fs, val, reg, 0)?; // 0 = all results
                    fs.emit_abc(OpCode::Return, base, 0, 0, line); // B=0: return to top
                    fs.free_reg_to(base);
                    return Ok(());
                }
            }
            compile_expr_to_reg(fs, val, reg)?;
        }
        // B = nvals + 1
        fs.emit_abc(OpCode::Return, base, (nvals + 1) as u8, 0, line);
        fs.free_reg_to(base);
    }
    Ok(())
}

fn is_multi_value_expr(expr: &Expr) -> bool {
    matches!(
        &expr.node,
        ExprKind::FunctionCall(_) | ExprKind::VarArg
    )
}

// ── Expression compilation ─────────────────────────────────────────

/// Compile an expression and ensure the result is in `dest` register.
fn compile_expr_to_reg(fs: &mut FuncState, expr: &Expr, dest: u8) -> Result<(), LuaError> {
    let line = expr.location.line;

    match &expr.node {
        ExprKind::Nil => {
            fs.emit_abc(OpCode::LoadNil, dest, 0, 0, line);
        }
        ExprKind::True => {
            fs.emit_abc(OpCode::LoadBool, dest, 1, 0, line);
        }
        ExprKind::False => {
            fs.emit_abc(OpCode::LoadBool, dest, 0, 0, line);
        }
        ExprKind::Integer(n) => {
            if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                fs.emit_asbx(OpCode::LoadI, dest, *n as i16, line);
            } else {
                let k = fs.add_constant(Constant::Integer(*n));
                fs.emit_abx(OpCode::LoadK, dest, k, line);
            }
        }
        ExprKind::Float(n) => {
            let k = fs.add_constant(Constant::Float(*n));
            fs.emit_abx(OpCode::LoadK, dest, k, line);
        }
        ExprKind::String(s) => {
            let k = fs.string_constant(s);
            fs.emit_abx(OpCode::LoadK, dest, k, line);
        }
        ExprKind::VarArg => {
            // C=2 means one result into dest
            fs.emit_abc(OpCode::VarArg, dest, 0, 2, line);
        }
        ExprKind::Var(var) => {
            compile_var_read(fs, var, dest, line)?;
        }
        ExprKind::FunctionCall(call) => {
            compile_funcall(fs, call, dest, 2, false, line)?; // C=2: one result
        }
        ExprKind::Paren(inner) => {
            compile_expr_to_reg(fs, inner, dest)?;
        }
        ExprKind::FunctionDef(body) => {
            let proto = compile_func_body_with_parent(fs, body, line)?;
            let proto_idx = fs.proto.protos.len() as u16;
            fs.proto.protos.push(proto);
            // Reference Lua emits CLOSURE after parsing the body, so the
            // instruction carries the line of the closing `end`.
            fs.emit_abx(OpCode::Closure, dest, proto_idx, body.end_line);
        }
        ExprKind::TableConstructor(fields) => {
            compile_table_constructor(fs, fields, dest, line)?;
        }
        ExprKind::BinOp { op, lhs, rhs } => {
            compile_binop(fs, *op, lhs, rhs, dest, line)?;
        }
        ExprKind::UnOp { op, operand } => {
            compile_unop(fs, *op, operand, dest, line)?;
        }
    }
    Ok(())
}

/// Compile an expression that may produce multiple results.
/// `wanted`: number of results wanted (0 = all).
fn compile_expr_multi(
    fs: &mut FuncState,
    expr: &Expr,
    dest: u8,
    wanted: u8,
) -> Result<(), LuaError> {
    let line = expr.location.line;
    match &expr.node {
        ExprKind::FunctionCall(call) => {
            // C=0 means results go to top of stack; C=wanted+1 means wanted results
            let c = if wanted == 0 { 0 } else { wanted + 1 };
            compile_funcall(fs, call, dest, c, false, line)?;
        }
        ExprKind::VarArg => {
            let c = if wanted == 0 { 0 } else { wanted + 1 };
            fs.emit_abc(OpCode::VarArg, dest, 0, c, line);
        }
        _ => {
            // Single-value expression
            compile_expr_to_reg(fs, expr, dest)?;
            // Fill remaining with nil if more than 1 result wanted
            if wanted > 1 {
                fs.emit_abc(OpCode::LoadNil, dest + 1, wanted - 2, 0, line);
            }
        }
    }
    Ok(())
}

// ── Variable read ──────────────────────────────────────────────────

/// The upvalue index holding `_ENV` in the current function. Global
/// accesses resolve it lazily so upvalues appear in first-use order.
fn env_upvalue(fs: &mut FuncState) -> Result<u8, LuaError> {
    fs.find_upvalue("_ENV")
        .ok_or_else(|| LuaError::new("cannot resolve '_ENV' upvalue"))
}

fn compile_var_read(
    fs: &mut FuncState,
    var: &Var,
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    match var {
        Var::Name(name) => {
            if let Some(local_reg) = fs.find_local(name) {
                if local_reg != dest {
                    fs.emit_abc(OpCode::Move, dest, local_reg, 0, line);
                }
            } else if let Some(uv) = fs.find_upvalue(name) {
                fs.emit_abc(OpCode::GetUpval, dest, uv, 0, line);
            } else {
                // Global: _ENV[name]
                let env = env_upvalue(fs)?;
                let k = fs.string_constant(name.as_bytes());
                fs.emit_gettabup(dest, env, k, line);
            }
        }
        Var::Index { table, key } => {
            let base = fs.free_reg;
            let tab_reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, table, tab_reg)?;
            let key_reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, key, key_reg)?;
            fs.emit_abc(OpCode::GetTable, dest, tab_reg, key_reg, line);
            fs.free_reg_to(base);
        }
        Var::Field { table, name } => {
            let base = fs.free_reg;
            let tab_reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, table, tab_reg)?;
            let key_reg = fs.alloc_reg()?;
            let k = fs.string_constant(name.as_bytes());
            fs.emit_abx(OpCode::LoadK, key_reg, k, line);
            fs.emit_abc(OpCode::GetTable, dest, tab_reg, key_reg, line);
            fs.free_reg_to(base);
        }
    }
    Ok(())
}

// ── Function call compilation ──────────────────────────────────────

fn compile_funcall(
    fs: &mut FuncState,
    call: &FunctionCall,
    base: u8,
    c: u8,
    is_tailcall: bool,
    line: u32,
) -> Result<(), LuaError> {
    // Compile callee into base register
    compile_expr_to_reg(fs, &call.callee, base)?;

    let has_self = call.method.is_some();
    let self_extra: u8 = if has_self { 1 } else { 0 };
    let arg_start = if has_self { base + 2 } else { base + 1 };

    // Save free_reg before any temporary allocations
    let saved_free = fs.free_reg;

    // For method calls: obj:method(args) → obj.method(obj, args)
    // We need to load the method and put self as first arg
    if let Some(method_name) = &call.method {
        // base = callee (the object)
        // We need:
        //   R[base] = obj[method_name]  (the function)
        //   R[base+1] = obj             (self)
        //   R[base+2..] = args
        let self_reg = if fs.free_reg <= base + 1 {
            fs.alloc_reg()?
        } else {
            base + 1
        };
        // Save the object
        fs.emit_abc(OpCode::Move, self_reg, base, 0, line);
        // Load the method
        let key_reg = fs.alloc_reg()?;
        let k = fs.string_constant(method_name.as_bytes());
        fs.emit_abx(OpCode::LoadK, key_reg, k, line);
        fs.emit_abc(OpCode::GetTable, base, self_reg, key_reg, line);
        // Free temporaries and set free_reg to arg_start
        fs.free_reg = arg_start;
        if fs.free_reg > fs.proto.max_stack_size {
            fs.proto.max_stack_size = fs.free_reg;
        }
        // self_reg already has the object
    }

    // Compile arguments
    let args = match &call.args {
        CallArgs::Exprs(exprs) => exprs.clone(),
        CallArgs::Table(fields) => {
            // Single table constructor argument
            vec![Expr {
                node: ExprKind::TableConstructor(fields.clone()),
                location: crate::token::Location::new(line, 1),
            }]
        }
        CallArgs::String(s) => {
            vec![Expr {
                node: ExprKind::String(s.clone()),
                location: crate::token::Location::new(line, 1),
            }]
        }
    };

    let nargs = args.len();

    // Arguments always live immediately above the call base (CALL reads
    // exactly R[base+1]..). Any temporaries used while evaluating the
    // callee are dead now, so reset the temporary stack even if it was
    // raised (e.g. by a pre-allocated concat operand block).
    fs.free_reg = arg_start;
    if fs.free_reg > fs.proto.max_stack_size {
        fs.proto.max_stack_size = fs.free_reg;
    }

    for (i, arg) in args.iter().enumerate() {
        let is_last = i == nargs - 1;
        let arg_reg = fs.alloc_reg()?;
        if is_last && is_multi_value_expr(arg) {
            compile_expr_multi(fs, arg, arg_reg, 0)?; // 0 = pass all
            // B=0: args extend to top
            let opcode = if is_tailcall { OpCode::TailCall } else { OpCode::Call };
            fs.emit_abc(opcode, base, 0, c, line);
            fs.free_reg_to(saved_free.max(base + 1));
            return Ok(());
        }
        compile_expr_to_reg(fs, arg, arg_reg)?;
    }

    // B = nargs + 1 (plus self if method call)
    let b = (nargs as u8) + self_extra + 1;
    let opcode = if is_tailcall { OpCode::TailCall } else { OpCode::Call };
    fs.emit_abc(opcode, base, b, c, line);
    fs.free_reg_to(saved_free.max(base + 1));

    Ok(())
}

// ── Table constructor ──────────────────────────────────────────────

fn compile_table_constructor(
    fs: &mut FuncState,
    fields: &[Field],
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    // Count array vs hash fields for size hints
    let mut arr_count = 0u8;
    let mut hash_count = 0u8;
    for f in fields {
        match &f.kind {
            FieldKind::Positional(_) => arr_count = arr_count.saturating_add(1),
            _ => hash_count = hash_count.saturating_add(1),
        }
    }

    fs.emit_abc(OpCode::NewTable, dest, arr_count, hash_count, line);

    let mut arr_pending = 0u32; // consecutively pending positional fields
    let mut arr_total = 0u32;   // total positional fields seen

    for field in fields {
        match &field.kind {
            FieldKind::Positional(val) => {
                arr_pending += 1;
                arr_total += 1;
                let val_reg = fs.alloc_reg()?;

                // Check if this is the last field and it's multi-value
                let is_last = arr_total as usize + hash_count as usize == fields.len();
                if is_last && is_multi_value_expr(val) {
                    compile_expr_multi(fs, val, val_reg, 0)?;
                    // SETLIST with B=0: extends to top of stack
                    let flush_idx = ((arr_total - 1) / FIELDS_PER_FLUSH + 1) as u8;
                    fs.emit_abc(OpCode::SetList, dest, 0, flush_idx, line);
                    fs.free_reg_to(dest + 1);
                    continue;
                }

                compile_expr_to_reg(fs, val, val_reg)?;

                // Flush when we hit FIELDS_PER_FLUSH or this is the last positional
                if arr_pending >= FIELDS_PER_FLUSH || is_last {
                    let flush_idx = ((arr_total - arr_pending) / FIELDS_PER_FLUSH + 1) as u8;
                    fs.emit_abc(OpCode::SetList, dest, arr_pending as u8, flush_idx, line);
                    fs.free_reg_to(dest + 1);
                    arr_pending = 0;
                }
            }
            FieldKind::NameAssign { name, value } => {
                let base = fs.free_reg;
                let key_reg = fs.alloc_reg()?;
                let k = fs.string_constant(name.as_bytes());
                fs.emit_abx(OpCode::LoadK, key_reg, k, line);
                let val_reg = fs.alloc_reg()?;
                compile_expr_to_reg(fs, value, val_reg)?;
                fs.emit_abc(OpCode::SetTable, dest, key_reg, val_reg, line);
                fs.free_reg_to(base);
            }
            FieldKind::IndexedAssign { key, value } => {
                let base = fs.free_reg;
                let key_reg = fs.alloc_reg()?;
                compile_expr_to_reg(fs, key, key_reg)?;
                let val_reg = fs.alloc_reg()?;
                compile_expr_to_reg(fs, value, val_reg)?;
                fs.emit_abc(OpCode::SetTable, dest, key_reg, val_reg, line);
                fs.free_reg_to(base);
            }
        }
    }

    Ok(())
}

// ── Binary operations ──────────────────────────────────────────────

fn compile_binop(
    fs: &mut FuncState,
    op: BinOp,
    lhs: &Expr,
    rhs: &Expr,
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    match op {
        // Short-circuit logical operators
        BinOp::And => {
            compile_expr_to_reg(fs, lhs, dest)?;
            // TESTSET dest, dest, 0 → if falsy, keep dest and skip
            fs.emit_abc(OpCode::TestSet, dest, dest, 0, line);
            let skip_jmp = fs.emit_jmp(line);
            compile_expr_to_reg(fs, rhs, dest)?;
            fs.patch_jmp(skip_jmp, fs.current_pc());
        }
        BinOp::Or => {
            compile_expr_to_reg(fs, lhs, dest)?;
            // TESTSET dest, dest, 1 → if truthy, keep dest and skip
            fs.emit_abc(OpCode::TestSet, dest, dest, 1, line);
            let skip_jmp = fs.emit_jmp(line);
            compile_expr_to_reg(fs, rhs, dest)?;
            fs.patch_jmp(skip_jmp, fs.current_pc());
        }

        // Concatenation: arrange operands in consecutive registers
        BinOp::Concat => {
            compile_concat(fs, lhs, rhs, dest, line)?;
        }

        // Comparison operators: produce boolean via LOADBOOL
        BinOp::Eq | BinOp::NEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
            compile_comparison(fs, op, lhs, rhs, dest, line)?;
        }

        // Arithmetic & bitwise: straightforward binary ops
        _ => {
            let base = fs.free_reg;
            let lhs_reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, lhs, lhs_reg)?;
            let rhs_reg = fs.alloc_reg()?;
            compile_expr_to_reg(fs, rhs, rhs_reg)?;

            let opcode = match op {
                BinOp::Add => OpCode::Add,
                BinOp::Sub => OpCode::Sub,
                BinOp::Mul => OpCode::Mul,
                BinOp::Div => OpCode::Div,
                BinOp::IDiv => OpCode::IDiv,
                BinOp::Mod => OpCode::Mod,
                BinOp::Pow => OpCode::Pow,
                BinOp::BAnd => OpCode::BAnd,
                BinOp::BOr => OpCode::BOr,
                BinOp::BXor => OpCode::BXor,
                BinOp::Shl => OpCode::Shl,
                BinOp::Shr => OpCode::Shr,
                _ => unreachable!(),
            };

            fs.emit_abc(opcode, dest, lhs_reg, rhs_reg, line);
            fs.free_reg_to(base);
        }
    }
    Ok(())
}

fn compile_comparison(
    fs: &mut FuncState,
    op: BinOp,
    lhs: &Expr,
    rhs: &Expr,
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    let base = fs.free_reg;
    let lhs_reg = fs.alloc_reg()?;
    compile_expr_to_reg(fs, lhs, lhs_reg)?;
    let rhs_reg = fs.alloc_reg()?;
    compile_expr_to_reg(fs, rhs, rhs_reg)?;

    // Determine opcode and operand order
    // `>` and `>=` are compiled by swapping operands
    // `~=` is compiled as `EQ` with inverted A
    let (opcode, a_val, left, right) = match op {
        BinOp::Eq => (OpCode::Eq, 0u8, lhs_reg, rhs_reg),
        BinOp::NEq => (OpCode::Eq, 1, lhs_reg, rhs_reg),
        BinOp::Lt => (OpCode::Lt, 0, lhs_reg, rhs_reg),
        BinOp::Gt => (OpCode::Lt, 0, rhs_reg, lhs_reg),    // swap
        BinOp::LtEq => (OpCode::Le, 0, lhs_reg, rhs_reg),
        BinOp::GtEq => (OpCode::Le, 0, rhs_reg, lhs_reg),  // swap
        _ => unreachable!(),
    };

    // CMP A B C: if ((R[B] op R[C]) ~= A) then PC++ (skip next)
    // A=1 for positive tests (==, <, <=): skip next if NOT matching
    // A=0 for negative test (~=): skip next if matching
    // > and >= are compiled by swapping operands with < and <=
    //
    // Pattern:
    //   CMP a_invert left right  — skip next if condition doesn't hold
    //   LOADBOOL dest 1 1        — match: true, skip next
    //   LOADBOOL dest 0 0        — no match: false
    let a_invert = match op {
        BinOp::Eq | BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => 1u8,
        BinOp::NEq => 0,
        _ => unreachable!(),
    };

    fs.emit_abc(opcode, a_invert, left, right, line);
    fs.emit_abc(OpCode::LoadBool, dest, 1, 1, line);
    fs.emit_abc(OpCode::LoadBool, dest, 0, 0, line);

    fs.free_reg_to(base);
    Ok(())
}

fn compile_concat(
    fs: &mut FuncState,
    lhs: &Expr,
    rhs: &Expr,
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    // Flatten the concat chain: a .. b .. c → [a, b, c]
    let mut parts: Vec<&Expr> = Vec::new();
    collect_concat_parts(lhs, &mut parts);
    collect_concat_parts(rhs, &mut parts);

    let base = fs.free_reg;
    if dest as usize + 1 == base as usize {
        // The destination register is immediately below the temporaries:
        // evaluate the first operand there (like reference Lua), so no
        // stale value is left in `dest` before the CONCAT's possible GC.
        compile_expr_to_reg(fs, parts[0], dest)?;
        for (i, part) in parts.iter().enumerate().skip(1) {
            let reg = dest + i as u8;
            fs.alloc_reg()?;
            compile_expr_to_reg(fs, part, reg)?;
        }
        let end = dest + parts.len() as u8 - 1;
        fs.emit_abc(OpCode::Concat, dest, dest, end, line);
        fs.free_reg_to(base);
        return Ok(());
    }
    let start = fs.alloc_regs(parts.len() as u8)?;
    for (i, part) in parts.iter().enumerate() {
        compile_expr_to_reg(fs, part, start + i as u8)?;
    }
    let end = start + parts.len() as u8 - 1;
    fs.emit_abc(OpCode::Concat, dest, start, end, line);
    fs.free_reg_to(base);

    Ok(())
}

fn collect_concat_parts<'a>(expr: &'a Expr, parts: &mut Vec<&'a Expr>) {
    if let ExprKind::BinOp {
        op: BinOp::Concat,
        lhs,
        rhs,
    } = &expr.node
    {
        collect_concat_parts(lhs, parts);
        collect_concat_parts(rhs, parts);
    } else {
        parts.push(expr);
    }
}

// ── Unary operations ───────────────────────────────────────────────

fn compile_unop(
    fs: &mut FuncState,
    op: UnOp,
    operand: &Expr,
    dest: u8,
    line: u32,
) -> Result<(), LuaError> {
    let base = fs.free_reg;
    let src = fs.alloc_reg()?;
    compile_expr_to_reg(fs, operand, src)?;

    let opcode = match op {
        UnOp::Neg => OpCode::Unm,
        UnOp::Not => OpCode::Not,
        UnOp::Len => OpCode::Len,
        UnOp::BNot => OpCode::BNot,
    };

    fs.emit_abc(opcode, dest, src, 0, line);
    fs.free_reg_to(base);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn compile_str(source: &str) -> Proto {
        let mut lexer = Lexer::new(source.as_bytes(), "test");
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let block = parser.parse_chunk().expect("parser failed");
        compile(&block, Some("test".into())).expect("compiler failed")
    }

    #[test]
    fn test_compile_empty() {
        let proto = compile_str("");
        // Should have VARARGPREP and RETURN
        assert!(proto.code.len() >= 2);
        assert_eq!(decode_op(proto.code[0]), OpCode::VarArgPrep as u8);
        let last = *proto.code.last().unwrap();
        assert_eq!(decode_op(last), OpCode::Return as u8);
    }

    #[test]
    fn test_compile_local_int() {
        let proto = compile_str("local x = 42");
        // VARARGPREP, LOADI x 42, RETURN
        assert!(proto.code.len() >= 3);
        let loadi = proto.code[1];
        assert_eq!(decode_op(loadi), OpCode::LoadI as u8);
        assert_eq!(decode_a(loadi), 0); // register 0
        assert_eq!(decode_sbx(loadi), 42);
    }

    #[test]
    fn test_compile_local_string() {
        let proto = compile_str(r#"local x = "hello""#);
        let loadk = proto.code[1];
        assert_eq!(decode_op(loadk), OpCode::LoadK as u8);
        assert_eq!(proto.constants[0], Constant::String(b"hello".to_vec()));
    }

    #[test]
    fn test_compile_local_nil() {
        let proto = compile_str("local x");
        // VARARGPREP, LOADNIL, RETURN
        let loadnil = proto.code[1];
        assert_eq!(decode_op(loadnil), OpCode::LoadNil as u8);
    }

    #[test]
    fn test_compile_arithmetic() {
        let proto = compile_str("local a, b = 1, 2; local c = a + b");
        // Should have ADD somewhere
        let has_add = proto.code.iter().any(|&inst| decode_op(inst) == OpCode::Add as u8);
        assert!(has_add, "should contain ADD instruction");
    }

    #[test]
    fn test_compile_global_access() {
        let proto = compile_str("print(x)");
        // Should have GETTABUP for print and x
        let gettabup_count = proto
            .code
            .iter()
            .filter(|&&inst| decode_op(inst) == OpCode::GetTabUp as u8)
            .count();
        assert!(gettabup_count >= 2, "should have GETTABUP for print and x");
    }

    #[test]
    fn test_compile_if() {
        let proto = compile_str("if true then local x = 1 end");
        let has_test = proto.code.iter().any(|&inst| decode_op(inst) == OpCode::Test as u8);
        let has_jmp = proto.code.iter().any(|&inst| decode_op(inst) == OpCode::Jmp as u8);
        assert!(has_test, "should contain TEST");
        assert!(has_jmp, "should contain JMP");
    }

    #[test]
    fn test_compile_while() {
        let proto = compile_str("local i = 0; while i < 10 do i = i + 1 end");
        let has_lt = proto.code.iter().any(|&inst| decode_op(inst) == OpCode::Lt as u8);
        assert!(has_lt, "should contain LT comparison");
    }

    #[test]
    fn test_compile_numeric_for() {
        let proto = compile_str("for i = 1, 10 do end");
        let has_forprep = proto
            .code
            .iter()
            .any(|&inst| decode_op(inst) == OpCode::ForPrep as u8);
        let has_forloop = proto
            .code
            .iter()
            .any(|&inst| decode_op(inst) == OpCode::ForLoop as u8);
        assert!(has_forprep, "should contain FORPREP");
        assert!(has_forloop, "should contain FORLOOP");
    }

    #[test]
    fn test_compile_function_def() {
        let proto = compile_str("local function f(a, b) return a + b end");
        assert!(!proto.protos.is_empty(), "should have nested proto");
        let child = &proto.protos[0];
        assert_eq!(child.num_params, 2);
    }

    #[test]
    fn test_compile_table_constructor() {
        let proto = compile_str("local t = {1, 2, 3, x = 4}");
        let has_newtable = proto
            .code
            .iter()
            .any(|&inst| decode_op(inst) == OpCode::NewTable as u8);
        let has_setlist = proto
            .code
            .iter()
            .any(|&inst| decode_op(inst) == OpCode::SetList as u8);
        assert!(has_newtable, "should contain NEWTABLE");
        assert!(has_setlist, "should contain SETLIST");
    }

    #[test]
    fn test_compile_return() {
        let proto = compile_str("return 1, 2, 3");
        // Find RETURN instruction
        let ret_inst = proto.code.iter().find(|&&inst| {
            decode_op(inst) == OpCode::Return as u8 && decode_b(inst) == 4 // 3 values + 1
        });
        assert!(ret_inst.is_some(), "should have RETURN with B=4");
    }

    #[test]
    fn test_compile_concat() {
        let proto = compile_str(r#"local x = "a" .. "b" .. "c""#);
        let has_concat = proto
            .code
            .iter()
            .any(|&inst| decode_op(inst) == OpCode::Concat as u8);
        assert!(has_concat, "should contain CONCAT");
    }

    #[test]
    fn test_compile_unop() {
        let proto = compile_str("local x = 1; local y = -x");
        let has_unm = proto.code.iter().any(|&inst| decode_op(inst) == OpCode::Unm as u8);
        assert!(has_unm, "should contain UNM");
    }

    #[test]
    fn test_disassemble() {
        let proto = compile_str("local x = 1 + 2");
        let dis = proto.disassemble();
        assert!(dis.contains("LOADI"), "disassembly should contain LOADI");
        assert!(dis.contains("ADD"), "disassembly should contain ADD");
    }
}
