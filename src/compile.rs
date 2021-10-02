use std::{
    cell::{Ref, RefCell},
    collections::{BTreeMap, HashMap, HashSet},
    convert::TryInto,
    fmt::format,
    rc::Rc,
    vec,
};

use crate::{
    bytecode::*,
    closure::ClosurePrototype,
    debug_println, log_2,
    parse::{Expr, FunctionName, SourceLocation, Stmt, TableField, Token},
    state::MAX_LOCALS,
};

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    ParseOrTokenizeError,
    TooManyLocals,
    OutOfMemoryError,
    SemanticError,
    InternalError,
}

#[derive(Clone, Debug)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub msg: String,
    pub loc: SourceLocation,
}
#[derive(Clone, Copy, Debug)]
struct VarInfo {
    func_scope: usize,
    location: u32,
    uid: u32,
    // is_on_stack: bool,
    is_upvalue: bool,
}
struct SymbolTable {
    vars: HashMap<String, VarInfo>,
    n_locals: usize,
    func_scope: usize,
    parent: *mut SymbolTable,
}
#[derive(Clone, Copy)]
struct CompileExprExtra {
    n_expect_values: u8,
}
impl Default for CompileExprExtra {
    fn default() -> Self {
        Self { n_expect_values: 1 }
    }
}
impl SymbolTable {
    fn set(&mut self, var: String, info: VarInfo) {
        self.vars.insert(var, info);
    }
    fn get_cur<'a>(&'a mut self, var: &String) -> Option<&'a mut VarInfo> {
        if let Some(info) = self.vars.get_mut(var) {
            Some(info)
        } else {
            unsafe {
                if let Some(parent) = self.parent.as_mut() {
                    if parent.func_scope == self.func_scope {
                        parent.get_cur(var)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
    fn get_prev<'a>(&'a mut self, var: &String) -> Option<&'a mut VarInfo> {
        unsafe {
            if let Some(parent) = self.parent.as_mut() {
                if parent.func_scope == self.func_scope {
                    parent.get_prev(var)
                } else if parent.func_scope == self.func_scope - 1 {
                    parent.get_cur(var)
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
    fn get<'a>(&'a self, var: &String) -> Option<&'a VarInfo> {
        if let Some(info) = self.vars.get(var) {
            Some(info)
        } else {
            unsafe {
                if let Some(parent) = self.parent.as_ref() {
                    parent.get(var)
                } else {
                    None
                }
            }
        }
    }
    // fn get_mut<'a>(&'a mut self, var: &String) -> Option<&'a mut VarInfo> {
    //     if let Some(info) = self.vars.get_mut(var) {
    //         Some(info)
    //     } else {
    //         unsafe {
    //             if let Some(parent) = self.parent.as_mut() {
    //                 if parent.func_scope == self.func_scope {
    //                     parent.get_mut(var)
    //                 } else {
    //                     None
    //                 }
    //             } else {
    //                 None
    //             }
    //         }
    //     }
    // }
}
enum FunctionLocation {
    Local,
    Global,
    Stack,
    Repl,
}
#[derive(Clone, Debug)]
pub(crate) struct UpValueInfo {
    pub(crate) from_parent: bool,
    pub(crate) id: u32,
    pub(crate) uid: u32,
    pub(crate) location: u32,
    pub(crate) is_special: bool,
    pub(crate) name: String,
}
struct FuncInfo {
    upvalues: HashMap<u32, UpValueInfo>,
}
struct Compiler {
    symbols: SymbolTable,
    funcs: Vec<FuncInfo>,
    module: ByteCodeModule,
    str_map: HashMap<String, usize>,
    var_uid_gen: u32,
    label_gen: u32,
    labels: HashMap<u32, usize>,
    break_labels: Vec<u32>,
}
macro_rules! compile_assign_impl {
    ($self:expr, $loc:expr, $lhs:expr, $rhs:expr, $is_local_var_def:expr,$handle_var_def:expr, $compile_lhs:expr) => {{
        let loc = $loc;
        let lhs = $lhs;
        let rhs = $rhs;
        let is_local_var_def = $is_local_var_def;
        if lhs.len() >= u8::MAX as usize - 1 || rhs.len() >= u8::MAX as usize - 1 {
            return Err(CompileError {
                loc: loc.clone(),
                kind: ErrorKind::TooManyLocals,
                msg: "number of variables in assignment exceed limit (u8::MAX - 1)".into(),
            });
        }
        let n_vars = lhs.len();
        let is_last_expr_call = if let Some(e) = rhs.last() {
            match &**e {
                Expr::CallExpr { .. } => true,
                Expr::MethodCallExpr { .. } => true,
                _ => false,
            }
        } else {
            false
        };
        for (i, e) in rhs.iter().enumerate() {
            if i + 1 < rhs.len() {
                $self.compile_expr(e, Default::default())?;
            } else {
                if n_vars >= rhs.len() {
                    $self.compile_expr(
                        e,
                        CompileExprExtra {
                            n_expect_values: (n_vars + 1 - rhs.len()) as u8,
                        },
                    )?;
                } else {
                    $self.compile_expr(e, Default::default())?;
                    $self.emit(ByteCode::Op(OpCode::Pop));
                }
            }
        }
        if !is_last_expr_call && rhs.len() < n_vars {
            for _ in rhs.len()..n_vars {
                $self.emit(ByteCode::Op(OpCode::LoadNil));
            }
        }
        if is_local_var_def {
            for var in lhs {
                ($handle_var_def)($self, var)?;
            }
        }
        for var in lhs.iter().rev() {
            ($compile_lhs)($self, var)?;
        }
        // Ok(())
    }};
}
impl Compiler {
    fn emit(&mut self, inst: ByteCode) {
        // let ret = self.module.code.len();
        self.module.code.push(inst);
        // ret
    }
    fn new_label(&mut self) -> u32 {
        let l = self.label_gen;
        self.label_gen += 1;
        l
    }
    fn emit_label(&mut self, l: u32) {
        self.labels.insert(l, self.module.code.len());
    }
    fn push_number(&mut self, n: f64) {
        let bytes = n.to_le_bytes();

        self.emit(ByteCode::Op(OpCode::LoadNumber));
        self.emit(ByteCode::FloatLo([bytes[0], bytes[1], bytes[2], bytes[3]]));
        self.emit(ByteCode::FloatHi([bytes[4], bytes[5], bytes[6], bytes[7]]));
    }
    fn string_pool_index(&mut self, s: &String) -> u32 {
        let idx = if let Some(idx) = self.str_map.get(s) {
            *idx
        } else {
            let idx = self.module.string_pool.len();
            self.module.string_pool.push(s.clone());
            self.str_map.insert(s.clone(), idx);
            idx
        } as u32;
        idx
    }
    fn push_string(&mut self, s: &String) {
        let idx = self.string_pool_index(s);
        let bytes = idx.to_le_bytes();
        assert!(bytes[3] == 0);
        self.emit(ByteCode::Op3U8(
            OpCode::LoadStr,
            [bytes[0], bytes[1], bytes[2]],
        ));
    }
    fn resolve_upvalue(&mut self, name: &String) {
        if let Some(info) = self.symbols.get(name).map(|x| *x) {
            if info.func_scope != self.funcs.len() {
                // println!("{} is upvalue", name);
                // is a upvalue
                let mut p = &mut self.symbols as *mut SymbolTable;
                let mut tables = vec![];
                unsafe {
                    while let Some(tab) = p.as_mut() {
                        tables.push(p);
                        if tab.func_scope < info.func_scope {
                            break;
                        }

                        if tab.func_scope == info.func_scope {
                            // the locals of that function

                            let func = &mut self.funcs[tab.func_scope - 1];
                            if !func.upvalues.contains_key(&info.uid) {
                                debug_println!("local upvalue {}", name);
                                let id = func.upvalues.len() as u32;
                                let info = tab.get_cur(name).unwrap_or_else(|| {
                                    panic!("{}", name);
                                });
                                func.upvalues.insert(
                                    info.uid,
                                    UpValueInfo {
                                        from_parent: false,
                                        id,
                                        uid: info.uid,
                                        location: info.location,
                                        is_special: false,
                                        name: name.clone(),
                                    },
                                );
                            }
                            break;
                        }

                        p = tab.parent;
                    }
                    tables.reverse();
                    for p in tables {
                        let tab = p.as_mut().unwrap();
                        if tab.func_scope > info.func_scope {
                            assert!(tab.func_scope <= self.funcs.len());
                            let func = &mut *self.funcs.as_mut_ptr().add(tab.func_scope - 1);

                            if !func.upvalues.contains_key(&info.uid) {
                                let prev_func = &mut self.funcs[tab.func_scope - 2];
                                let id = func.upvalues.len() as u32;
                                let info = *tab.get_prev(name).unwrap();
                                let location = prev_func.upvalues.get(&info.uid).unwrap().id;
                                func.upvalues.insert(
                                    info.uid,
                                    UpValueInfo {
                                        from_parent: true,
                                        id,
                                        uid: info.uid,
                                        location,
                                        is_special: false,
                                        name: name.clone(),
                                    },
                                );
                                tab.set(
                                    name.clone(),
                                    VarInfo {
                                        func_scope: tab.func_scope,
                                        location: id,
                                        uid: info.uid,
                                        // is_on_stack: false,
                                        is_upvalue: true,
                                    },
                                )
                            }
                        }
                    }
                }
            }
        }
    }
    fn is_string_literal(expr: &Expr) -> bool {
        match expr {
            Expr::Literal { token } => match token {
                Token::String { .. } => {
                    return true;
                }
                _ => {}
            },
            _ => {}
        }
        false
    }
    fn is_identifier(expr: &Expr) -> bool {
        match expr {
            Expr::Identifier { .. } => true,
            _ => false,
        }
    }
    fn load_identifier(&mut self, name: &String) -> Result<(), CompileError> {
        self.resolve_upvalue(name);
        if let Some(info) = self.symbols.get(name).map(|x| *x) {
            if info.is_upvalue {
                self.emit(ByteCode::Op3U8(
                    OpCode::LoadUpvalue,
                    get_3xu8(info.location),
                ));
            } else {
                self.emit(ByteCode::Op3U8(OpCode::LoadLocal, get_3xu8(info.location)));
            }
        } else {
            // self.push_string(name);
            let idx = self.string_pool_index(name);
            let info = *self.symbols.get(&"_ENV".into()).unwrap();
            if info.is_upvalue {
                self.emit(ByteCode::Op3U8(
                    OpCode::LoadUpvalue,
                    get_3xu8(info.location),
                ));
            } else {
                self.emit(ByteCode::Op3U8(OpCode::LoadLocal, get_3xu8(info.location)));
            }
            // self.emit(ByteCode::Op(OpCode::LoadTable));
            self.emit(ByteCode::Op3U8(OpCode::LoadTableStringKey, get_3xu8(idx)));
        }
        Ok(())
    }
    fn compile_expr(&mut self, expr: &Expr, ext: CompileExprExtra) -> Result<(), CompileError> {
        match expr {
            Expr::ParenExpr { expr, .. } => {
                self.compile_expr(expr, CompileExprExtra { n_expect_values: 1 })
            }
            Expr::VarArgs { token: _ } => self.load_identifier(&"...".into()),
            Expr::Const { token } => match token {
                Token::Keyword { value, .. } => {
                    match value.as_str() {
                        "nil" => {
                            self.emit(ByteCode::Op(OpCode::LoadNil));
                        }
                        "true" => {
                            self.emit(ByteCode::Op(OpCode::LoadTrue));
                        }
                        "false" => {
                            self.emit(ByteCode::Op(OpCode::LoadFalse));
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                }
                _ => unreachable!(),
            },
            Expr::Literal { token } => match token {
                Token::Number { value, .. } => {
                    self.push_number(*value);
                    Ok(())
                }
                Token::String { value, .. } => {
                    self.push_string(value);
                    Ok(())
                }
                _ => unreachable!(),
            },
            Expr::Identifier { token } => {
                let name = match token {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.load_identifier(name)?;
                Ok(())
            }
            Expr::BinaryExpr { op, lhs, rhs } => {
                let opcode = match op {
                    Token::Keyword { value, .. } => match value.as_str() {
                        "and" => {
                            self.compile_expr(lhs, Default::default())?;
                            self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 0, 1]));
                            let l = self.new_label();
                            self.emit(ByteCode::Label(l.to_le_bytes()));
                            self.compile_expr(rhs, Default::default())?;
                            self.emit_label(l);
                            return Ok(());
                        }
                        "or" => {
                            self.compile_expr(lhs, Default::default())?;
                            self.emit(ByteCode::Op3U8(OpCode::TestJump, [1, 0, 1]));
                            let l = self.new_label();
                            self.compile_expr(rhs, Default::default())?;
                            self.emit_label(l);
                            return Ok(());
                        }
                        _ => unreachable!(),
                    },
                    Token::Symbol { value, .. } => match value.as_str() {
                        "+" => OpCode::Add,
                        "-" => OpCode::Sub,
                        "*" => OpCode::Mul,
                        "/" => OpCode::Div,
                        "%" => OpCode::Mod,
                        "^" => OpCode::Pow,
                        "&" => OpCode::And,
                        "|" => OpCode::Or,
                        "//" => OpCode::IDiv,
                        "<" => OpCode::LessThan,
                        "<=" => OpCode::LessThanEqual,
                        ">=" => OpCode::GreaterThanEqual,
                        ">" => OpCode::GreaterThan,
                        "==" => OpCode::Equal,
                        "~=" => OpCode::NotEqual,
                        ".." => OpCode::Concat,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                self.compile_expr(lhs, Default::default())?;
                self.compile_expr(rhs, Default::default())?;
                self.emit(ByteCode::Op(opcode));
                Ok(())
            }
            Expr::UnaryExpr { op, arg } => {
                self.compile_expr(arg, Default::default())?;
                let opcode = match op {
                    Token::Symbol { value, .. } => match value.as_str() {
                        "~" => OpCode::BitwiseNot,
                        "-" => OpCode::Neg,
                        "#" => OpCode::Len,
                        _ => unreachable!(),
                    },
                    Token::Keyword { value, .. } if value == "not" => OpCode::Not,
                    _ => unreachable!(),
                };
                self.emit(ByteCode::Op(opcode));
                Ok(())
            }
            Expr::IndexExpr { loc: _, lhs, rhs } => {
                if Self::is_string_literal(rhs) {
                    match &**rhs {
                        Expr::Literal {
                            token: Token::String { value, .. },
                        } => {
                            let idx = self.string_pool_index(value);
                            self.compile_expr(lhs, Default::default())?;
                            self.emit(ByteCode::Op3U8(OpCode::LoadTableStringKey, get_3xu8(idx)));
                        }
                        _ => unreachable!(),
                    }
                } else {
                    self.compile_expr(rhs, Default::default())?;
                    self.compile_expr(lhs, Default::default())?;
                    self.emit(ByteCode::Op(OpCode::LoadTable));
                }
                Ok(())
            }
            Expr::DotExpr {
                loc: _,
                lhs,
                rhs: token,
            } => {
                let name = match token {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.push_string(name);

                self.compile_expr(lhs, Default::default())?;
                self.emit(ByteCode::Op(OpCode::LoadTable));
                Ok(())
            }
            Expr::CallExpr { callee, args } => {
                for (i, arg) in args.iter().rev().enumerate() {
                    self.compile_expr(
                        &**arg,
                        CompileExprExtra {
                            n_expect_values: if i == 0 { u8::MAX } else { 1 },
                        },
                    )?;
                }
                self.compile_expr(&*callee, Default::default())?;
                self.emit(ByteCode::Op3U8(
                    OpCode::Call,
                    [args.len() as u8, ext.n_expect_values, 0],
                ));
                Ok(())
            }
            Expr::MethodCallExpr {
                callee,
                method,
                args,
            } => {
                // todo!()
                for (i, arg) in args.iter().rev().enumerate() {
                    self.compile_expr(
                        &**arg,
                        CompileExprExtra {
                            n_expect_values: if i == 0 { u8::MAX } else { 1 },
                        },
                    )?;
                }
                self.compile_expr(callee, Default::default())?;
                let method = match method {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.push_string(method);
                self.emit(ByteCode::Op(OpCode::Self_));
                self.emit(ByteCode::Op3U8(
                    OpCode::Call,
                    [args.len() as u8 + 1, ext.n_expect_values, 0],
                ));
                Ok(())
            }
            Expr::FunctionExpr { loc, args, body } => {
                self.compile_function(None, args, body, FunctionLocation::Stack)
            }
            Expr::Table { loc: _, fields } => {
                let array: Vec<_> = fields
                    .iter()
                    .filter(|f| match f {
                        TableField::ArrayEntry(_) => true,
                        _ => false,
                    })
                    .collect();
                let table: Vec<_> = fields
                    .iter()
                    .filter(|f| match f {
                        TableField::ArrayEntry(_) => false,
                        _ => true,
                    })
                    .collect();
                self.emit(ByteCode::Op3U8(
                    OpCode::NewTable,
                    [
                        (array.len() as u32).to_le_bytes()[0],
                        (table.len() as u32).to_le_bytes()[1],
                        log_2(table.len()).unwrap_or(0).try_into().unwrap(),
                    ],
                ));
                {
                    self.emit(ByteCode::Op(OpCode::Dup));
                    for (i, el) in array.iter().enumerate() {
                        match el {
                            TableField::ArrayEntry(v) => {
                                self.compile_expr(
                                    v,
                                    CompileExprExtra {
                                        n_expect_values: if i + 1 == array.len() {
                                            u8::MAX
                                        } else {
                                            1
                                        },
                                    },
                                )?;
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.emit(ByteCode::Op3U8(
                        OpCode::StoreTableArray,
                        get_3xu8(array.len() as u32),
                    ));
                }
                for field in table {
                    self.emit(ByteCode::Op(OpCode::Dup));
                    match field {
                        TableField::ExprPair(k, v) => {
                            self.compile_expr(v, Default::default())?;
                            self.compile_expr(k, Default::default())?;
                        }
                        TableField::NamePair(k, v) => {
                            let name = match k {
                                Token::Identifier { value, .. } => value,
                                _ => unreachable!(),
                            };
                            self.compile_expr(v, Default::default())?;
                            self.push_string(name);
                        }
                        TableField::ArrayEntry(x) => {
                            unreachable!()
                        }
                    };
                    self.emit(ByteCode::Op(OpCode::RotBCA));
                    self.emit(ByteCode::Op(OpCode::StoreTable));
                }
                Ok(())
            }
        }
    }
    fn add_var(&mut self, var: String) {
        let uid = self.new_var_uid();
        self.symbols.set(
            var,
            VarInfo {
                func_scope: self.funcs.len(),
                location: self.symbols.n_locals as u32,
                is_upvalue: false,
                uid,
                // is_on_stack: true,
            },
        );
        self.symbols.n_locals += 1;
    }
    fn compile_assign(
        &mut self,
        loc: &SourceLocation,
        lhs: &Vec<Rc<Expr>>,
        rhs: &Vec<Rc<Expr>>,
        is_local_var_def: bool,
    ) -> Result<(), CompileError> {
        compile_assign_impl!(
            self,
            loc,
            lhs,
            rhs,
            is_local_var_def,
            |pself: &mut Self, var: &Rc<Expr>| {
                match &**var {
                    Expr::Identifier { token } => {
                        let name = match token {
                            Token::Identifier { value, .. } => value,
                            _ => unreachable!(),
                        };
                        // if let None = self.symbols.get_cur(name) {
                        pself.add_var(name.clone());
                        // }
                    }
                    _ => unreachable!(),
                }
                Ok(())
            },
            |pself: &mut Self, var| { pself.emit_store(var) }
        );
        Ok(())
    }
    fn compile_stmt(&mut self, block: &Stmt) -> Result<(), CompileError> {
        match &*block {
            Stmt::Break { .. } => {
                if self.break_labels.is_empty() {
                    return Err(CompileError {
                        kind: ErrorKind::SemanticError,
                        loc: block.loc().clone(),
                        msg: "break but be contained in loop!".into(),
                    });
                } else {
                    unsafe { self.close_upvalues_this_scope() };
                    self.emit(ByteCode::Op(OpCode::Jump));
                    self.emit(ByteCode::Label(
                        self.break_labels.last().unwrap().to_le_bytes(),
                    ));
                    Ok(())
                }
            }
            Stmt::Return { loc: _, expr } => {
                if expr.len() == 1 {
                    self.compile_expr(&expr[0], Default::default())?;
                    match &*expr[0] {
                        Expr::CallExpr { .. } => {
                            let last = *self.module.code.last().unwrap();
                            match last {
                                ByteCode::Op3U8(OpCode::Call, operands) => {
                                    *self.module.code.last_mut().unwrap() =
                                        ByteCode::Op3U8(OpCode::TailCall, operands);
                                    self.emit(ByteCode::Op(OpCode::Return));
                                    return Ok(());
                                }
                                _ => panic!("expected call instruction"),
                            }
                        }
                        _ => {}
                    }
                } else if expr.is_empty() {
                    self.emit(ByteCode::Op(OpCode::LoadNil));
                } else {
                    for e in expr {
                        self.compile_expr(e, Default::default())?;
                    }
                    self.emit(ByteCode::Op3U8(OpCode::Pack, get_3xu8(expr.len() as u32)));
                }
                self.emit(ByteCode::Op(OpCode::Return));
                Ok(())
            }
            Stmt::LocalVar { loc, vars } => match &**vars {
                Stmt::Assign { lhs, rhs, .. } => {
                    // if lhs.len() >= u8::MAX as usize - 1 || rhs.len() >= u8::MAX as usize - 1 {
                    //     return Err(CompileError {
                    //         loc: loc.clone(),
                    //         kind: ErrorKind::TooManyLocals,
                    //         msg: "number of variables in assignment exceed limit (u8::MAX - 1)"
                    //             .into(),
                    //     });
                    // }
                    // assert!(lhs.len() == 1 && rhs.len() == 1);
                    // self.compile_expr(&*rhs[0], Default::default())?;
                    // for var in lhs {
                    //     match &**var {
                    //         Expr::Identifier { token } => {
                    //             let name = match token {
                    //                 Token::Identifier { value, .. } => value,
                    //                 _ => unreachable!(),
                    //             };
                    //             // if let None = self.symbols.get_cur(name) {
                    //             self.add_var(name.clone());
                    //             // }
                    //         }
                    //         _ => unreachable!(),
                    //     };
                    // }
                    // self.emit_store(&*lhs[0])
                    // let n_vars = lhs.len();
                    // for (i, e) in rhs.iter().enumerate() {
                    //     if i + 1 < rhs.len() {
                    //         self.compile_expr(e, Default::default())?;
                    //     } else {
                    //         if n_vars >= rhs.len() {
                    //             self.compile_expr(
                    //                 e,
                    //                 CompileExprExtra {
                    //                     n_expect_values: (n_vars + 1 - rhs.len()) as u8,
                    //                 },
                    //             )?;
                    //         } else {
                    //             self.compile_expr(e, Default::default())?;
                    //             self.emit(ByteCode::Op(OpCode::Pop));
                    //         }
                    //     }
                    // }
                    // for var in lhs.iter().rev() {
                    //     match &**var {
                    //         Expr::Identifier { token } => {
                    //             let name = match token {
                    //                 Token::Identifier { value, .. } => value,
                    //                 _ => unreachable!(),
                    //             };
                    //             self.add_var(name.clone());
                    //         }
                    //         _ => unreachable!(),
                    //     };
                    //     self.emit_store(var)?;
                    // }
                    // Ok(())
                    self.compile_assign(loc, lhs, rhs, true)
                }
                Stmt::Expr { expr, .. } => match &**expr {
                    Expr::Identifier { token, .. } => {
                        let name = match token {
                            Token::Identifier { value, .. } => value,
                            _ => unreachable!(),
                        };
                        if let None = self.symbols.get(name) {
                            if self.symbols.n_locals >= MAX_LOCALS {
                                return Err(CompileError {
                                    loc: expr.loc().clone(),
                                    kind: ErrorKind::TooManyLocals,
                                    msg: format!(
                                        "exceeds maximum number of {} local varibles",
                                        MAX_LOCALS
                                    ),
                                });
                            }
                            self.add_var(name.clone());
                        }
                        Ok(())
                    }
                    _ => Err(CompileError {
                        loc: expr.loc().clone(),
                        kind: ErrorKind::SemanticError,
                        msg: format!("invalid expression on lhs"),
                    }),
                },
                _ => unreachable!(),
            },
            Stmt::LocalFunction { name, args, body } => {
                match name {
                    Token::Identifier { value, .. } => {
                        self.add_var(value.clone());
                    }
                    _ => unreachable!(),
                }
                self.compile_function(
                    Some(&FunctionName::Function { name: name.clone() }),
                    args,
                    body,
                    FunctionLocation::Local,
                )
            }
            Stmt::If {
                loc,
                cond,
                then,
                else_ifs,
                else_,
            } => {
                self.compile_expr(cond, Default::default())?;
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                // let mut jmp_end = vec![];
                let else_label = self.new_label();
                let end = self.new_label();
                self.emit(ByteCode::Label(else_label.to_le_bytes()));
                self.compile_stmt(then)?;
                self.emit(ByteCode::Op(OpCode::Jump));
                self.emit(ByteCode::Label(end.to_le_bytes()));
                self.emit_label(else_label);
                for (cond, then) in else_ifs {
                    self.compile_expr(cond, Default::default())?;
                    self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                    let else_label = self.new_label();
                    self.emit(ByteCode::Label(else_label.to_le_bytes()));
                    self.compile_stmt(then)?;
                    self.emit(ByteCode::Op(OpCode::Jump));
                    self.emit(ByteCode::Label(end.to_le_bytes()));
                    // jmp_end.push(end);
                    self.emit_label(else_label);
                }

                if let Some(else_) = else_ {
                    self.compile_stmt(else_)?;
                }
                self.emit_label(end);
                // {
                //     for jmp in jmp_end {
                //         self.emit_label(jmp);
                //     }
                // }
                Ok(())
            }
            Stmt::While { loc, cond, body } => {
                // let start = self.module.code.len();
                let begin = self.new_label();
                self.emit_label(begin);
                self.compile_expr(cond, Default::default())?;
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                let l = self.new_label();
                self.break_labels.push(l);
                self.emit(ByteCode::Label(l.to_le_bytes()));
                self.compile_stmt(body)?;
                self.emit(ByteCode::Op(OpCode::Jump));
                self.emit(ByteCode::Label(begin.to_le_bytes()));
                // self.emit(ByteCode::Address((start as u32).to_le_bytes()));
                self.emit_label(l);
                self.break_labels.pop();
                Ok(())
            }
            Stmt::Repeat { loc, cond, body } => todo!(),
            Stmt::For {
                name,
                init,
                end,
                step,
                body,
            } => {
                let var = match name {
                    Token::Identifier { value, .. } => value,
                    _ => {
                        return Err(CompileError {
                            kind: ErrorKind::SemanticError,
                            msg: "illegal induction variable".into(),
                            loc: name.loc().clone(),
                        })
                    }
                };
                self.enter_scope(false);
                self.add_var(var.clone());
                self.compile_expr(init, Default::default())?;
                self.store_variable(var)?;
                let step_var = format!("##step{}", self.module.code.len());
                self.add_var(step_var.clone());
                if let Some(step) = step {
                    self.compile_expr(step, Default::default())?;
                } else {
                    self.push_number(1.0);
                }
                self.emit(ByteCode::Op(OpCode::Dup));
                self.store_variable(&step_var)?;
                macro_rules! gen_for_loop {
                    ($op:ident) => {
                        let begin = self.new_label();
                        self.emit_label(begin);
                        self.load_identifier(var)?;
                        self.compile_expr(end, Default::default())?;
                        self.emit(ByteCode::Op(OpCode::$op));
                        self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                        let l = self.new_label();
                        self.emit(ByteCode::Label(l.to_le_bytes()));
                        self.enter_scope(false);
                        self.compile_stmt(body)?;
                        self.leave_scope(false);
                        self.load_identifier(var)?;
                        self.load_identifier(&step_var)?;
                        self.emit(ByteCode::Op(OpCode::Add));
                        self.store_variable(var)?;
                        self.emit(ByteCode::Op(OpCode::Jump));
                        self.emit(ByteCode::Label(begin.to_le_bytes()));
                        self.emit_label(l);
                    };
                }

                self.push_number(0.0);
                self.emit(ByteCode::Op(OpCode::GreaterThan));
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                let end = self.new_label();
                self.break_labels.push(end);
                let l = self.new_label();
                self.emit(ByteCode::Label(l.to_le_bytes()));
                gen_for_loop!(LessThanEqual);
                self.emit(ByteCode::Op(OpCode::Jump));
                self.emit(ByteCode::Label(end.to_le_bytes()));
                self.emit_label(l);
                gen_for_loop!(GreaterThanEqual);
                self.emit_label(end);
                self.leave_scope(false);
                self.break_labels.pop();
                Ok(())
            }
            Stmt::ForIn { vars, range, body } => {
                let f_var = format!("##_f{}", self.module.code.len());
                let s_var = format!("##_s{}", self.module.code.len());
                let ctrl_var = format!("##_var{}", self.module.code.len());
                self.enter_scope(false);
                let loc = block.loc();
                let mut varnames = vec![];
                let mut varnames_original = vec![];
                for var in vars {
                    match &**var {
                        Expr::Identifier { token } => match token {
                            Token::Identifier { value, .. } => {
                                varnames_original.push(value);
                                varnames.push(format!("##_{}_{}", value, self.module.code.len()));
                                self.add_var(varnames.last().unwrap().clone());
                            }
                            _ => unreachable!(),
                        },
                        _ => {
                            return Err(CompileError {
                                loc: loc.clone(),
                                kind: ErrorKind::SemanticError,
                                msg: "generic for loop expect identifiers".into(),
                            });
                        }
                    }
                }
                //  local _f, _s, _var = explist
                compile_assign_impl!(
                    self,
                    loc,
                    &vec![&f_var, &s_var, &ctrl_var],
                    range,
                    true,
                    |pself: &mut Self, var: &String| {
                        pself.add_var(var.clone());
                        Ok(())
                    },
                    |pself: &mut Self, var| { pself.store_variable(var) }
                );

                let end = self.new_label();
                let begin = self.new_label();
                self.break_labels.push(end);
                self.emit_label(begin);
                self.load_identifier(&ctrl_var)?;
                self.load_identifier(&s_var)?;
                self.load_identifier(&f_var)?;
                self.emit(ByteCode::Op3U8(OpCode::Call, [2, vars.len() as u8, 0])); // local var_1, ... , var_n = _f(_s, _var)
                self.enter_scope(false);
                for (i, var) in vars.iter().enumerate().rev() {
                    self.add_var(varnames_original[i].clone());
                    self.emit(ByteCode::Op(OpCode::Dup));
                    self.store_variable(&varnames[i])?;
                    self.emit_store(var)?;
                }
                self.compile_expr(&vars[0], Default::default())?;
                self.emit(ByteCode::Op(OpCode::Dup));
                self.store_variable(&ctrl_var)?;
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1])); //  if _var == nil then break end
                self.emit(ByteCode::Label(end.to_le_bytes()));

                self.enter_scope(false);
                self.compile_stmt(body)?;
                self.leave_scope(false);
                self.leave_scope(false);
                self.emit(ByteCode::Op(OpCode::Jump));
                self.emit(ByteCode::Label(begin.to_le_bytes()));
                self.emit_label(end);
                self.leave_scope(false);
                self.break_labels.pop();
                Ok(())
            }
            Stmt::Assign { loc, lhs, rhs } => {
                // assert!(lhs.len() == 1 && rhs.len() == 1);
                // self.compile_expr(&*rhs[0], Default::default())?;
                // self.emit_store(&*lhs[0])
                self.compile_assign(loc, lhs, rhs, false)
            }
            Stmt::Expr { loc, expr } => {
                self.compile_expr(expr, Default::default())?;
                self.emit(ByteCode::Op(OpCode::Pop));
                Ok(())
            }
            Stmt::Block { loc, stmts } => {
                self.enter_scope(false);
                for stmt in stmts {
                    self.compile_stmt(&**stmt)?;
                }
                self.leave_scope(false);
                Ok(())
            }
            Stmt::Function { name, args, body } => {
                self.compile_function(Some(name), args, body, FunctionLocation::Global)
            }
        }
    }
    fn new_var_uid(&mut self) -> u32 {
        let id = self.var_uid_gen;
        self.var_uid_gen += 1;
        id
    }
    fn compile_function(
        &mut self,
        name: Option<&FunctionName>,
        args: &Vec<Token>,
        body: &Stmt,
        location: FunctionLocation,
    ) -> Result<(), CompileError> {
        self.emit(ByteCode::Op(OpCode::Jump));
        let end = self.new_label();
        self.emit(ByteCode::Label(end.to_le_bytes()));
        let entry = self.module.code.len() as u32;
        self.enter_scope(true);
        let tmp = std::mem::replace(&mut self.break_labels, vec![]);
        if let None = self.symbols.get(&"_ENV".into()) {
            let env_uid = self.new_var_uid();
            self.symbols.set(
                "_ENV".into(),
                VarInfo {
                    func_scope: self.funcs.len(),
                    location: 0,
                    uid: env_uid,
                    is_upvalue: true,
                },
            );
            let func = self.funcs.last_mut().unwrap();
            func.upvalues.insert(
                env_uid,
                UpValueInfo {
                    from_parent: true,
                    id: 0,
                    uid: env_uid,
                    location: 0,
                    is_special: false,
                    name: "_ENV".into(),
                },
            );
        } else {
            self.resolve_upvalue(&"_ENV".into());
            let info = self.symbols.get(&"_ENV".into()).unwrap();
            assert!(info.is_upvalue);
            let func = self.funcs.last().unwrap();
            let info = func.upvalues.get(&info.uid).unwrap();
            assert_eq!(info.location, 0);
            assert!(info.from_parent);
            assert!(info.id == 0);
        }
        let mut arg_offset = 0;
        match location {
            FunctionLocation::Global => match name.unwrap() {
                FunctionName::Method {
                    access_chain: _,
                    method,
                } => {
                    if method.is_some() {
                        arg_offset = 1;
                        let uid = self.new_var_uid();
                        self.symbols.set(
                            "self".into(),
                            VarInfo {
                                func_scope: self.funcs.len(),
                                location: 0,
                                uid,
                                is_upvalue: false,
                            },
                        );
                    }
                }
                _ => {}
            },
            _ => {}
        }
        let mut has_varargs = false;
        for (i, arg) in args.iter().enumerate() {
            let name = match arg {
                Token::Identifier { value, .. } => value,
                Token::Symbol { value, .. } => {
                    debug_assert!(value == "..." && i == args.len() - 1);
                    has_varargs = true;
                    value
                }
                _ => unreachable!(),
            };
            let uid = self.new_var_uid();
            self.symbols.set(
                name.clone(),
                VarInfo {
                    func_scope: self.funcs.len(),
                    location: i as u32 + arg_offset,
                    uid,
                    is_upvalue: false,
                },
            );
        }
        self.symbols.n_locals = arg_offset as usize + args.len();
        self.compile_stmt(body)?;
        match location {
            FunctionLocation::Repl => match *self.module.code.last().unwrap() {
                ByteCode::Op(OpCode::Pop) => {
                    self.module.code.pop().unwrap();
                }
                _ => {
                    self.emit(ByteCode::Op(OpCode::LoadNil));
                }
            },
            _ => {
                self.emit(ByteCode::Op(OpCode::LoadNil));
            }
        }

        self.emit(ByteCode::Op(OpCode::Return));
        let proto = ClosurePrototype {
            n_args: if has_varargs {
                args.len() - 1 + arg_offset as usize
            } else {
                args.len() + arg_offset as usize
            },
            has_varargs,
            upvalues: {
                let mut tmp: Vec<_> = self
                    .funcs
                    .last()
                    .unwrap()
                    .upvalues
                    .iter()
                    .map(|(_, info)| info.clone())
                    .collect();
                tmp.sort_by(|a, b| a.id.partial_cmp(&b.id).unwrap());
                tmp
            },
            entry: entry as usize,
        };
        self.leave_scope(true);
        let proto_idx = self.module.prototypes.len() as u32;
        self.module.prototypes.push(proto);
        self.emit_label(end);
        self.emit(ByteCode::Op3U8(OpCode::NewClosure, get_3xu8(proto_idx)));
        self.emit(ByteCode::Address(entry.to_le_bytes()));
        match location {
            FunctionLocation::Global => match name.unwrap() {
                FunctionName::Method {
                    access_chain,
                    method,
                } => {
                    let access_chain: Vec<_> = access_chain
                        .iter()
                        .map(|x| match x {
                            Token::Identifier { value, .. } => value,
                            _ => unreachable!(),
                        })
                        .collect();
                    let method = method.as_ref().map(|x| match x {
                        Token::Identifier { value, .. } => value,
                        _ => unreachable!(),
                    });
                    // if let Some(method) = method {
                    //     self.push_string(method);
                    // }
                    // for (_i, acc) in access_chain.iter().enumerate().rev() {
                    //     self.push_string(acc);
                    // }
                    // {
                    //     let info = *self.symbols.get(&"_ENV".into()).unwrap();
                    //     if info.is_upvalue {
                    //         self.emit(ByteCode::Op3U8(
                    //             OpCode::LoadUpvalue,
                    //             get_3xu8(info.location),
                    //         ));
                    //     } else {
                    //         self.emit(ByteCode::Op3U8(OpCode::LoadLocal, get_3xu8(info.location)));
                    //     }
                    // }
                    self.load_identifier(&access_chain[0])?;
                    if let Some(method) = method {
                        assert!(access_chain.len() > 0);
                        for (_i, acc) in access_chain.iter().enumerate().skip(1) {
                            let idx = self.string_pool_index(*acc);
                            self.emit(ByteCode::Op3U8(OpCode::LoadTableStringKey, get_3xu8(idx)));
                        }
                        let idx = self.string_pool_index(method);
                        self.emit(ByteCode::Op3U8(OpCode::StoreTableStringKey, get_3xu8(idx)));
                    } else {
                        assert!(access_chain.len() > 1);
                        for (i, acc) in access_chain.iter().enumerate().skip(1) {
                            let idx = self.string_pool_index(*acc);
                            if i != access_chain.len() - 1 {
                                self.emit(ByteCode::Op3U8(
                                    OpCode::LoadTableStringKey,
                                    get_3xu8(idx),
                                ));
                            } else {
                                self.emit(ByteCode::Op3U8(
                                    OpCode::StoreTableStringKey,
                                    get_3xu8(idx),
                                ));
                            }
                        }
                    }
                }
                FunctionName::Function { name } => match name {
                    Token::Identifier { value, .. } => {
                        self.push_string(value);
                        let info = *self.symbols.get(&"_ENV".into()).unwrap();
                        if info.is_upvalue {
                            self.emit(ByteCode::Op3U8(
                                OpCode::LoadUpvalue,
                                get_3xu8(info.location),
                            ));
                        } else {
                            self.emit(ByteCode::Op3U8(OpCode::LoadLocal, get_3xu8(info.location)));
                        }
                        self.emit(ByteCode::Op(OpCode::StoreTable));
                    }
                    _ => unreachable!(),
                },
            },
            FunctionLocation::Local => match name.unwrap() {
                FunctionName::Function {
                    name: Token::Identifier { value, .. },
                } => {
                    self.resolve_upvalue(&value);
                    let info = *self.symbols.get(value).unwrap();
                    if info.is_upvalue {
                        self.emit(ByteCode::Op3U8(
                            OpCode::StoreUpvalue,
                            get_3xu8(info.location),
                        ));
                    } else {
                        self.emit(ByteCode::Op3U8(OpCode::StoreLocal, get_3xu8(info.location)));
                    }
                }
                _ => unreachable!(),
            },
            FunctionLocation::Stack => {}
            FunctionLocation::Repl => {}
        };
        self.break_labels = tmp;
        Ok(())
    }
    fn store_variable(&mut self, name: &String) -> Result<(), CompileError> {
        let info = self.symbols.get(name).map(|x| *x);
        self.resolve_upvalue(name);
        if let Some(info) = info {
            if info.is_upvalue {
                self.emit(ByteCode::Op3U8(
                    OpCode::StoreUpvalue,
                    get_3xu8(info.location),
                ));
            } else {
                self.emit(ByteCode::Op3U8(OpCode::StoreLocal, get_3xu8(info.location)));
            }
        } else {
            // self.push_string(name);
            let info = *self.symbols.get(&"_ENV".into()).unwrap();
            if info.is_upvalue {
                self.emit(ByteCode::Op3U8(
                    OpCode::LoadUpvalue,
                    get_3xu8(info.location),
                ));
            } else {
                self.emit(ByteCode::Op3U8(OpCode::LoadLocal, get_3xu8(info.location)));
            }
            let idx = self.string_pool_index(name);
            self.emit(ByteCode::Op3U8(OpCode::StoreTableStringKey, get_3xu8(idx)));
        }
        Ok(())
    }
    fn emit_store(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            // Expr::Const { token } => todo!(),
            // Expr::Literal { token } => todo!(),
            Expr::Identifier { token } => {
                let name = match token {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.store_variable(name)
            }
            // Expr::BinaryExpr { op, lhs, rhs } => todo!(),
            // Expr::UnaryExpr { op, arg } => todo!(),
            Expr::IndexExpr { loc: _, lhs, rhs } => {
                self.compile_expr(rhs, Default::default())?;
                self.compile_expr(lhs, Default::default())?;
                self.emit(ByteCode::Op(OpCode::StoreTable));
                Ok(())
            }
            Expr::DotExpr {
                loc: _,
                lhs,
                rhs: token,
            } => {
                let name = match token {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.push_string(name);

                self.compile_expr(lhs, Default::default())?;
                self.emit(ByteCode::Op(OpCode::StoreTable));
                Ok(())
            }
            // Expr::CallExpr { callee, args } => todo!(),
            // Expr::MethodCallExpr { callee, method, args } => todo!(),
            // Expr::FunctionExpr { loc, args, body } => ,
            // Expr::Table { loc, fields } => todo!(),
            _ => Err(CompileError {
                loc: expr.loc().clone(),
                kind: ErrorKind::SemanticError,
                msg: format!("invalid expression on lhs"),
            }),
        }
    }
    fn resolve_labels(&mut self) {
        for (_i, inst) in self.module.code.iter_mut().enumerate() {
            match *inst {
                ByteCode::Label(label) => {
                    let level = u32::from_le_bytes(label);
                    let addr = *self.labels.get(&level).unwrap() as u32;
                    *inst = ByteCode::Address(addr.to_le_bytes());
                }
                _ => {}
            }
        }
    }
    fn run(&mut self, block: Rc<Stmt>, is_eval: bool) -> Result<ByteCodeModule, CompileError> {
        // self.enter_scope(true);
        // {
        //     let env_uid = self.new_var_uid();
        //     self.symbols.set(
        //         "_ENV".into(),
        //         VarInfo {
        //             func_scope: self.funcs.len(),
        //             location: 0,
        //             uid: env_uid,
        //             is_upvalue: true,
        //         },
        //     );
        // }

        // self.compile_stmt(&*block)?;
        // self.leave_scope(true);
        self.compile_function(
            None,
            &vec![],
            &*block,
            if is_eval {
                FunctionLocation::Repl
            } else {
                FunctionLocation::Stack
            },
        )?;
        self.emit(ByteCode::Op3U8(OpCode::Call, [0, 1, 0]));
        self.resolve_labels();
        let module = std::mem::replace(
            &mut self.module,
            ByteCodeModule {
                // debug_info: ModuleDebugInfo::new(),
                string_pool_cache: vec![],
                prototypes: vec![],
                code: vec![],
                string_pool: vec![],
            },
        );
        Ok(module)
    }
    fn enter_scope(&mut self, function_scope: bool) {
        let new = SymbolTable {
            vars: HashMap::new(),
            parent: std::ptr::null_mut(),
            func_scope: if function_scope {
                self.funcs.len() + 1
            } else {
                self.funcs.len()
            },
            n_locals: if function_scope {
                0
            } else {
                self.symbols.n_locals
            },
        };
        let old = std::mem::replace(&mut self.symbols, new);
        let old = Box::into_raw(Box::new(old));
        self.symbols.parent = old;
        if function_scope {
            self.funcs.push(FuncInfo {
                upvalues: HashMap::new(),
            });
        }
    }
    unsafe fn close_upvalues_this_scope(&mut self) {
        let mut upvalues_to_close = vec![];
        for (_var, info) in &self.symbols.vars {
            let upvalues = &self.funcs.last().unwrap().upvalues;

            if let Some(upvalue_info) = upvalues.get(&info.uid) {
                // println!("is_upvalue {}", var);
                // println!("from_parent {} {}", var, upvalue_info.from_parent);
                if !upvalue_info.from_parent {
                    // println!("fuck {}", var);
                    upvalues_to_close.push(upvalue_info.id);
                }
            }
        }
        for v in upvalues_to_close {
            self.emit(ByteCode::Op3U8(OpCode::CloseUpvalue, [v as u8, 0, 0]));
        }
    }

    fn leave_scope(&mut self, function_scope: bool) {
        unsafe {
            let n_locals = self.symbols.n_locals;
            if !function_scope {
                self.close_upvalues_this_scope();
            }

            let parent = self.symbols.parent;
            std::mem::swap(&mut self.symbols, parent.as_mut().unwrap());
            Box::from_raw(parent);
            if !function_scope {
                self.symbols.n_locals = n_locals;
            }
        }
        if function_scope {
            self.funcs.pop().unwrap();
        }
    }
}

pub fn compile(block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
    let mut compiler = Compiler {
        funcs: vec![],
        var_uid_gen: 0,
        label_gen: 0,
        labels: HashMap::new(),
        symbols: SymbolTable {
            vars: HashMap::new(),
            parent: std::ptr::null_mut(),
            func_scope: 0,
            n_locals: 0,
        },
        break_labels: vec![],
        module: ByteCodeModule {
            // debug_info: ModuleDebugInfo::new(),
            string_pool_cache: vec![],
            prototypes: vec![],
            code: vec![],
            string_pool: vec![],
        },
        str_map: HashMap::new(),
    };
    compiler.run(block, false)
}

pub fn compile_repl(block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
    let mut compiler = Compiler {
        funcs: vec![],
        var_uid_gen: 0,
        label_gen: 0,
        labels: HashMap::new(),
        symbols: SymbolTable {
            vars: HashMap::new(),
            parent: std::ptr::null_mut(),
            func_scope: 0,
            n_locals: 0,
        },
        break_labels: vec![],
        module: ByteCodeModule {
            // debug_info: ModuleDebugInfo::new(),
            string_pool_cache: vec![],
            prototypes: vec![],
            code: vec![],
            string_pool: vec![],
        },
        str_map: HashMap::new(),
    };
    compiler.run(block, true)
}
