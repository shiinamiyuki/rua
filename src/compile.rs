use std::{
    cell::{Ref, RefCell},
    collections::{HashMap, HashSet},
    convert::TryInto,
    rc::Rc,
    vec,
};

use crate::{
    bytecode::*,
    closure::ClosurePrototype,
    log_2,
    parse::{Expr, FunctionName, SourceLocation, Stmt, TableField, Token},
    state::MAX_LOCALS,
};

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
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
}
#[derive(Clone, Copy, Debug)]
pub(crate) struct UpValueInfo {
    pub(crate) from_parent: bool,
    pub(crate) id: u32,
    pub(crate) uid: u32,
    pub(crate) location: u32,
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
}

impl Compiler {
    fn emit(&mut self, inst: ByteCode) -> usize {
        let ret = self.module.code.len();
        self.module.code.push(inst);
        ret
    }
    fn push_number(&mut self, n: f64) {
        let bytes = n.to_le_bytes();

        self.emit(ByteCode::Op(OpCode::LoadNumber));
        self.emit(ByteCode::FloatLo([bytes[0], bytes[1], bytes[2], bytes[3]]));
        self.emit(ByteCode::FloatHi([bytes[4], bytes[5], bytes[6], bytes[7]]));
    }
    fn push_string(&mut self, s: &String) {
        let idx = if let Some(idx) = self.str_map.get(s) {
            *idx
        } else {
            let idx = self.module.string_pool.len();
            self.module.string_pool.push(s.clone());
            self.str_map.insert(s.clone(), idx);
            idx
        } as u32;
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
                                    },
                                );
                                tab.set(
                                    name.clone(),
                                    VarInfo {
                                        func_scope: tab.func_scope,
                                        location,
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

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
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
                    self.push_string(name);
                    self.emit(ByteCode::Op(OpCode::LoadGlobal));
                }
                Ok(())
            }
            Expr::BinaryExpr { op, lhs, rhs } => {
                let opcode = match op {
                    Token::Keyword { value, .. } => match value.as_str() {
                        "and" => {
                            self.compile_expr(lhs)?;
                            self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 0, 1]));
                            let jmp = self.emit(ByteCode::Address([0; 4]));
                            self.compile_expr(rhs)?;
                            self.module.code[jmp] = ByteCode::Address(
                                (self.module.code.len() as u32 - 1).to_le_bytes(),
                            );
                            return Ok(());
                        }
                        "or" => {
                            self.compile_expr(lhs)?;
                            self.emit(ByteCode::Op3U8(OpCode::TestJump, [1, 0, 1]));
                            let jmp = self.emit(ByteCode::Address([0; 4]));
                            self.compile_expr(rhs)?;
                            self.module.code[jmp] = ByteCode::Address(
                                (self.module.code.len() as u32 - 1).to_le_bytes(),
                            );
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
                        "<" => OpCode::LessThan,
                        "<=" => OpCode::LessThanEqual,
                        ">=" => OpCode::GreaterThanEqual,
                        ">" => OpCode::GreaterThan,
                        "==" => OpCode::Equal,
                        "!=" => OpCode::NotEqual,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;
                self.emit(ByteCode::Op(opcode));
                Ok(())
            }
            Expr::UnaryExpr { op, arg } => {
                self.compile_expr(arg)?;
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
                self.compile_expr(rhs)?;
                self.compile_expr(lhs)?;
                self.emit(ByteCode::Op(OpCode::LoadTable));
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

                self.compile_expr(lhs)?;
                self.emit(ByteCode::Op(OpCode::LoadTable));
                Ok(())
            }
            Expr::CallExpr { callee, args } => {
                for arg in args {
                    self.compile_expr(&**arg)?;
                }
                self.compile_expr(&*callee)?;
                self.emit(ByteCode::Op3U8(OpCode::Call, [args.len() as u8, 0, 0]));
                Ok(())
            }
            Expr::MethodCallExpr {
                callee,
                method,
                args,
            } => todo!(),
            Expr::FunctionExpr { loc, args, body } => {
                self.compile_function(None, args, body, FunctionLocation::Stack)
            }
            Expr::Table { loc: _, fields } => {
                let n_arrays = fields
                    .iter()
                    .filter(|f| match f {
                        TableField::ArrayEntry(_) => true,
                        _ => false,
                    })
                    .count();
                let n_hash = fields.len() - n_arrays;
                let n_arrays: u16 = n_arrays.min(u16::MAX as usize).try_into().unwrap();
                self.emit(ByteCode::Op3U8(
                    OpCode::NewTable,
                    [
                        n_arrays.to_le_bytes()[0],
                        n_arrays.to_le_bytes()[1],
                        log_2(n_hash).unwrap_or(0).try_into().unwrap(),
                    ],
                ));
                let mut array_entry_cnt = 1;
                for field in fields {
                    self.emit(ByteCode::Op(OpCode::Dup));
                    match field {
                        TableField::ExprPair(k, v) => {
                            self.compile_expr(v)?;
                            self.compile_expr(k)?;
                        }
                        TableField::NamePair(k, v) => {
                            let name = match k {
                                Token::Identifier { value, .. } => value,
                                _ => unreachable!(),
                            };
                            self.compile_expr(v)?;
                            self.push_string(name);
                        }
                        TableField::ArrayEntry(x) => {
                            self.compile_expr(x)?;
                            self.push_number(array_entry_cnt as f64);
                            array_entry_cnt += 1;
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
    fn compile_stmt(&mut self, block: &Stmt) -> Result<(), CompileError> {
        match &*block {
            Stmt::Return { loc: _, expr } => {
                if let Some(expr) = expr {
                    self.compile_expr(&*expr)?;
                    match &**expr {
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
                } else {
                    self.emit(ByteCode::Op(OpCode::LoadNil));
                }
                self.emit(ByteCode::Op(OpCode::Return));
                Ok(())
            }
            Stmt::LocalVar { loc: _, vars } => match &**vars {
                Stmt::Assign { lhs, rhs, .. } => {
                    assert!(lhs.len() == 1 && rhs.len() == 1);
                    self.compile_expr(&*rhs[0])?;
                    for var in lhs {
                        match &**var {
                            Expr::Identifier { token } => {
                                let name = match token {
                                    Token::Identifier { value, .. } => value,
                                    _ => unreachable!(),
                                };
                                if let None = self.symbols.get(name) {
                                    self.add_var(name.clone());
                                }
                            }
                            _ => unreachable!(),
                        };
                    }
                    self.emit_store(&*lhs[0])
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
                self.compile_expr(cond)?;
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                let mut jmp_end = vec![];
                let jmp = self.emit(ByteCode::Address([0; 4]));
                self.compile_stmt(then)?;
                for (cond, then) in else_ifs {
                    self.compile_expr(cond)?;
                    self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                    let jmp_then_end = self.emit(ByteCode::Address([0; 4]));
                    self.compile_stmt(then)?;
                    self.emit(ByteCode::Op(OpCode::Jump));
                    let jmp = self.emit(ByteCode::Address([0; 4]));
                    jmp_end.push(jmp);
                    self.module.code[jmp_then_end] =
                        ByteCode::Address((self.module.code.len() as u32).to_le_bytes());
                }
                self.module.code[jmp] =
                    ByteCode::Address((self.module.code.len() as u32).to_le_bytes());
                if let Some(else_) = else_ {
                    self.compile_stmt(else_)?;
                }
                {
                    let end = self.module.code.len() as u32;
                    for jmp in jmp_end {
                        assert!(self.module.code[jmp] == ByteCode::Address([0; 4]));
                        self.module.code[jmp] = ByteCode::Address(end.to_le_bytes());
                    }
                }
                Ok(())
            }
            Stmt::While { loc, cond, body } => {
                let start = self.module.code.len();
                self.compile_expr(cond)?;
                self.emit(ByteCode::Op3U8(OpCode::TestJump, [0, 1, 1]));
                let jmp_endloop = self.emit(ByteCode::Address([0; 4]));
                self.compile_stmt(body)?;
                self.emit(ByteCode::Op(OpCode::Jump));
                self.emit(ByteCode::Address((start as u32).to_le_bytes()));
                self.module.code[jmp_endloop] =
                    ByteCode::Address((self.module.code.len() as u32).to_le_bytes());
                Ok(())
            }
            Stmt::Repeat { loc, cond, body } => todo!(),
            Stmt::For {
                name,
                init,
                end,
                step,
                body,
            } => todo!(),
            Stmt::ForIn { name, range, body } => todo!(),
            Stmt::Assign { loc, lhs, rhs } => {
                assert!(lhs.len() == 1 && rhs.len() == 1);
                self.compile_expr(&*rhs[0])?;
                self.emit_store(&*lhs[0])
            }
            Stmt::Expr { loc, expr } => {
                self.compile_expr(expr)?;
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
        let jmp = self.emit(ByteCode::Address([0; 4]));
        let entry = self.module.code.len() as u32;
        self.enter_scope(true);
        for (i, arg) in args.iter().enumerate() {
            let name = match arg {
                Token::Identifier { value, .. } => value,
                _ => unreachable!(),
            };
            let uid = self.new_var_uid();
            self.symbols.set(
                name.clone(),
                VarInfo {
                    func_scope: self.funcs.len(),
                    location: i as u32,
                    uid,
                    is_upvalue: false,
                },
            );
        }
        self.symbols.n_locals = args.len();
        self.compile_stmt(body)?;
        self.emit(ByteCode::Op(OpCode::LoadNil));
        self.emit(ByteCode::Op(OpCode::Return));
        let proto = ClosurePrototype {
            n_args: args.len(),
            upvalues: {
                let mut tmp: Vec<_> = self
                    .funcs
                    .last()
                    .unwrap()
                    .upvalues
                    .iter()
                    .map(|(_, info)| *info)
                    .collect();
                tmp.sort_by(|a, b| a.id.partial_cmp(&b.id).unwrap());
                tmp
            },
            entry: entry as usize,
        };
        self.leave_scope(true);
        let proto_idx = self.module.protypes.len() as u32;
        self.module.protypes.push(proto);
        let end = self.emit(ByteCode::Op3U8(OpCode::NewClosure, get_3xu8(proto_idx))) as u32;
        self.emit(ByteCode::Address(entry.to_le_bytes()));
        self.module.code[jmp] = ByteCode::Address(end.to_le_bytes());
        match location {
            FunctionLocation::Global => match name.unwrap() {
                FunctionName::Method {
                    access_chain: _,
                    method: _,
                } => todo!(),
                FunctionName::Function { name } => match name {
                    Token::Identifier { value, .. } => {
                        self.push_string(value);
                        self.emit(ByteCode::Op(OpCode::StoreGlobal));
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
        };
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
                    self.push_string(name);
                    self.emit(ByteCode::Op(OpCode::StoreGlobal));
                }
                Ok(())
            }
            // Expr::BinaryExpr { op, lhs, rhs } => todo!(),
            // Expr::UnaryExpr { op, arg } => todo!(),
            Expr::IndexExpr { loc:_, lhs, rhs } => {
                self.compile_expr(rhs)?;
                self.compile_expr(lhs)?;
                self.emit(ByteCode::Op(OpCode::StoreTable));
                Ok(())
            }
            Expr::DotExpr {
                loc:_,
                lhs,
                rhs: token,
            } => {
                let name = match token {
                    Token::Identifier { value, .. } => value,
                    _ => unreachable!(),
                };
                self.push_string(name);

                self.compile_expr(lhs)?;
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
    fn run(&mut self, block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
        self.compile_stmt(&*block)?;
        let module = std::mem::replace(
            &mut self.module,
            ByteCodeModule {
                protypes: vec![],
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
    fn leave_scope(&mut self, function_scope: bool) {
        unsafe {
            let parent = self.symbols.parent;
            std::mem::swap(&mut self.symbols, parent.as_mut().unwrap());
            Box::from_raw(parent);
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
        symbols: SymbolTable {
            vars: HashMap::new(),
            parent: std::ptr::null_mut(),
            func_scope: 0,
            n_locals: 0,
        },
        module: ByteCodeModule {
            protypes: vec![],
            code: vec![],
            string_pool: vec![],
        },
        str_map: HashMap::new(),
    };
    compiler.run(block)
}

pub fn compile_repl(block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
    let mut compiler = Compiler {
        funcs: vec![],
        var_uid_gen: 0,
        symbols: SymbolTable {
            vars: HashMap::new(),
            parent: std::ptr::null_mut(),
            func_scope: 0,
            n_locals: 0,
        },
        module: ByteCodeModule {
            protypes: vec![],
            code: vec![],
            string_pool: vec![],
        },
        str_map: HashMap::new(),
    };
    let mut module = compiler.run(block)?;
    match *module.code.last().unwrap() {
        ByteCode::Op(OpCode::Pop) => {
            module.code.pop().unwrap();
        }
        _ => {}
    }
    Ok(module)
}
