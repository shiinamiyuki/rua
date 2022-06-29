use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    convert::TryInto,
    hash::Hash,
    ops::Range,
    rc::Rc,
    vec,
};

use crate::{
    get_3xu8,
    parse::{Expr, FunctionName, SourceLocation, Stmt, Token},
    u32_from_3xu8,
    vm::{BinaryOp, ByteCodeModule, CompiledFunction, Instruction, LoadConst, MAX_REG},
};

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    ParseOrTokenizeError,
    NotEnoughRegisters,
    OutOfMemoryError,
    SemanticError,
    InternalError,
    NameError,
}

#[derive(Clone, Debug)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub msg: String,
    pub loc: SourceLocation,
}
#[derive(Clone, Copy, Debug)]
pub(crate) enum UpValueInfo {
    Parent { id: u32, location: u32 },
    Child { id: u32, parent_location: u32 },
}
#[derive(Clone, Copy, Debug)]
struct VarInfo {
    func_scope: usize,
    location: u32,
    uid: u32,
    is_onstack: bool,
    is_upvalue: Option<UpValueInfo>,
    // is_optional: bool,
    is_const: bool,
}

struct SymbolTableScope {
    vars: HashMap<String, VarInfo>,
}
impl SymbolTableScope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Reg {
    Unused,
    Local,
    Used,
}

struct RegAllocator {
    regs: [Reg; MAX_REG],
    max_reg_usized: usize,
}

impl RegAllocator {
    // allocate n contiguous registers
    fn allocate(&mut self, n: usize) -> Option<Range<usize>> {
        for i in 0..MAX_REG {
            if (i..i + n).all(|x| self.regs[x] == Reg::Unused) {
                for j in i..i + n {
                    self.regs[j] = Reg::Used;
                }
                self.max_reg_usized = self.max_reg_usized.max(i + n);
                return Some(i..i + n);
            }
        }
        None
    }
    fn free_all(&mut self) {
        for r in &mut self.regs {
            if *r != Reg::Local {
                *r = Reg::Unused;
            }
        }
    }
    fn free_one(&mut self, i: usize) {
        if self.regs[i] != Reg::Local {
            self.regs[i] = Reg::Unused;
        }
    }
    fn free(&mut self, range: Range<usize>) {
        for i in range {
            assert!(self.regs[i] == Reg::Used);
            self.regs[i] = Reg::Unused;
        }
    }
}

struct PerFunctionSymbolTable {
    scopes: Vec<SymbolTableScope>,
    nlocals: usize,
    upvalues: Vec<UpValueInfo>,
    reg_alloc: RegAllocator,
}

impl PerFunctionSymbolTable {
    fn new() -> Self {
        Self {
            nlocals: 0,
            upvalues: vec![],
            scopes: vec![SymbolTableScope::new()],
            reg_alloc: RegAllocator {
                regs: [Reg::Unused; MAX_REG],
                max_reg_usized: 0,
            },
        }
    }
    fn push(&mut self) {
        self.scopes.push(SymbolTableScope::new())
    }
    fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }
    fn get_mut<'a>(&'a mut self, var: &String) -> Option<&'a mut VarInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.vars.get_mut(var) {
                return Some(info);
            }
        }
        None
    }
    fn get(&self, var: &String) -> Option<VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.vars.get(var) {
                return Some(*info);
            }
        }
        None
    }
    fn add(&mut self, var: String, info: VarInfo) {
        self.scopes.last_mut().unwrap().vars.insert(var, info);
    }
}
thread_local! {
    static UID_GEN:Cell<u32> = Cell::new(0);
}
fn gen_uid() -> u32 {
    UID_GEN.with(|c| {
        let id = c.get();
        c.set(id + 1);
        id
    })
}
struct SymbolTable {
    functions: Vec<PerFunctionSymbolTable>,
}
impl SymbolTable {
    fn allocator(&mut self) -> &mut RegAllocator {
        &mut self.functions.last_mut().unwrap().reg_alloc
    }
    fn get_mut<'a>(&'a mut self, var: &String) -> Option<&'a mut VarInfo> {
        self.functions.last_mut().unwrap().get_mut(var)
    }
    fn get(&self, var: &String) -> Option<VarInfo> {
        self.functions.last().unwrap().get(var)
    }
    fn add_var(&mut self, var: String) -> u8 {
        let scope = self.functions.len();
        let f = self.functions.last_mut().unwrap();
        f.add(
            var,
            VarInfo {
                is_onstack: true,
                func_scope: scope,
                location: f.nlocals as u32,
                uid: gen_uid(),
                is_upvalue: None,
                is_const: false,
            },
        );
        let r = f.nlocals as u8;
        f.nlocals += 1;
        r
    }
}

struct Compiler {
    cur_function: Option<CompiledFunction>,
    string_cache: HashMap<String, usize>,
    module: Option<ByteCodeModule>,
    symbols: SymbolTable,
    labels: HashMap<u32, u32>,
    label_cnt: u32,
    loop_labels: Vec<(u32, u32)>,
}

#[derive(Clone, Copy)]
struct CompileExprArgs {
    // optional_unwrap_label: Option<u32>,
    result_reg: Option<usize>,
}
impl Default for CompileExprArgs {
    fn default() -> Self {
        Self { result_reg: None }
    }
}

#[derive(Clone, Debug)]
enum LValue {
    Local(u8),
    UpValue(u32),
    Global(String),
    TableAssign(u8, u8),
    TableKeyAssign(u8, String),
    // ListAssign(u8, u8),
}

#[derive(Clone, Copy)]
struct CompileExprRets {
    reg: usize,
}
impl Compiler {
    fn new_label(&mut self) -> u32 {
        let i = self.label_cnt;
        self.label_cnt += 1;
        i
    }
    fn emit_label(&mut self, l: u32) {
        self.labels
            .insert(l, self.cur_function.as_ref().unwrap().code.len() as u32);
    }

    fn new() -> Self {
        Self {
            loop_labels: vec![],
            cur_function: None,
            labels: HashMap::new(),
            module: None,
            string_cache: HashMap::new(),
            symbols: SymbolTable { functions: vec![] },
            label_cnt: 0,
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

    fn alloc_reg(&mut self) -> usize {
        match self.symbols.allocator().allocate(1) {
            Some(x) => x.start,
            None => panic!("not enough registers"),
        }
    }
    fn alloc_reg_n(&mut self, n: usize) -> Range<usize> {
        match self.symbols.allocator().allocate(n) {
            Some(x) => x,
            None => panic!("not enough registers"),
        }
    }
    fn alloc_reg_if_need(&mut self, arg: CompileExprArgs) -> usize {
        if let Some(r) = arg.result_reg {
            r
        } else {
            self.alloc_reg()
        }
    }
    fn emit(&mut self, inst: Instruction) {
        self.cur_function.as_mut().unwrap().code.push(inst);
    }
    fn push_float(&mut self, x: f64, reg: usize) {
        let bytes = x.to_le_bytes();
        self.emit(Instruction::LoadFloat {
            dst: reg as u8,
            bytes: [bytes[0], bytes[1]],
        });
        self.emit(Instruction::Byte([bytes[2], bytes[3], bytes[4]]));
        self.emit(Instruction::Byte([bytes[5], bytes[6], bytes[7]]));
    }
    fn push_int(&mut self, x: i64, reg: usize) {
        let bytes = x.to_le_bytes();
        self.emit(Instruction::LoadInt {
            dst: reg as u8,
            bytes: [bytes[0], bytes[1]],
        });
        self.emit(Instruction::Byte([bytes[2], bytes[3], bytes[4]]));
        self.emit(Instruction::Byte([bytes[5], bytes[6], bytes[7]]));
    }
    // fn resolve_identifier(&mut self, name:&String)->Result<CompileExprRets, CompileError> {

    // }
    fn push_string(&mut self, s: String, reg: usize) {
        let i = self.add_string(s.clone());
        self.emit(Instruction::LoadString {
            dst: reg as u8,
            str_id: i as u16,
        });
    }
    fn add_string(&mut self, s: String) -> usize {
        if let Some(i) = self.string_cache.get(&s) {
            *i
        } else {
            let i = self.module.as_mut().unwrap().string_pool.len();
            self.string_cache.insert(s.clone(), i);
            self.module.as_mut().unwrap().string_pool.push(s);
            i
        }
    }
    fn load_global(&mut self, name: &String, reg: usize) -> Result<(), CompileError> {
        let i = self.add_string(name.clone());
        self.emit(Instruction::LoadGlobal {
            dst: reg as u8,
            name_id: i as u16,
        });
        Ok(())
    }
    fn store_global(&mut self, name: &String, reg: usize) -> Result<(), CompileError> {
        let i = self.add_string(name.clone());
        self.emit(Instruction::StoreGlobal {
            src: reg as u8,
            name_id: i as u16,
        });
        Ok(())
    }
    fn resolve_upvalue(&mut self, name: &String) {
        if self.symbols.get(name).is_some() {
            return;
        }

        let mut idx = None;
        let mut parent_location = 0;
        for i in (0..self.symbols.functions.len()).rev() {
            let table = &mut self.symbols.functions[i];
            let b = table.get(name).is_some();
            let id = table.upvalues.len() as u32;
            if let Some(info) = table.get_mut(name) {
                assert!(b);
                idx = Some(i);
                info.is_upvalue = Some(UpValueInfo::Parent {
                    id,
                    location: info.location,
                });
                parent_location = info.location;
            } else {
                assert!(!b);
            }
            if b {
                // println!("{} is upvalue",name);
                table.upvalues.push(UpValueInfo::Parent {
                    id,
                    location: parent_location,
                });
                parent_location = id;
                break;
            }
        }
        if idx.is_none() {
            return;
        }

        let idx = idx.unwrap();
        for i in idx + 1..self.symbols.functions.len() {
            let f = &mut self.symbols.functions[i];
            let loc = f.upvalues.len();
            f.upvalues.push(UpValueInfo::Child {
                id: loc as u32,
                parent_location,
            });
            f.add(
                name.clone(),
                VarInfo {
                    func_scope: i,
                    location: loc as u32,
                    uid: gen_uid(),
                    is_onstack: false,
                    is_upvalue: Some(UpValueInfo::Child {
                        id: loc as u32,
                        parent_location,
                    }),
                    is_const: false,
                },
            );
            parent_location = loc as u32;
        }
    }
    fn load_identifier(
        &mut self,
        name: &String,
        args: CompileExprArgs,
    ) -> Result<CompileExprRets, CompileError> {
        self.resolve_upvalue(name);

        if let Some(v) = self.symbols.get(name) {
            if v.is_upvalue.is_none() || v.is_onstack {
                let r = if args.result_reg.is_some() {
                    let r = args.result_reg.unwrap();
                    self.emit(Instruction::Move {
                        dst: r as u8,
                        src: v.location as u8,
                    });
                    r
                } else {
                    v.location as usize
                };
                Ok(CompileExprRets { reg: r })
            } else {
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::LoadUpvalue {
                    dst: r as u8,
                    upvalue: v.location as u8,
                });
                Ok(CompileExprRets { reg: r })
            }
        } else {
            let r = self.alloc_reg_if_need(args);
            self.load_global(name, r)?;
            Ok(CompileExprRets { reg: r })
        }
    }
    fn compile_expr(
        &mut self,
        expr: &Rc<Expr>,
        args: CompileExprArgs,
    ) -> Result<CompileExprRets, CompileError> {
        match &**expr {
            Expr::VarArgs { token } => todo!(),
            Expr::Const { token } => {
                let r = self.alloc_reg_if_need(args);
                match token.to_string().as_str() {
                    "nil" => {
                        self.emit(Instruction::LoadConst(LoadConst::Nil, r as u8));
                    }
                    "true" => {
                        self.emit(Instruction::LoadConst(LoadConst::True, r as u8));
                    }
                    "false" => {
                        self.emit(Instruction::LoadConst(LoadConst::False, r as u8));
                    }
                    _ => unreachable!(),
                }
                Ok(CompileExprRets { reg: r })
            }
            Expr::Literal { token } => match token {
                Token::Number { value, .. } => {
                    let r = self.alloc_reg_if_need(args);
                    // match value {
                    //     Number::Float(x) => {
                    //         self.push_float(*x, r);
                    //     }
                    //     Number::Int(x) => {
                    //         self.push_int(*x, r);
                    //     }
                    // }
                    self.push_float(*value, r);
                    Ok(CompileExprRets { reg: r })
                }
                Token::String { value, .. } => {
                    let r = self.alloc_reg_if_need(args);
                    self.push_string(value.clone(), r);
                    Ok(CompileExprRets { reg: r })
                }
                _ => unreachable!(),
            },
            Expr::Identifier { token } => {
                self.load_identifier(token.as_identifier().unwrap(), args)
            }
            Expr::BinaryExpr { op, lhs, rhs } => {
                let op = op.to_string();
                let op = match op.as_str() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "**" => BinaryOp::Pow,
                    "%" => BinaryOp::Mod,
                    "^" => BinaryOp::Xor,
                    "&" => BinaryOp::BitwiseAnd,
                    "|" => BinaryOp::BitwiseOr,
                    "//" => BinaryOp::IDiv,
                    "<" => BinaryOp::LessThan,
                    "<=" => BinaryOp::LessThanEqual,
                    ">=" => BinaryOp::GreaterThanEqual,
                    ">" => BinaryOp::GreaterThan,
                    "==" => BinaryOp::Equal,
                    "~=" => BinaryOp::NotEqual,
                    op @ ("and" | "or") => {
                        let r = self.alloc_reg_if_need(args);
                        let _ret_lhs = self.compile_expr(
                            lhs,
                            CompileExprArgs {
                                result_reg: Some(r),
                            },
                        )?;
                        let l = self.new_label();
                        match op {
                            "and" => {
                                self.emit(Instruction::JumpConditional {
                                    cond: true,
                                    arg: r as u8,
                                });
                            }
                            "or" => {
                                self.emit(Instruction::JumpConditional {
                                    cond: false,
                                    arg: r as u8,
                                });
                            }
                            _ => unreachable!(),
                        }
                        self.emit(Instruction::Label(get_3xu8(l)));
                        let _ret_rhs = self.compile_expr(
                            rhs,
                            CompileExprArgs {
                                result_reg: Some(r),
                            },
                        )?;
                        self.emit_label(l);

                        return Ok(CompileExprRets { reg: r });
                    }
                    _ => unreachable!(), // ".." => BinaryOp::Concat,
                };
                let ret_lhs = self.compile_expr(lhs, CompileExprArgs { result_reg: None })?;
                let ret_rhs = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::BinaryInst {
                    op,
                    lhs: ret_lhs.reg as u8,
                    rhs: ret_rhs.reg as u8,
                    ret: r as u8,
                });
                Ok(CompileExprRets { reg: r })
            }
            Expr::UnaryExpr { op, arg } => todo!(),
            Expr::IndexExpr { loc: _, lhs, rhs } => {
                let ret_lhs = self.compile_expr(lhs, CompileExprArgs { result_reg: None })?;

                let ret_rhs = self.compile_expr(&rhs, CompileExprArgs { result_reg: None })?;
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::LoadTable {
                    table: ret_lhs.reg as u8,
                    key: ret_rhs.reg as u8,
                    dst: r as u8,
                });
                Ok(CompileExprRets { reg: r })
            }
            Expr::DotExpr { loc: _, lhs, rhs } => {
                let ret_lhs = self.compile_expr(lhs, CompileExprArgs { result_reg: None })?;
                let i = self.add_string(rhs.to_string());
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::LoadTableStringKey {
                    table: ret_lhs.reg as u8,
                    dst: r as u8,
                });
                self.emit(Instruction::Byte(get_3xu8(i as u32)));
                Ok(CompileExprRets { reg: r })
            }
            Expr::MethodCallExpr {
                callee,
                method,
                args: call_args,
            } => {
                let regs = self.alloc_reg_n(call_args.len() + 2);
                let _: Vec<_> = call_args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| -> Result<CompileExprRets, CompileError> {
                        self.compile_expr(
                            arg,
                            CompileExprArgs {
                                result_reg: Some(regs.start + i + 2),
                            },
                        )
                    })
                    .collect();

                let ret_lhs = self.compile_expr(
                    callee,
                    CompileExprArgs {
                        result_reg: Some(regs.start + 1),
                    },
                )?;
                let i = self.add_string(method.to_string());
                self.emit(Instruction::LoadTableStringKey {
                    table: ret_lhs.reg as u8,
                    dst: regs.start as u8,
                });
                self.emit(Instruction::Byte(get_3xu8(i as u32)));
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::CallMethod {
                    arg_start: regs.start as u8,
                    arg_end: regs.end as u8,
                    ret: r as u8,
                });
                Ok(CompileExprRets { reg: r })
            }
            Expr::CallExpr {
                callee,
                args: call_args,
            } => {
                let regs = self.alloc_reg_n(call_args.len() + 1);
                let _: Vec<_> = call_args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| -> Result<CompileExprRets, CompileError> {
                        self.compile_expr(
                            arg,
                            CompileExprArgs {
                                result_reg: Some(regs.start + i + 1),
                            },
                        )
                    })
                    .collect();
                let _ = self.compile_expr(
                    callee,
                    CompileExprArgs {
                        result_reg: Some(regs.start),
                    },
                )?;
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::Call {
                    arg_start: regs.start as u8,
                    arg_end: regs.end as u8,
                    ret: r as u8,
                });
                Ok(CompileExprRets { reg: r })
            }
            Expr::FunctionExpr {
                loc,
                args: f_args,
                body,
                ..
            } => {
                let r = self.alloc_reg_if_need(args);
                let params: Vec<_> = f_args.iter().map(|x| x.to_string()).collect();
                let id = self.compile_function(loc, &params, body)?;

                self.emit(Instruction::LoadClosure {
                    dst: r as u8,
                    func_id: id as u16,
                });
                Ok(CompileExprRets { reg: r })
            }
            Expr::Table { loc: _, fields } => {
                let r = self.alloc_reg_if_need(args);
                self.emit(Instruction::NewTable {
                    dst: r as u8,
                    is_table: true,
                });
                for field in fields {
                    match field {
                        crate::parse::TableField::ExprPair(k, v) => {
                            let ret_k = self.compile_expr(k, Default::default())?;
                            let ret_v = self.compile_expr(v, Default::default())?;
                            self.emit(Instruction::StoreTable {
                                table: r as u8,
                                key: ret_k.reg as u8,
                                value: ret_v.reg as u8,
                            });
                            self.symbols.allocator().free_one(ret_k.reg);
                            self.symbols.allocator().free_one(ret_v.reg);
                        }
                        crate::parse::TableField::NamePair(k, v) => {
                            let ret_v = self.compile_expr(v, Default::default())?;
                            self.emit(Instruction::StoreTableStringKey {
                                table: r as u8,
                                value: ret_v.reg as u8,
                            });
                            let key = self.add_string(k.to_string());
                            self.emit(Instruction::Byte(get_3xu8(key as u32)));
                            self.symbols.allocator().free_one(ret_v.reg);
                        }
                        crate::parse::TableField::ArrayEntry(_) => todo!(),
                    }
                }
                Ok(CompileExprRets { reg: r })
            }
            Expr::ParenExpr { loc: _, expr } => self.compile_expr(expr, args),
        }
    }
    fn resolve_lvalue(&mut self, e: &Rc<Expr>) -> Result<LValue, CompileError> {
        match &**e {
            Expr::IndexExpr { lhs, rhs, .. } => {
                let ret_lhs = self.compile_expr(lhs, CompileExprArgs { result_reg: None })?;
                let ret_rhs = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
                Ok(LValue::TableAssign(ret_lhs.reg as u8, ret_rhs.reg as u8))
            }
            Expr::DotExpr { lhs, rhs, .. } => {
                let ret_lhs = self.compile_expr(lhs, CompileExprArgs { result_reg: None })?;
                Ok(LValue::TableKeyAssign(ret_lhs.reg as u8, rhs.to_string()))
            }
            Expr::Identifier { token } => {
                self.resolve_upvalue(&token.to_string());
                if let Some(v) = self.symbols.get(&token.to_string()) {
                    if v.is_upvalue.is_none() {
                        return Ok(LValue::Local(v.location.try_into().unwrap()));
                    } else {
                        return Ok(LValue::UpValue(v.location));
                    }
                } else {
                    return Ok(LValue::Global(token.to_string()));
                }
            }
            _ => unreachable!(),
        }
    }
    fn emit_store(&mut self, lvalue: LValue, rhs: &Rc<Expr>) -> Result<(), CompileError> {
        match lvalue {
            LValue::Local(r) => {
                let _ = self.compile_expr(
                    rhs,
                    CompileExprArgs {
                        result_reg: Some(r as usize),
                    },
                )?;
                Ok(())
            }
            _=>{
                let e = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
                self.emit_store2(lvalue, e.reg as u8)?;
                Ok(())
            }
            // LValue::UpValue(_) => {
            //     let e = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
            //     self.emit_store2(lvalue, e.reg as u8)?;
            //     Ok(())
            // }
            // LValue::Global(_) => {
            //     let e = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
            //     self.emit_store2(lvalue, e.reg as u8)?;
            //     return Ok(());
            // }
            // LValue::TableAssign(_, _) => todo!(),
            // LValue::TableKeyAssign(_, _) => todo!(),
            // LValue::ListAssign(_, _) => todo!(),
        }
    }
    fn emit_store2(&mut self, lvalue: LValue, rhs: u8) -> Result<(), CompileError> {
        match lvalue {
            LValue::Local(r) => {
                self.emit(Instruction::Move { src: rhs, dst: r });
                Ok(())
            }
            LValue::UpValue(r) => {
                self.emit(Instruction::StoreUpvalue {
                    src: rhs,
                    dst: r as u8,
                });
                Ok(())
            }
            LValue::Global(v) => {
                self.store_global(&v, rhs.try_into().unwrap())?;
                return Ok(());
            }
            LValue::TableAssign(table, key) => {
                self.emit(Instruction::StoreTable {
                    table,
                    key,
                    value: rhs,
                });
                Ok(())
            }
            LValue::TableKeyAssign(table, key) => {
                let i = self.add_string(key);
                self.emit(Instruction::StoreTableStringKey { table, value: rhs });
                self.emit(Instruction::Byte(get_3xu8(i as u32)));
                Ok(())
            } // LValue::ListAssign(_, _) => todo!(),
        }
    }
    // fn emit_store(&mut self, lhs: &Rc<Expr>, rhs: &Rc<Expr>) -> Result<(), CompileError> {
    //     match &**lhs {
    //         Expr::Identifier { token } => {
    //             self.resolve_upvalue(&token.str());
    //             if let Some(v) = self.symbols.get(&token.str()) {
    //                 if v.is_global {
    //                     let e = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
    //                     self.store_global(&token.str(), e.reg)?;
    //                     return Ok(());
    //                 }
    //                 if v.is_upvalue.is_none() {
    //                     let _ = self.compile_expr(
    //                         rhs,
    //                         CompileExprArgs {
    //                             result_reg: Some(v.location as usize),
    //                         },
    //                     )?;
    //                 } else {
    //                     let e = self.compile_expr(rhs, CompileExprArgs { result_reg: None })?;
    //                     self.emit(Instruction::StoreUpvalue {
    //                         src: e.reg as u8,
    //                         dst: v.location as u8,
    //                     });
    //                 }
    //             } else {
    //                 return Err(CompileError {
    //                     kind: ErrorKind::InternalError,
    //                     msg: format!("identifier `{}` is not defined", token.str()),
    //                     loc: lhs.loc().clone(),
    //                 });
    //             }
    //             Ok(())
    //         }
    //         _ => unreachable!(),
    //     }
    // }
    // fn emit_store2(&mut self, lhs: &Rc<Expr>, rhs: u8) -> Result<(), CompileError> {
    //     match &**lhs {
    //         Expr::Identifier { token } => {
    //             self.resolve_upvalue(&token.str());
    //             if let Some(v) = self.symbols.get(&token.str()) {
    //                 if v.is_global {
    //                     self.store_global(&token.str(), rhs as usize)?;
    //                     return Ok(());
    //                 }
    //                 if v.is_upvalue.is_none() {
    //                     self.emit(Instruction::Move {
    //                         src: rhs,
    //                         dst: v.location as u8,
    //                     })
    //                 } else {
    //                     self.emit(Instruction::StoreUpvalue {
    //                         src: rhs as u8,
    //                         dst: v.location as u8,
    //                     });
    //                 }
    //             } else {
    //                 return Err(CompileError {
    //                     kind: ErrorKind::InternalError,
    //                     msg: format!("identifier `{}` is not defined", token.str()),
    //                     loc: lhs.loc().clone(),
    //                 });
    //             }
    //             Ok(())
    //         }
    //         _ => unreachable!(),
    //     }
    // }
    fn close_upvalues(&mut self) {
        let insts: Vec<_> = self
            .symbols
            .functions
            .last()
            .unwrap()
            .scopes
            .last()
            .unwrap()
            .vars
            .iter()
            .map(|(_, v)| match v.is_upvalue {
                Some(info) => match info {
                    UpValueInfo::Parent { id, .. } => {
                        Some(Instruction::CloseUpvalue { dst: id as u8 })
                    }
                    UpValueInfo::Child { .. } => None,
                },
                _ => None,
            })
            .collect();
        for i in insts {
            if let Some(i) = i {
                self.emit(i);
            }
        }
    }
    fn compile_assign(&mut self, lhs: &[Rc<Expr>], rhs: &[Rc<Expr>]) -> Result<(), CompileError> {
        if lhs.len() == rhs.len() && lhs.len() == 1 {
            let lvalue = self.resolve_lvalue(&lhs[0])?;
            self.emit_store(lvalue, &rhs[0])
        } else if lhs.len() == rhs.len() {
            let mut regs: Vec<_> = vec![];
            // rhs
            //     .iter()
            //     .map(|e| self.compile_expr(e, CompileExprArgs { result_reg: None }))
            //     .collect();
            for e in rhs {
                regs.push(self.compile_expr(e, CompileExprArgs { result_reg: None })?);
            }
            let mut lvalues = vec![];
            let mut conflicts = HashSet::new();
            for lv in lhs {
                let lvalue = self.resolve_lvalue(lv)?;
                match &lvalue {
                    LValue::Local(r) => {
                        conflicts.insert(*r);
                    }
                    _ => {}
                }
                lvalues.push(lvalue);
            }
            for (_, e) in regs.iter_mut().enumerate() {
                if conflicts.contains(&(e.reg as u8)) {
                    let reg = self.alloc_reg() as u8;
                    self.emit(Instruction::Move {
                        src: e.reg as u8,
                        dst: reg,
                    });
                    e.reg = reg as usize;
                }
            }
            for (i, e) in regs.iter().enumerate() {
                let lvalue = lvalues[i].clone();
                self.emit_store2(lvalue, e.reg as u8)?;
            }
            Ok(())
        } else {
            todo!()
        }
    }
    fn compile_stmt(&mut self, stmt: &Rc<Stmt>) -> Result<(), CompileError> {
        match &**stmt {
            Stmt::Return { loc: _, expr } => {
                if expr.len() == 1 {
                    let r = self.compile_expr(&expr[0], CompileExprArgs { result_reg: None })?;
                    self.emit(Instruction::Return { arg: r.reg as u8 });
                } else if expr.is_empty() {
                    let r = self.alloc_reg();
                    self.emit(Instruction::LoadConst(LoadConst::Nil, r as u8));
                    self.emit(Instruction::Return { arg: r as u8 });
                } else {
                    todo!()
                }
                Ok(())
            }
            Stmt::Break { loc } => {
                if self.loop_labels.is_empty() {
                    return Err(CompileError {
                        loc: loc.clone(),
                        kind: ErrorKind::SemanticError,
                        msg: "break statement must be inside a loop".into(),
                    });
                } else {
                    self.close_upvalues();
                    self.emit(Instruction::Jump);
                    self.emit(Instruction::Label(get_3xu8(
                        self.loop_labels.last().unwrap().1,
                    )));
                    Ok(())
                }
            }
            Stmt::LocalVar { vars, .. } => match &**vars {
                Stmt::Assign { lhs, .. } => {
                    for var in lhs {
                        match &**var {
                            Expr::Identifier { token } => {
                                let r = self.symbols.add_var(token.to_string());
                                self.symbols.allocator().regs[r as usize] = Reg::Local;
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.compile_stmt(vars)
                }
                _ => unreachable!(),
            },
            Stmt::If {
                loc: _,
                cond,
                then,
                else_ifs,
                else_,
            } => {
                let cond_r = self.compile_expr(cond, Default::default())?;
                self.emit(Instruction::JumpConditional {
                    cond: false,
                    arg: cond_r.reg as u8,
                });
                self.symbols.allocator().free_all();
                // let mut jmp_end = vec![];
                let else_label = self.new_label();
                let end = self.new_label();
                self.emit(Instruction::Label(get_3xu8(else_label)));
                self.compile_stmt(then)?;
                self.emit(Instruction::Jump);
                self.emit(Instruction::Label(get_3xu8(end)));
                self.emit_label(else_label);
                for (cond, then) in else_ifs {
                    let cond_r = self.compile_expr(cond, Default::default())?;
                    self.emit(Instruction::JumpConditional {
                        cond: false,
                        arg: cond_r.reg as u8,
                    });
                    self.symbols.allocator().free_all();
                    let else_label = self.new_label();
                    self.emit(Instruction::Label(get_3xu8(else_label)));
                    self.compile_stmt(then)?;
                    self.emit(Instruction::Jump);
                    self.emit(Instruction::Label(get_3xu8(end)));
                    // jmp_end.push(end);
                    self.emit_label(else_label);
                }

                if let Some(else_) = else_ {
                    self.compile_stmt(else_)?;
                }
                self.emit_label(end);
                Ok(())
            }
            Stmt::While { loc: _, cond, body } => {
                let begin = self.new_label();
                self.emit_label(begin);
                let cond_r = self.compile_expr(cond, Default::default())?;
                self.emit(Instruction::JumpConditional {
                    cond: false,
                    arg: cond_r.reg as u8,
                });
                let end = self.new_label();
                self.loop_labels.push((begin, end));
                self.emit(Instruction::Label(get_3xu8(end)));
                self.compile_stmt(body)?;
                self.emit(Instruction::Jump);
                self.emit(Instruction::Label(get_3xu8(begin)));
                self.emit_label(end);
                self.loop_labels.pop();
                Ok(())
            }
            // Stmt::Loop { body } => {
            //     let begin = self.new_label();
            //     self.emit_label(begin);
            //     let end = self.new_label();
            //     self.loop_labels.push((begin, end));
            //     self.compile_stmt(body)?;
            //     self.emit(Instruction::Jump);
            //     self.emit(Instruction::Label(get_3xu8(begin)));
            //     self.emit_label(end);
            //     self.loop_labels.pop();
            //     Ok(())
            // }
            // Stmt::For { vars, range, body } => todo!(),
            Stmt::Assign { loc: _, lhs, rhs } => self.compile_assign(&lhs, &rhs),
            Stmt::Expr { loc: _, expr } => {
                self.compile_expr(
                    expr,
                    CompileExprArgs {
                        // optional_unwrap_label: None,
                        result_reg: None,
                    },
                )?;
                self.symbols.allocator().free_all();
                Ok(())
            }
            Stmt::Block { loc: _, stmts } => {
                self.symbols.functions.last_mut().unwrap().push();
                for s in stmts {
                    self.compile_stmt(s)?;
                    self.symbols.allocator().free_all();
                }
                self.close_upvalues();
                self.symbols.functions.last_mut().unwrap().pop();
                Ok(())
            }
            Stmt::LocalFunction { name, args, body } => {
                let v = self.symbols.add_var(name.to_string());
                let params: Vec<_> = args.iter().map(|x| x.to_string()).collect();
                let id = self.compile_function(stmt.loc(), &params, body)?;
                self.emit(Instruction::LoadClosure {
                    func_id: id as u16,
                    dst: v,
                });
                Ok(())
            }
            Stmt::Function {
                name: func_name,
                args,
                body,
                ..
            } => {
                let mut params: Vec<_> = args.iter().map(|x| x.to_string()).collect();
                if let FunctionName::Method { method, .. } = func_name {
                    if method.is_some() {
                        params.insert(0, String::from("self"));
                    }
                }
                let id = self.compile_function(stmt.loc(), &params, body)?;

                if let FunctionName::Function { name } = func_name {
                    assert!(!self.symbols.functions.is_empty());

                    self.symbols.add_var(name.to_string());
                    let reg = self.alloc_reg();
                    self.emit(Instruction::LoadClosure {
                        dst: reg as u8,
                        func_id: id as u16,
                    });
                    self.store_global(name.as_identifier().unwrap(), reg)?;
                } else if let FunctionName::Method {
                    access_chain,
                    method,
                } = func_name
                {
                    let mut tables = access_chain
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>();
                    let name = if let Some(m) = method {
                        m.to_string()
                    } else {
                        tables.pop().unwrap()
                    };

                    let dst = self.alloc_reg();
                    let l = tables.len();
                    for i in 0..l {
                        if i == 0 {
                            let _ = self.load_identifier(
                                &tables[i],
                                CompileExprArgs {
                                    result_reg: Some(dst),
                                },
                            )?;
                        } else {
                            let sid = self.add_string(tables[i].to_string());
                            self.emit(Instruction::LoadTableStringKey {
                                table: dst as u8,
                                dst: dst as u8,
                            });
                            self.emit(Instruction::Byte(get_3xu8(sid as u32)));
                        }
                    }
                    let f = self.alloc_reg();
                    self.emit(Instruction::LoadClosure {
                        dst: f as u8,
                        func_id: id as u16,
                    });
                    let sid = self.add_string(name);
                    self.emit(Instruction::StoreTableStringKey {
                        table: dst as u8,
                        value: f as u8,
                    });
                    self.emit(Instruction::Byte(get_3xu8(sid as u32)));
                } else {
                    unreachable!()
                }
                self.symbols.allocator().free_all();
                Ok(())
            }
            _ => todo!(),
        }
    }
    fn enter_function(&mut self) {
        self.symbols.functions.push(PerFunctionSymbolTable::new());
    }
    fn leave_function(&mut self) {
        self.symbols.functions.pop().unwrap();
        let f = self.cur_function.as_mut().unwrap();
        for c in f.code.iter_mut() {
            match *c {
                Instruction::Label(l) => {
                    *c = Instruction::JumpAddress(get_3xu8(
                        *self.labels.get(&u32_from_3xu8(l)).unwrap(),
                    ));
                }
                _ => {}
            }
        }
        self.labels.clear();
        self.loop_labels.clear();
    }
    fn compile_function(
        &mut self,
        loc: &SourceLocation,
        params: &[String],
        body: &Rc<Stmt>,
    ) -> Result<u32, CompileError> {
        let loop_labels = std::mem::replace(&mut self.loop_labels, vec![]);
        let labels = std::mem::replace(&mut self.labels, HashMap::new());
        let cur_function = std::mem::replace(
            &mut self.cur_function,
            Some(CompiledFunction {
                n_args: 0,
                code: vec![],
                upvalues: vec![],
                is_method: false,
            }),
        );
        self.enter_function();
        self.symbols.functions.last_mut().unwrap().push();
        let mut has_self = false;
        for (i, p) in params.iter().enumerate() {
            if p == "self" {
                has_self = true;
                if i != 0 {
                    return Err(CompileError {
                        kind: ErrorKind::SemanticError,
                        msg: "self must be the first parameter".into(),
                        loc: loc.clone(),
                    });
                }
            }
            let r = self.symbols.add_var(p.clone());
            assert!(r as usize == i);
            self.symbols.allocator().regs[i] = Reg::Local;
        }
        self.cur_function.as_mut().unwrap().is_method = has_self;
        self.symbols.functions.last_mut().unwrap().push();
        self.compile_stmt(body)?;
        self.symbols.functions.last_mut().unwrap().pop();
        self.symbols.functions.last_mut().unwrap().pop();

        let upvalues = self.symbols.functions.last().unwrap().upvalues.clone();
        self.leave_function();
        let id = {
            let m = self.module.as_mut().unwrap();
            let id = m.functions.len() as u32;
            let mut f = self.cur_function.take().unwrap();
            f.upvalues = upvalues;
            m.functions.push(f);
            id
        };
        self.cur_function = cur_function;
        self.labels = labels;
        self.loop_labels = loop_labels;
        Ok(id)
    }
    fn compile_program(&mut self, program: &Vec<Rc<Stmt>>) -> Result<(), CompileError> {
        {
            self.cur_function = Some(CompiledFunction {
                n_args: 0,
                // entry:0,
                code: vec![],
                upvalues: vec![],
                is_method: false,
            });
        }
        self.enter_function();
        for s in program {
            self.compile_stmt(s)?;
        }
        let upvalues = self.symbols.functions.last().unwrap().upvalues.clone();
        self.leave_function();
        {
            let m = self.module.as_mut().unwrap();
            let mut f = self.cur_function.take().unwrap();
            f.upvalues = upvalues;
            m.functions.push(f);
        }
        Ok(())
    }
    fn run(&mut self, program: &Vec<Rc<Stmt>>) -> Result<ByteCodeModule, CompileError> {
        let module = ByteCodeModule {
            string_pool: vec![],
            string_pool_cache: vec![],
            functions: vec![],
        };
        self.module = Some(module);
        self.compile_program(program)?;
        Ok(std::mem::replace(&mut self.module, None).unwrap())
    }
}

pub fn compile(program: &Vec<Rc<Stmt>>) -> Result<ByteCodeModule, CompileError> {
    let mut compiler = Compiler::new();
    compiler.run(program)
}
