use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
    vec,
};

use crate::{
    bytecode::*,
    parse::{Expr, SourceLocation, Stmt, Token},
};

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
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
    location: u32,
    on_stack: bool, // is false, then on upvalue
}
struct SymbolTable {
    vars: RefCell<HashMap<String, VarInfo>>,
    parent: Option<Rc<SymbolTable>>,
}
struct Compiler {
    symbols: Rc<SymbolTable>,
    module: ByteCodeModule,
    str_map: HashMap<String, usize>,
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
            Expr::Identifier { token } => todo!(),
            Expr::BinaryExpr { op, lhs, rhs } => {
                let opcode = match op {
                    Token::Symbol { value, .. } => match value.as_str() {
                        "+" => OpCode::Add,
                        "-" => OpCode::Sub,
                        "*" => OpCode::Mul,
                        "/" => OpCode::Div,
                        "%" => OpCode::Mod,
                        "^" => OpCode::Pow,
                        "&" => OpCode::And,
                        "|" => OpCode::Or,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;
                self.emit(ByteCode::Op(opcode));
                Ok(())
            }
            Expr::UnaryExpr { op, arg } => todo!(),
            Expr::IndexExpr { loc, lhs, rhs } => todo!(),
            Expr::DotExpr { loc, lhs, rhs } => todo!(),
            Expr::CallExpr { callee, args } => todo!(),
            Expr::MethodCallExpr {
                callee,
                method,
                args,
            } => todo!(),
            Expr::FunctionExpr { loc, args, body } => todo!(),
            Expr::Table { loc, fields } => todo!(),
        }
    }
    fn compile_stmt(&mut self, block: &Stmt) -> Result<(), CompileError> {
        match &*block {
            Stmt::Return { loc, expr } => todo!(),
            Stmt::LocalVar { loc, vars } => todo!(),
            Stmt::LocalFunction { name, args, body } => todo!(),
            Stmt::If {
                loc,
                cond,
                then,
                else_ifs,
                else_,
            } => todo!(),
            Stmt::While { loc, cond, body } => todo!(),
            Stmt::Repeat { loc, cond, body } => todo!(),
            Stmt::For {
                name,
                init,
                end,
                step,
                body,
            } => todo!(),
            Stmt::ForIn { name, range, body } => todo!(),
            Stmt::Assign { loc, lhs, rhs } => todo!(),
            Stmt::Expr { loc, expr } => self.compile_expr(expr),
            Stmt::Block { loc, stmts } => {
                for stmt in stmts {
                    self.compile_stmt(&**stmt)?;
                }
                Ok(())
            }
            Stmt::Function { name, args, body } => todo!(),
        }
    }
    fn run(&mut self, block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
        self.compile_stmt(&*block)?;
        let module = std::mem::replace(
            &mut self.module,
            ByteCodeModule {
                code: vec![],
                string_pool: vec![],
            },
        );
        Ok(module)
    }
}

pub fn compile(block: Rc<Stmt>) -> Result<ByteCodeModule, CompileError> {
    let mut compiler = Compiler {
        symbols: Rc::new(SymbolTable {
            vars: RefCell::new(HashMap::new()),
            parent: None,
        }),
        module: ByteCodeModule {
            code: vec![],
            string_pool: vec![],
        },
        str_map: HashMap::new(),
    };
    compiler.run(block)
}
