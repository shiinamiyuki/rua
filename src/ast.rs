//! AST node definitions for the Lua 5.5 parser.

use crate::token::Location;

/// A source-located AST node.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub location: Location,
}

impl<T> Spanned<T> {
    pub fn new(node: T, location: Location) -> Self {
        Spanned { node, location }
    }
}

// ── Block ──────────────────────────────────────────────────────────

/// A block is a sequence of statements, optionally ending with a return.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stat>,
    pub ret: Option<RetStat>,
}

/// `return [explist] [';']`
#[derive(Debug, Clone, PartialEq)]
pub struct RetStat {
    pub values: Vec<Expr>,
    pub location: Location,
}

// ── Statements ─────────────────────────────────────────────────────

pub type Stat = Spanned<StatKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatKind {
    /// `;`
    Empty,

    /// `varlist '=' explist`
    Assign {
        targets: Vec<Var>,
        values: Vec<Expr>,
    },

    /// Function call used as a statement.
    FunctionCall(FunctionCall),

    /// `do block end`
    DoBlock(Block),

    /// `while exp do block end`
    While {
        cond: Expr,
        body: Block,
    },

    /// `repeat block until exp`
    Repeat {
        body: Block,
        cond: Expr,
    },

    /// `if exp then block {elseif exp then block} [else block] end`
    If {
        cond: Expr,
        then_block: Block,
        elseif_clauses: Vec<(Expr, Block)>,
        else_block: Option<Block>,
    },

    /// `for Name '=' exp ',' exp [',' exp] do block end`
    NumericFor {
        name: String,
        init: Expr,
        limit: Expr,
        step: Option<Expr>,
        body: Block,
    },

    /// `for namelist in explist do block end`
    GenericFor {
        names: Vec<String>,
        iterators: Vec<Expr>,
        body: Block,
    },

    /// `goto Name`
    Goto(String),

    /// `:: Name ::`
    Label(String),

    /// `break`
    Break,

    /// `function funcname funcbody`
    FuncDef {
        name: FuncName,
        body: FuncBody,
    },

    /// `local function Name funcbody`
    LocalFuncDef {
        name: String,
        body: FuncBody,
    },

    /// `global function Name funcbody`
    GlobalFuncDef {
        name: String,
        body: FuncBody,
    },

    /// `local attnamelist ['=' explist]`
    LocalDecl {
        names: Vec<AttName>,
        values: Vec<Expr>,
    },

    /// `global attnamelist ['=' explist]`
    GlobalDecl {
        names: Vec<AttName>,
        values: Vec<Expr>,
    },

    /// `global [attrib] '*'`
    GlobalStar {
        attrib: Option<String>,
    },
}

// ── Attributes and names ───────────────────────────────────────────

/// A name with an optional attribute.
#[derive(Debug, Clone, PartialEq)]
pub struct AttName {
    pub name: String,
    pub attrib: Option<String>,
}

/// `funcname ::= Name {'.' Name} [':' Name]`
#[derive(Debug, Clone, PartialEq)]
pub struct FuncName {
    pub path: Vec<String>,
    pub method: Option<String>,
}

// ── Function body ──────────────────────────────────────────────────

/// `funcbody ::= '(' [parlist] ')' block end`
#[derive(Debug, Clone, PartialEq)]
pub struct FuncBody {
    pub params: Vec<String>,
    pub has_varargs: bool,
    /// Lua 5.5: optional name for the vararg table (`... name`).
    pub vararg_name: Option<String>,
    pub body: Block,
}

// ── Expressions ────────────────────────────────────────────────────

pub type Expr = Spanned<ExprKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Nil,
    True,
    False,
    Integer(i64),
    Float(f64),
    String(Vec<u8>),

    /// `...`
    VarArg,

    /// A variable reference (name, table index, field).
    Var(Var),

    /// A function call expression.
    FunctionCall(FunctionCall),

    /// `function funcbody`
    FunctionDef(FuncBody),

    /// `{ fieldlist }`
    TableConstructor(Vec<Field>),

    /// `exp binop exp`
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    /// `unop exp`
    UnOp {
        op: UnOp,
        operand: Box<Expr>,
    },
}

// ── Variables ──────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    /// A simple name reference.
    Name(String),

    /// `prefixexp '[' exp ']'`
    Index {
        table: Box<Expr>,
        key: Box<Expr>,
    },

    /// `prefixexp '.' Name`
    Field {
        table: Box<Expr>,
        name: String,
    },
}

// ── Function calls ─────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub callee: Box<Expr>,
    pub method: Option<String>,
    pub args: CallArgs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArgs {
    /// `'(' [explist] ')'`
    Exprs(Vec<Expr>),
    /// A single table constructor argument: `f { ... }`
    Table(Vec<Field>),
    /// A single string literal argument: `f "str"` or `f [[str]]`
    String(Vec<u8>),
}

// ── Table fields ───────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub kind: FieldKind,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldKind {
    /// `[exp] = exp`
    IndexedAssign { key: Expr, value: Expr },
    /// `Name = exp`
    NameAssign { name: String, value: Expr },
    /// `exp` (positional)
    Positional(Expr),
}

// ── Operators ──────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    // String
    Concat,
    // Comparison
    Eq,
    NEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    // Logical
    And,
    Or,
    // Bitwise
    BAnd,
    BOr,
    BXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,   // -
    Not,   // not
    Len,   // #
    BNot,  // ~
}

// ── Operator info for Pratt parsing ────────────────────────────────

impl BinOp {
    /// Returns (left_binding_power, right_binding_power).
    /// Right-associative operators have right_bp < left_bp at the same level.
    pub fn binding_power(self) -> (u8, u8) {
        match self {
            BinOp::Or => (1, 2),
            BinOp::And => (3, 4),
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq |
            BinOp::NEq | BinOp::Eq => (5, 6),
            BinOp::BOr => (7, 8),
            BinOp::BXor => (9, 10),
            BinOp::BAnd => (11, 12),
            BinOp::Shl | BinOp::Shr => (13, 14),
            BinOp::Concat => (16, 15),  // right-assoc
            BinOp::Add | BinOp::Sub => (17, 18),
            BinOp::Mul | BinOp::Div | BinOp::IDiv | BinOp::Mod => (19, 20),
            // unary is 21
            BinOp::Pow => (24, 23),     // right-assoc
        }
    }
}

impl UnOp {
    pub fn binding_power(self) -> u8 {
        22 // between mul/div and pow
    }
}
