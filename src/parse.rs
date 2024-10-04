/*
chunk ::= block

    block ::= {stat} [retstat]

    stat ::=  ‘;’ |
         varlist ‘=’ explist |
         functioncall |
         label |
         break |
         goto Name |
         do block end |
         while exp do block end |
         repeat block until exp |
         if exp then block {elseif exp then block} [else block] end |
         for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
         for namelist in explist do block end |
         function funcname funcbody |
         local function Name funcbody |
         local attnamelist [‘=’ explist]

    attnamelist ::=  Name attrib {‘,’ Name attrib}

    attrib ::= [‘<’ Name ‘>’]

    retstat ::= return [explist] [‘;’]

    label ::= ‘::’ Name ‘::’

    funcname ::= Name {‘.’ Name} [‘:’ Name]

    varlist ::= var {‘,’ var}

    var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name

    namelist ::= Name {‘,’ Name}

    explist ::= exp {‘,’ exp}

    exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
         prefixexp | tableconstructor | exp binop exp | unop exp

    prefixexp ::= var | functioncall | ‘(’ exp ‘)’

    functioncall ::=  prefixexp args | prefixexp ‘:’ Name args

    args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString

    functiondef ::= function funcbody

    funcbody ::= ‘(’ [parlist] ‘)’ block end

    parlist ::= namelist [‘,’ ‘...’] | ‘...’

    tableconstructor ::= ‘{’ [fieldlist] ‘}’

    fieldlist ::= field {fieldsep field} [fieldsep]

    field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp

    fieldsep ::= ‘,’ | ‘;’

    binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
         ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
         ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |
         and | or

    unop ::= ‘-’ | not | ‘#’ | ‘~’

*/

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: Option<Rc<String>>,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Empty, // ‘;’
    Assignment {
        varlist: Vec<Var>,
        explist: Vec<Expression>,
    }, // varlist ‘=’ explist
    FunctionCall(FunctionCall), // functioncall
    Label(String), // label ::= ‘::’ Name ‘::’
    Break,
    Goto(String), // goto Name
    Do(Block),    // do block end
    While {
        condition: Expression,
        block: Block,
    }, // while exp do block end
    Repeat {
        block: Block,
        condition: Expression,
    }, // repeat block until exp
    If {
        clauses: Vec<(Expression, Block)>,
        else_block: Option<Block>,
    }, // if exp then block {elseif exp then block} [else block] end
    ForNumeric {
        name: String,
        start: Expression,
        end: Expression,
        step: Option<Expression>,
        block: Block,
    }, // for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
    ForGeneric {
        namelist: Vec<String>,
        explist: Vec<Expression>,
        block: Block,
    }, // for namelist in explist do block end
    FunctionDef {
        name: FunctionName,
        body: FunctionBody,
    }, // function funcname funcbody
    LocalFunction {
        name: String,
        body: FunctionBody,
    }, // local function Name funcbody
    LocalAssignment {
        namelist: Vec<String>,
        explist: Vec<Expression>,
    }, // local attnamelist [‘=’ explist]
    Return(Option<Vec<Expression>>), // return [explist] [‘;’]
}

#[derive(Debug)]
pub enum Var {
    Name(String),
    Indexed {
        prefix: Box<Expression>,
        index: Expression,
    }, // prefixexp ‘[’ exp ‘]’
    Member {
        prefix: Box<Expression>,
        member: String,
    }, // prefixexp ‘.’ Name
}

#[derive(Debug)]
pub enum Expression {
    Nil,
    False,
    True,
    Numeral(f64),
    LiteralString(String),
    VarArg,                     // ‘...’
    FunctionDef(FunctionBody),  // functiondef ::= function funcbody
    PrefixExp(Box<Expression>), // prefixexp ::= var | functioncall | ‘(’ exp ‘)’
    TableConstructor(TableConstructor),
    BinaryOp {
        left: Box<Expression>,
        op: BinOp,
        right: Box<Expression>,
    }, // exp binop exp
    UnaryOp {
        op: UnOp,
        expr: Box<Expression>,
    }, // unop exp
}

#[derive(Debug)]
pub struct FunctionBody {
    pub params: Vec<String>,
    pub is_vararg: bool,
    pub block: Block,
}

#[derive(Debug)]
pub struct FunctionName {
    pub names: Vec<String>,
    pub method: Option<String>, // [‘:’ Name]
}

#[derive(Debug)]
pub enum FunctionCall {
    Normal {
        prefix: Box<Expression>,
        args: Args,
    }, // prefixexp args
    Method {
        prefix: Box<Expression>,
        method: String,
        args: Args,
    }, // prefixexp ‘:’ Name args
}

#[derive(Debug)]
pub enum Args {
    Explist(Vec<Expression>), // ‘(’ [explist] ‘)’
    Table(TableConstructor),  // tableconstructor
    String(String),           // LiteralString
}

#[derive(Debug)]
pub struct TableConstructor {
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub enum Field {
    Array(Expression),                              // exp
    Rec { key: String, value: Expression },         // Name ‘=’ exp
    Indexed { key: Expression, value: Expression }, // ‘[’ exp ‘]’ ‘=’ exp
}

#[derive(Debug)]
pub enum BinOp {
    Add,        // '+'
    Sub,        // '-'
    Mul,        // '*'
    Div,        // '/'
    FloorDiv,   // '//'
    Pow,        // '^'
    Mod,        // '%'
    BitAnd,     // '&'
    BitXor,     // '~'
    BitOr,      // '|'
    ShiftRight, // '>>'
    ShiftLeft,  // '<<'
    Concat,     // '..'
    Lt,         // '<'
    Le,         // '<='
    Gt,         // '>'
    Ge,         // '>='
    Eq,         // '=='
    Ne,         // '~='
    And,        // and
    Or,         // or
}

#[derive(Debug)]
pub enum UnOp {
    Neg,    // '-'
    Not,    // not
    Len,    // '#'
    BitNot, // '~'
}
