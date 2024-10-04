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

use hexf_parse::parse_hexf64;
use lazy_static::lazy_static;
use std::collections::HashSet;

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

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
}

#[derive(Debug)]
pub enum Statement {
    Empty, // ‘;’
    Assignment {
        varlist: Vec<Spanned<Var>>,
        explist: Vec<Spanned<Expression>>,
    }, // varlist ‘=’ explist
    FunctionCall(Spanned<FunctionCall>), // functioncall
    Label(Spanned<String>), // label ::= ‘::’ Name ‘::’
    Break,
    Goto(Spanned<String>), // goto Name
    Do(Spanned<Block>),    // do block end
    While {
        condition: Spanned<Expression>,
        block: Spanned<Block>,
    }, // while exp do block end
    Repeat {
        block: Spanned<Block>,
        condition: Spanned<Expression>,
    }, // repeat block until exp
    If {
        clauses: Vec<(Spanned<Expression>, Spanned<Block>)>,
        else_block: Option<Spanned<Block>>,
    }, // if exp then block {elseif exp then block} [else block] end
    ForNumeric {
        name: Spanned<String>,
        start: Spanned<Expression>,
        end: Spanned<Expression>,
        step: Option<Spanned<Expression>>,
        block: Spanned<Block>,
    }, // for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
    ForGeneric {
        namelist: Vec<Spanned<String>>,
        explist: Vec<Spanned<Expression>>,
        block: Spanned<Block>,
    }, // for namelist in explist do block end
    FunctionDef {
        name: Spanned<FunctionName>,
        body: Spanned<FunctionBody>,
    }, // function funcname funcbody
    LocalFunction {
        name: Spanned<String>,
        body: Spanned<FunctionBody>,
    }, // local function Name funcbody
    LocalAssignment {
        namelist: Vec<Spanned<String>>,
        explist: Vec<Spanned<Expression>>,
    }, // local attnamelist [‘=’ explist]
    Return(Option<Vec<Spanned<Expression>>>), // return [explist] [‘;’]
}

#[derive(Debug)]
pub enum Var {
    Name(Spanned<String>),
    Indexed {
        prefix: Box<Spanned<Expression>>,
        index: Spanned<Expression>,
    }, // prefixexp ‘[’ exp ‘]’
    Member {
        prefix: Box<Spanned<Expression>>,
        member: Spanned<String>,
    }, // prefixexp ‘.’ Name
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Numeral {
    Float(f64),
    Integer(i64),
}

#[derive(Debug)]
pub enum Expression {
    Nil,
    False,
    True,
    Numeral(Spanned<Numeral>),
    LiteralString(Spanned<String>),
    VarArg,                              // ‘...’
    FunctionDef(Spanned<FunctionBody>),  // functiondef ::= function funcbody
    PrefixExp(Box<Spanned<Expression>>), // prefixexp ::= var | functioncall | ‘(’ exp ‘)’
    TableConstructor(Spanned<TableConstructor>),
    BinaryOp {
        left: Box<Spanned<Expression>>,
        op: BinOp,
        right: Box<Spanned<Expression>>,
    }, // exp binop exp
    UnaryOp {
        op: UnOp,
        expr: Box<Spanned<Expression>>,
    }, // unop exp
}

#[derive(Debug)]
pub struct FunctionBody {
    pub params: Vec<Spanned<String>>,
    pub is_vararg: bool,
    pub block: Spanned<Block>,
}

#[derive(Debug)]
pub struct FunctionName {
    pub names: Vec<Spanned<String>>,
    pub method: Option<Spanned<String>>, // [‘:’ Name]
}

#[derive(Debug)]
pub enum FunctionCall {
    Normal {
        prefix: Box<Spanned<Expression>>,
        args: Spanned<Args>,
    }, // prefixexp args
    Method {
        prefix: Box<Spanned<Expression>>,
        method: Spanned<String>,
        args: Spanned<Args>,
    }, // prefixexp ‘:’ Name args
}

#[derive(Debug)]
pub enum Args {
    Explist(Vec<Spanned<Expression>>), // ‘(’ [explist] ‘)’
    Table(Spanned<TableConstructor>),  // tableconstructor
    String(String),                    // LiteralString
}

#[derive(Debug)]
pub struct TableConstructor {
    pub fields: Vec<Spanned<Field>>,
}

#[derive(Debug)]
pub enum Field {
    Array(Spanned<Expression>), // exp
    Rec {
        key: Spanned<String>,
        value: Spanned<Expression>,
    }, // Name ‘=’ exp
    Indexed {
        key: Spanned<Expression>,
        value: Spanned<Expression>,
    }, // ‘[’ exp ‘]’ ‘=’ exp
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

struct Parser {
    chars: Vec<char>,
    file: Option<Rc<String>>,
    pos: usize,
    line: usize,
    column: usize,
}
#[derive(Debug)]
pub struct ParsingError {
    pub message: String,
    pub span: Span,
}

lazy_static! {
    static ref KEYWORDS: HashSet<&'static str> = {
        let mut m = HashSet::new();
        m.insert("and");
        m.insert("break");
        m.insert("do");
        m.insert("else");
        m.insert("elseif");
        m.insert("end");
        m.insert("false");
        m.insert("for");
        m.insert("function");
        m.insert("goto");
        m.insert("if");
        m.insert("in");
        m.insert("local");
        m.insert("nil");
        m.insert("not");
        m.insert("or");
        m.insert("repeat");
        m.insert("return");
        m.insert("then");
        m.insert("true");
        m.insert("until");
        m.insert("while");
        m
    };
}
impl Parser {
    fn new(input: &str, file: Option<Rc<String>>) -> Self {
        Self {
            chars: input.chars().collect(),
            file,
            pos: 0,
            line: 1,
            column: 1,
        }
    }
    fn get_char(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).cloned()
    }
    fn advance(&mut self, count: usize) {
        for _ in 0..count {
            // check if newline and update line and columne accordingly
            if let Some(c) = self.get_char(0) {
                if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                self.pos += 1;
            }
        }
    }
    fn expect_char(&self, offset: usize, c: char) -> Result<(), ParsingError> {
        if self.get_char(offset) == Some(c) {
            Ok(())
        } else {
            Err(ParsingError {
                message: format!(
                    "Expected '{}', found '{}'",
                    c,
                    self.get_char(offset).unwrap()
                ),
                span: Span {
                    start: SourceLocation {
                        file: self.file.clone(),
                        line: self.line,
                        column: self.column,
                    },
                    end: SourceLocation {
                        file: self.file.clone(),
                        line: self.line,
                        column: self.column + offset,
                    },
                },
            })
        }
    }
    fn peek_str(&self, s: &str) -> bool {
        for (i, expected_char) in s.chars().enumerate() {
            if self.get_char(i) != Some(expected_char) {
                return false;
            }
        }
        true
    }
    fn expect_str(&self, offset: usize, s: &str) -> Result<(), ParsingError> {
        for (i, expected_char) in s.chars().enumerate() {
            if self.get_char(offset + i) != Some(expected_char) {
                return Err(ParsingError {
                    message: format!(
                        "Expected '{}', found '{}'",
                        s,
                        self.chars[self.pos..self.pos + s.len()]
                            .iter()
                            .collect::<String>()
                    ),
                    span: Span {
                        start: SourceLocation {
                            file: self.file.clone(),
                            line: self.line,
                            column: self.column,
                        },
                        end: SourceLocation {
                            file: self.file.clone(),
                            line: self.line,
                            column: self.column + offset + i,
                        },
                    },
                });
            }
        }
        Ok(())
    }
    /*

         Examples of valid integer constants are

         3   345   0xff   0xBEBADA

    Examples of valid float constants are

         3.0     3.1416     314.16e-2     0.31416E1     34e1
         0x0.1E  0xA23p-4   0X1.921FB54442D18P+1
     */
    fn parse_numeral(&mut self) -> Result<Spanned<Numeral>, ParsingError> {
        let start_line = self.line;
        let start_column = self.column;
        macro_rules! make_span {
            ($self:expr, $start_line:expr, $start_column:expr) => {
                Span {
                    start: SourceLocation {
                        file: $self.file.clone(),
                        line: $start_line,
                        column: $start_column,
                    },
                    end: SourceLocation {
                        file: $self.file.clone(),
                        line: $self.line,
                        column: $self.column,
                    },
                }
            };
        }

        // first handle hexadecimals
        if self.get_char(0) == Some('0')
            && (self.get_char(1) == Some('x') || self.get_char(1) == Some('X'))
        {
            let mut is_hex_float = false;

            self.advance(2); // skip '0x' or '0X'
            let mut hex_str = String::from("0x");
            // keep collecting until a non-hex or 'P' is found
            while let Some(c) = self.get_char(0) {
                if c.is_ascii_hexdigit() || c == '.' {
                    if c == '.' {
                        if is_hex_float {
                            // invalid, return error
                            return Err(ParsingError {
                                message: format!("Invalid hexadecimal number '{}'", hex_str),
                                span: make_span!(self, start_line, start_column),
                            });
                        }
                        is_hex_float = true;
                    }
                    hex_str.push(c);
                    self.advance(1);
                } else if c == 'p' || c == 'P' {
                    is_hex_float = true;
                    // if must be followed by a +/- and then a number
                    hex_str.push(c);
                    self.advance(1);
                    if let Some(c) = self.get_char(0) {
                        if c == '+' || c == '-' {
                            hex_str.push(c);
                            self.advance(1);
                            while let Some(c) = self.get_char(0) {
                                if c.is_ascii_digit() {
                                    hex_str.push(c);
                                    self.advance(1);
                                } else {
                                    break;
                                }
                            }
                        } else {
                            // invalid, return error
                            return Err(ParsingError {
                                message: format!("Invalid hexadecimal number '{}'", hex_str),
                                span: make_span!(self, start_line, start_column),
                            });
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            // try to parse the hex string
            if is_hex_float {
                // parse using parse_hexf64
                match parse_hexf64(&hex_str, false) {
                    Ok(value) => {
                        return Ok(Spanned {
                            node: Numeral::Float(value),
                            span: make_span!(self, start_line, start_column),
                        })
                    }
                    Err(_) => {
                        return Err(ParsingError {
                            message: format!("Invalid hexadecimal float '{}'", hex_str),
                            span: make_span!(self, start_line, start_column),
                        });
                    }
                }
            } else {
                // parse as integer
                match i64::from_str_radix(&hex_str[2..], 16) {
                    Ok(value) => {
                        return Ok(Spanned {
                            node: Numeral::Integer(value),
                            span: make_span!(self, start_line, start_column),
                        })
                    }
                    Err(_) => {
                        return Err(ParsingError {
                            message: format!("Invalid hexadecimal integer '{}'", hex_str),
                            span: make_span!(self, start_line, start_column),
                        });
                    }
                }
            }
        }
        // handle decimal numbers
        else {
            let mut is_float = false;
            let mut num_str = String::new();

            // first collect the integer part
            while let Some(c) = self.get_char(0) {
                if c.is_ascii_digit() {
                    num_str.push(c);
                    self.advance(1);
                } else {
                    break;
                }
            }
            // check if there is a decimal part
            if let Some(c) = self.get_char(0) {
                if c == '.' {
                    is_float = true;
                    num_str.push(c);
                    self.advance(1);
                    while let Some(c) = self.get_char(0) {
                        if c.is_ascii_digit() {
                            num_str.push(c);
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
            }

            // check exponential part
            if let Some(c) = self.get_char(0) {
                if c == 'e' || c == 'E' {
                    is_float = true;
                    num_str.push(c);
                    self.advance(1);
                    if let Some(c) = self.get_char(0) {
                        if c == '+' || c == '-' {
                            num_str.push(c);
                            self.advance(1);
                        }
                        while let Some(c) = self.get_char(0) {
                            if c.is_ascii_digit() {
                                num_str.push(c);
                                self.advance(1);
                            } else {
                                break;
                            }
                        }
                    }
                }
            }

            if is_float {
                match num_str.parse::<f64>() {
                    Ok(value) => Ok(Spanned {
                        node: Numeral::Float(value),
                        span: make_span!(self, start_line, start_column),
                    }),
                    Err(_) => Err(ParsingError {
                        message: format!("Invalid float '{}'", num_str),
                        span: make_span!(self, start_line, start_column),
                    }),
                }
            } else {
                match num_str.parse::<i64>() {
                    Ok(value) => Ok(Spanned {
                        node: Numeral::Integer(value),
                        span: make_span!(self, start_line, start_column),
                    }),
                    Err(_) => Err(ParsingError {
                        message: format!("Invalid integer '{}'", num_str),
                        span: make_span!(self, start_line, start_column),
                    }),
                }
            }
        }
    }
    fn parse_literal_string(&mut self) -> Result<Spanned<String>, ParsingError> {
        let start_line = self.line;
        let start_column = self.column;
        macro_rules! make_span {
            ($self:expr, $start_line:expr, $start_column:expr) => {
                Span {
                    start: SourceLocation {
                        file: $self.file.clone(),
                        line: $start_line,
                        column: $start_column,
                    },
                    end: SourceLocation {
                        file: $self.file.clone(),
                        line: $self.line,
                        column: $self.column,
                    },
                }
            };
        }
        let c = self.get_char(0).unwrap();
        if c == '\'' || c == '\"' {
            let quote = c;
            self.advance(1);
            let mut string_content = String::new();
            while let Some(c) = self.get_char(0) {
                if c == quote {
                    self.advance(1);
                    return Ok(Spanned {
                        node: string_content,
                        span: make_span!(self, start_line, start_column),
                    });
                } else if c == '\\' {
                    self.advance(1);
                    if let Some(escaped_char) = self.get_char(0) {
                        match escaped_char {
                            'a' => {
                                string_content.push('\u{0007}');
                                self.advance(1);
                            }
                            'b' => {
                                string_content.push('\u{0008}');
                                self.advance(1);
                            }
                            'f' => {
                                string_content.push('\u{000C}');
                                self.advance(1);
                            }
                            'n' => {
                                string_content.push('\n');
                                self.advance(1);
                            }
                            'r' => {
                                string_content.push('\r');
                                self.advance(1);
                            }
                            't' => {
                                string_content.push('\t');
                                self.advance(1);
                            }
                            'v' => {
                                string_content.push('\u{000B}');
                                self.advance(1);
                            }
                            '\\' => {
                                string_content.push('\\');
                                self.advance(1);
                            }
                            '\'' => {
                                string_content.push('\'');
                                self.advance(1);
                            }
                            '"' => {
                                string_content.push('"');
                                self.advance(1);
                            }
                            'u' => {
                                self.advance(1); // skip 'u'
                                let mut unicode_escape = String::new();
                                self.advance(1); // skip '{'
                                while let Some(c) = self.get_char(0) {
                                    if c == '}' {
                                        self.advance(1); // skip '}'
                                        break;
                                    } else if c.is_ascii_hexdigit() {
                                        unicode_escape.push(c);
                                        self.advance(1);
                                    } else {
                                        return Err(ParsingError {
                                            message: format!(
                                                "Invalid Unicode escape sequence '\\u{{{}}}'",
                                                unicode_escape
                                            ),
                                            span: make_span!(self, start_line, start_column),
                                        });
                                    }
                                }
                                if let Ok(code_point) = u32::from_str_radix(&unicode_escape, 16) {
                                    if let Some(unicode_char) = std::char::from_u32(code_point) {
                                        string_content.push(unicode_char);
                                    } else {
                                        return Err(ParsingError {
                                            message: format!(
                                                "Invalid Unicode code point '\\u{{{}}}'",
                                                unicode_escape
                                            ),
                                            span: make_span!(self, start_line, start_column),
                                        });
                                    }
                                } else {
                                    return Err(ParsingError {
                                        message: format!(
                                            "Invalid Unicode escape sequence '\\u{{{}}}'",
                                            unicode_escape
                                        ),
                                        span: make_span!(self, start_line, start_column),
                                    });
                                }
                            }

                            _ => {
                                return Err(ParsingError {
                                    message: format!(
                                        "Invalid escape sequence '\\{}'",
                                        escaped_char
                                    ),
                                    span: make_span!(self, start_line, start_column),
                                })
                            }
                        }
                    } else {
                        return Err(ParsingError {
                            message: "Unexpected end of input in escape sequence".to_string(),
                            span: make_span!(self, start_line, start_column),
                        });
                    }
                } else {
                    string_content.push(c);
                    self.advance(1);
                }
            }
            Err(ParsingError {
                message: "Unexpected end of input in string literal".to_string(),
                span: make_span!(self, start_line, start_column),
            })
        } else if c == '[' {
            let mut level = 0;
            self.advance(1);
            while let Some(c) = self.get_char(0) {
                if c == '=' {
                    level += 1;
                    self.advance(1);
                } else if c == '[' {
                    self.advance(1);
                    break;
                } else {
                    return Err(ParsingError {
                        message: "Invalid long string delimiter".to_string(),
                        span: make_span!(self, start_line, start_column),
                    });
                }
            }

            let mut string_content = String::new();
            let mut end_sequence = String::new();
            end_sequence.push(']');
            for _ in 0..level {
                end_sequence.push('=');
            }
            end_sequence.push(']');

            if self.get_char(0) == Some('\n') {
                self.advance(1);
            }

            while let Some(c) = self.get_char(0) {
                if self.peek_str(&end_sequence) {
                    self.advance(end_sequence.len());
                    return Ok(Spanned {
                        node: string_content,
                        span: make_span!(self, start_line, start_column),
                    });
                } else {
                    string_content.push(c);
                    self.advance(1);
                }
            }

            Err(ParsingError {
                message: "Unexpected end of input in long string literal".to_string(),
                span: make_span!(self, start_line, start_column),
            })
        } else {
            Err(ParsingError {
                message: "Unexpected token".to_string(),
                span: make_span!(self, start_line, start_column),
            })
        }
    }
    /// first parse nil, false, true, numeral, literalstring, '...', functiondef
    fn parse_atom_exp(&mut self) -> Result<Spanned<Expression>, ParsingError> {
        let start_line = self.line;
        let start_column = self.column;
        macro_rules! make_span {
            ($self:expr, $start_line:expr, $start_column:expr) => {
                Span {
                    start: SourceLocation {
                        file: $self.file.clone(),
                        line: $start_line,
                        column: $start_column,
                    },
                    end: SourceLocation {
                        file: $self.file.clone(),
                        line: $self.line,
                        column: $self.column,
                    },
                }
            };
        }

        if self.peek_str("nil") {
            self.advance(3);
            return Ok(Spanned {
                node: Expression::Nil,
                span: make_span!(self, start_line, start_column),
            });
        }

        if self.peek_str("false") {
            self.advance(5);
            return Ok(Spanned {
                node: Expression::False,
                span: make_span!(self, start_line, start_column),
            });
        }

        if self.peek_str("true") {
            self.advance(4);
            return Ok(Spanned {
                node: Expression::True,
                span: make_span!(self, start_line, start_column),
            });
        }

        if self.peek_str("...") {
            self.advance(3);
            return Ok(Spanned {
                node: Expression::VarArg,
                span: make_span!(self, start_line, start_column),
            });
        }

        if self.peek_str("function") {
            todo!();
        }

        if let Ok(numeral) = self.parse_numeral() {
            return Ok(Spanned {
                node: Expression::Numeral(numeral),
                span: make_span!(self, start_line, start_column),
            });
        }

        // Handle LiteralString
        match self.get_char(0) {
            Some('"') | Some('\'') | Some('[') => {
                return self.parse_literal_string().map(|s| Spanned {
                    node: Expression::LiteralString(s),
                    span: make_span!(self, start_line, start_column),
                });
            }
            _ => {}
        };

        Err(ParsingError {
            message: "Unexpected token".to_string(),
            span: make_span!(self, start_line, start_column),
        })
    }
    fn parse_exp(&mut self) -> Result<Spanned<Expression>, ParsingError> {
        todo!()
    }
}

// write some test
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_numeral() {
        // 3   345   0xff   0xBEBADA
        //      3.0     3.1416     314.16e-2     0.31416E1     34e1
        //      0x0.1E  0xA23p-4   0X1.921FB54442D18P+1
        // let mut parser = Parser::new("0x1234", None);
        // let result = parser.parse_numeral().ok();
        // assert_eq!(result, Some(Numeral::Integer(0x1234)));
        macro_rules! test_parse_integer {
            ($input:expr, $expected:expr) => {
                let mut parser = Parser::new($input, None);
                let result = parser.parse_numeral().unwrap().node;
                assert_eq!(result, Numeral::Integer($expected));
            };
        }

        test_parse_integer!("3", 3);
        test_parse_integer!("345", 345);
        test_parse_integer!("0xff", 0xff);
        test_parse_integer!("0xBEBADA", 0xBEBADA);
        macro_rules! test_parse_float {
            ($input:expr, $expected:expr) => {
                let mut parser = Parser::new($input, None);
                let result = parser.parse_numeral().unwrap().node;
                assert_eq!(result, Numeral::Float($expected));
            };
        }
        use hexf::hexf64;

        test_parse_float!("3.0", 3.0);
        test_parse_float!("3.1416", 3.1416);
        test_parse_float!("314.16e-2", 3.1416);
        test_parse_float!("0.31416E1", 3.1416);
        test_parse_float!("34e1", 340.0);
        // test_parse_float!("0x0.1E", hexf64!("0x0.1E"));
        test_parse_float!("0xA23p-4", hexf64!("0xA23p-4"));
        test_parse_float!("0X1.921FB54442D18P+1", hexf64!("0X1.921FB54442D18P+1"));
    }
    #[test]
    fn test_parse_literal_string() {
        macro_rules! test_parse_string {
            ($input:expr, $expected:expr) => {
                let mut parser = Parser::new($input, None);
                let result = parser.parse_literal_string().unwrap().node;
                assert_eq!(result, $expected.to_string());
            };
        }

        test_parse_string!(r#""hello""#, "hello");
        test_parse_string!(r#"'world'"#, "world");
        test_parse_string!(r#""escaped\nnewline""#, "escaped\nnewline");
        test_parse_string!(r#""escaped\\backslash""#, "escaped\\backslash");
        test_parse_string!(r#""escaped\"quote""#, "escaped\"quote");
        test_parse_string!(r#""unicode\u{1F600}""#, "unicode😀");
        test_parse_string!(
            r#""long string [[ with nested ]] brackets""#,
            "long string [[ with nested ]] brackets"
        );
        test_parse_string!(
            r#"[[long string with newlines
and more newlines]]"#,
            "long string with newlines\nand more newlines"
        );
    }
}
