use std::{collections::HashSet, iter::FromIterator, rc::Rc};

#[derive(Clone, Debug)]
pub struct SourceLocation {
    pub file: Rc<String>,
    pub line: usize,
    pub col: usize,
}
// #[derive(Clone, Copy, Debug)]
// pub enum Symbol {
//     Add,
//     Sub,
//     Mul,
//     Div,
//     Not,
//     And,
//     Or,
//     Assign,
//     Comma,
//     Colon,
//     SemiColon,
//     LessThan,
//     LessThanEqual,
//     GreaterThan,
//     GreaterThanEqual,
//     NotEqual,
//     Equal,
// }
// #[derive(Clone, Copy, Debug)]
// pub enum Keyword {
//     If,
//     While,
//     Repeat,
//     Until,
//     End,
//     Else,
//     Function,
//     Return,
//     Not,
//     And,
//     Or,
// }
#[derive(Clone, Debug)]
pub enum Token {
    Number { value: f64, loc: SourceLocation },
    Symbol { value: String, loc: SourceLocation },
    String { value: String, loc: SourceLocation },
    Identifier { value: String, loc: SourceLocation },
    Keyword { value: String, loc: SourceLocation },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal {
        token: Token,
    },
    BinaryExpr {
        op: Token,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    UnaryExpr { op: Token, arg: Rc<Expr> },
}
#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Rc<Expr>,
    pub then: Rc<Stmt>,
    pub else_ifs: Vec<(Rc<Expr>, Rc<Stmt>)>,
    pub else_: Option<Rc<Stmt>>,
}
#[derive(Clone, Debug)]
pub enum Stmt {
    If(IfStmt),
}
pub struct Program {}

#[derive(Clone, Debug)]
pub struct TokenizeError {
    pub msg: String,
    pub loc: SourceLocation,
}
struct Tokenizer {
    loc: SourceLocation,
    source: Vec<char>,
    pos: usize,
    symbols: Vec<HashSet<String>>,
    keywords: HashSet<String>,
}
impl Tokenizer {
    fn next(&mut self) -> Option<char> {
        let i = self.pos;
        self.pos += 1;
        if let Some(ch) = self.source.get(i) {
            if *ch == '\n' {
                self.loc.line += 1;
                self.loc.col = 1;
            } else {
                self.loc.col += 1;
            }
            Some(*ch)
        } else {
            None
        }
    }
    fn advance(&mut self, n: usize) {
        assert!(n > 0);
        for _ in 0..n {
            self.next();
        }
    }
    fn peek(&mut self) -> Option<char> {
        self.peek_n(0)
    }
    fn peek_n(&mut self, n: usize) -> Option<char> {
        match self.source.get(self.pos + n) {
            Some(ch) => Some(*ch),
            None => None,
        }
    }
    fn peek_n_str(&mut self, n: usize) -> Option<String> {
        let mut s = String::new();
        for i in 0..n {
            s.push(self.peek_n(i)?);
        }
        Some(s)
    }
    fn try_parse_symbol(&mut self) -> Option<Result<Token, TokenizeError>> {
        let max_len = self.symbols.len();
        for i in (0..max_len).rev() {
            if let Some(symbol) = self.peek_n_str(i + 1) {
                if self.symbols[i].contains(&symbol) {
                    self.advance(i + 1);
                    return Some(Ok(Token::Symbol {
                        value: symbol,
                        loc: self.loc.clone(),
                    }));
                }
            }
        }
        None
    }

    fn try_parse_identifier_keyword(&mut self) -> Option<Result<Token, TokenizeError>> {
        let ch = self.peek()?;
        let loc = self.loc.clone();
        let mut ident = String::new();
        if ch.is_alphabetic() || ch == '_' {
            ident.push(ch);
            self.next();
            loop {
                if let Some(ch) = self.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(ch);
                        self.next();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            if self.keywords.contains(&ident) {
                return Some(Ok(Token::Keyword { value: ident, loc }));
            }
            Some(Ok(Token::Identifier { value: ident, loc }))
        } else {
            None
        }
    }
    fn try_parse_number(&mut self) -> Option<Result<Token, TokenizeError>> {
        let ch = self.peek()?;
        let loc = self.loc.clone();
        if ch.is_digit(10) {
            let mut int: u64 = 0;
            let mut pow = 0;
            let mut frac = 0;
            let mut is_float = false;
            loop {
                let ch = self.peek();
                if ch.is_none() {
                    break;
                }
                let ch = ch.unwrap();
                if ch.is_digit(10) {
                    int = int * 10 + ch as u64 - '0' as u64;
                    self.next();
                } else if ch == '.' {
                    self.next();
                    is_float = true;
                    break;
                } else {
                    break;
                }
            }
            if is_float {
                loop {
                    let ch = self.peek();
                    if ch.is_none() {
                        break;
                    }
                    let ch = ch.unwrap();
                    if ch.is_digit(10) {
                        frac = frac * 10 + ch as u64 - '0' as u64;
                        pow += 1;
                        self.next();
                    } else {
                        break;
                    }
                }
            }
            Some(Ok(Token::Number {
                value: (int as f64) + (frac as f64) * (0.1f64).powi(pow),
                loc,
            }))
        } else {
            None
        }
    }
    fn try_parse_string(&mut self) -> Option<Result<Token, TokenizeError>> {
        let mut s = String::new();
        let loc = self.loc.clone();
        let open = self.peek()?;
        if open == '"' || open == '\'' {
            self.next();
            loop {
                let ch = self.next();
                if ch.is_none() {
                    self.error("unexpected EOF when parsing string literal");
                }
                let ch = ch.unwrap();
                match ch {
                    '\\' => {
                        let ch = self.next();
                        if ch.is_none() {
                            self.error("unexpected EOF when parsing escape sequence");
                        }
                        let ch = ch.unwrap();
                        match ch {
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            '\'' => s.push('\''),
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            _ => {
                                return Some(Err(self.error(&format!(
                                    "unrecognized escape sequence \"\\{}\"",
                                    ch
                                ))));
                            }
                        }
                    }
                    _ => {
                        if ch == open {
                            break;
                        } else {
                            s.push(ch);
                        }
                    }
                };
            }
            Some(Ok(Token::String { value: s, loc }))
        } else {
            None
        }
    }
    fn next_token(&mut self) -> Result<Token, TokenizeError> {
        let token = self
            .try_parse_identifier_keyword()
            .or_else(|| self.try_parse_number())
            .or_else(|| self.try_parse_string())
            .or_else(|| self.try_parse_symbol());
        if let Some(token) = token {
            token
        } else {
            let ch = self.next().unwrap();
            Err(self.error(&format!("unable to parse: '{}'", ch)))
        }
    }
    fn skip_line(&mut self) {
        while let Some(ch) = self.next() {
            if ch == '\n' {
                break;
            }
        }
    }
    fn skip_comment(&mut self) {
        if let Some(s) = self.peek_n_str(2) {
            if s == "--" {
                self.advance(2);
                self.skip_line();
            }
        }
    }
    fn skip_space(&mut self) {
        loop {
            let prev_pos = self.pos;
            self.skip_comment();
            self.skip_space_inner();
            if prev_pos == self.pos {
                break;
            }
        }
    }
    fn skip_space_inner(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.next();
            } else if ch == '\n' {
                self.next();
            } else {
                break;
            }
        }
    }

    fn error(&mut self, msg: &str) -> TokenizeError {
        TokenizeError {
            loc: self.loc.clone(),
            msg: String::from(msg),
        }
    }
    fn run(&mut self) -> Result<Vec<Token>, TokenizeError> {
        let mut tokens = vec![];
        while self.peek().is_some() {
            self.skip_space();
            tokens.push(self.next_token()?);
        }
        Ok(tokens)
    }
}
pub fn tokenize(filename: &str, source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut symbols = vec![];
    {
        symbols.push(HashSet::from_iter(
            vec![
                "+", "-", "*", "/", "[", "]", "(", ")", "{", "}", "%", "&", "^", "~", "!", ";",
                ",", ":", ".", "<", ">", "#", "=",
            ]
            .into_iter()
            .map(|s| String::from(s)),
        ));
        symbols.push(HashSet::from_iter(
            vec!["..", "//", "~=", "==", "!=", "<=", ">="]
                .into_iter()
                .map(|s| String::from(s)),
        ));
        symbols.push(HashSet::from_iter(
            vec!["..."].into_iter().map(|s| String::from(s)),
        ));
    }
    let keywords = HashSet::from_iter(
        vec![
            "break", "goto", "do", "end", "while", "if", "elseif", "else", "repeat", "until",
            "for", "local", "function", "do", "in", "return", "nil", "false", "true", "and", "not",
            "or",
        ]
        .into_iter()
        .map(|s| String::from(s)),
    );
    let mut tokenizer = Tokenizer {
        loc: SourceLocation {
            file: Rc::new(String::from(filename)),
            line: 1,
            col: 1,
        },
        symbols,
        source: source.chars().collect(),
        pos: 0,
        keywords,
    };
    tokenizer.run()
}

pub struct ParseError {
    pub msg: String,
    pub loc: SourceLocation,
}
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}
macro_rules! gen_parse_binary_expr {
    ($self:ident, $parse_next:expr, $($op:literal),+) => {
        {
            let lhs = $parse_next?;
            let mut expr = match lhs {
                Ok(x) => x,
                Err(x) => {
                    return Some(Err(x));
                }
            };
            let ops:Vec<_> = vec![$($op),+].into_iter().map(|s|String::from(s)).collect();
            loop {
                let next = $self.peek();
                if let Some(op) = next {
                    let new = op.clone();
                    std::mem::drop(op);
                    let op = new;
                    match &op {
                        Token::Symbol { value, loc } => {
                            if ops.contains(&value) {
                                $self.advance(1);
                                let rhs = $parse_next;//$self.parse_atom();
                                if rhs.is_none() {
                                    return Some(Err($self.error(
                                        &format!("expect atom when parsing binary expr {:?}", ops),
                                        loc.clone(),
                                    )));
                                }
                                let rhs = rhs.unwrap();
                                let rhs = match rhs {
                                    Err(e) => return Some(Err(e)),
                                    Ok(e) => e,
                                };
                                expr = Rc::new(Expr::BinaryExpr {
                                    op: op.clone(),
                                    lhs: expr,
                                    rhs,
                                });
                            } else {
                                break;
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                } else {
                    break;
                }
            };
            Some(Ok(expr))
        }
    };
}
impl Parser {
    fn advance(&mut self, n: usize) {
        assert!(n > 0);
        self.pos += n;
    }
    fn peek(&mut self) -> Option<&Token> {
        self.peek_n(0)
    }
    fn peek_n(&mut self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
    fn parse_atom(&mut self) -> Option<Result<Rc<Expr>, ParseError>> {
        let token = self.peek()?.clone();
        match token {
            Token::Number { .. } => {
                self.advance(1);
                Some(Ok(Rc::new(Expr::Literal {
                    token: token.clone(),
                })))
            }
            _ => None,
        }
    }
    fn parse_mul_expr(&mut self) -> Option<Result<Rc<Expr>, ParseError>> {
        gen_parse_binary_expr!(self, self.parse_atom(), "*", "/")
    }
    fn parse_add_expr(&mut self) -> Option<Result<Rc<Expr>, ParseError>> {
        gen_parse_binary_expr!(self, self.parse_mul_expr(), "+", "-")
    }
    fn parse_binary_expr(&mut self) -> Option<Result<Rc<Expr>, ParseError>> {
        self.parse_add_expr()
    }
    fn error(&mut self, msg: &str, loc: SourceLocation) -> ParseError {
        ParseError {
            loc,
            msg: String::from(msg),
        }
    }
    fn run(&mut self) -> Result<Rc<Expr>, ParseError> {
        self.parse_binary_expr().unwrap()
    }
}

pub fn parse_impl(tokens: Vec<Token>) -> Result<Rc<Expr>, ParseError> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.run()
}
