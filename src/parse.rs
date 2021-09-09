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
    EOF { loc: SourceLocation },
}
impl Token {
    pub fn is_eof(&self) -> bool {
        match self {
            Token::EOF { .. } => true,
            _ => false,
        }
    }
    #[allow(dead_code)]
    pub fn loc(&self) -> &SourceLocation {
        match self {
            Token::Number { value: _, loc } => loc,
            Token::Symbol { value: _, loc } => loc,
            Token::String { value: _, loc } => loc,
            Token::Identifier { value: _, loc } => loc,
            Token::Keyword { value: _, loc } => loc,
            Token::EOF { loc } => loc,
        }
    }
}
#[derive(Clone, Debug)]
pub enum TableField {
    ExprPair(Rc<Expr>, Rc<Expr>),
    NamePair(Token, Rc<Expr>),
    ArrayEntry(Rc<Expr>),
}
#[derive(Clone, Debug)]
pub enum Expr {
    Const {
        // nil, true, false
        token: Token,
    },
    Literal {
        token: Token,
    },
    Identifier {
        token: Token,
    },
    BinaryExpr {
        op: Token,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    UnaryExpr {
        op: Token,
        arg: Rc<Expr>,
    },
    IndexExpr {
        loc: SourceLocation,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    DotExpr {
        loc: SourceLocation,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    CallExpr {
        callee: Rc<Expr>,
        args: Vec<Rc<Expr>>,
    },
    MethodCallExpr {
        callee: Rc<Expr>,
        method: Token,
        args: Vec<Rc<Expr>>,
    },
    FunctionExpr {
        loc: SourceLocation,
        args: Vec<Token>,
        body: Rc<Stmt>,
    },
    Table {
        loc: SourceLocation,
        fields: Vec<TableField>,
    },
}
impl Expr {
    #[allow(dead_code)]
    pub fn loc(&self) -> &SourceLocation {
        match self {
            Expr::Literal { token } => token.loc(),
            Expr::Identifier { token } => token.loc(),
            Expr::BinaryExpr { op: _, lhs, rhs: _ } => lhs.loc(),
            Expr::UnaryExpr { op, arg: _ } => op.loc(),
            Expr::IndexExpr {
                loc: _,
                lhs,
                rhs: _,
            } => lhs.loc(),
            Expr::DotExpr {
                loc: _,
                lhs,
                rhs: _,
            } => lhs.loc(),
            Expr::CallExpr { callee, args: _ } => callee.loc(),
            Expr::FunctionExpr {
                loc,
                args: _,
                body: _,
            } => loc,
            Expr::MethodCallExpr {
                callee,
                method: _,
                args: _,
            } => callee.loc(),
            Expr::Table { loc, fields: _ } => loc,
            Expr::Const { token } => token.loc(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum FunctionName {
    Method {
        access_chain: Vec<Token>,
        method: Option<Token>,
    },
    Function {
        name: Token,
    },
}
#[derive(Clone, Debug)]
pub enum Stmt {
    Return {
        loc: SourceLocation,
        expr: Option<Rc<Expr>>,
    },
    LocalVar {
        loc: SourceLocation,
        vars: Rc<Stmt>,
    },
    LocalFunction {
        name: Token,
        args: Vec<Token>,
        body: Rc<Stmt>,
    },
    If {
        loc: SourceLocation,
        cond: Rc<Expr>,
        then: Rc<Stmt>,
        else_ifs: Vec<(Rc<Expr>, Rc<Stmt>)>,
        else_: Option<Rc<Stmt>>,
    },
    While {
        loc: SourceLocation,
        cond: Rc<Expr>,
        body: Rc<Stmt>,
    },
    Repeat {
        loc: SourceLocation,
        cond: Rc<Expr>,
        body: Rc<Stmt>,
    },
    For {
        name: Token,
        init: Rc<Expr>,
        end: Rc<Expr>,
        step: Option<Rc<Expr>>,
        body: Rc<Stmt>,
    },
    ForIn {
        name: Token,
        range: Rc<Expr>,
        body: Rc<Stmt>,
    },
    Assign {
        loc: SourceLocation,
        lhs: Vec<Rc<Expr>>,
        rhs: Vec<Rc<Expr>>,
    },
    Expr {
        loc: SourceLocation,
        expr: Rc<Expr>,
    },
    Block {
        loc: SourceLocation,
        stmts: Vec<Rc<Stmt>>,
    },
    Function {
        name: FunctionName,
        args: Vec<Token>,
        body: Rc<Stmt>,
    },
}

impl Stmt {
    #[allow(dead_code)]
    pub fn loc(&self) -> &SourceLocation {
        match self {
            Stmt::Return { loc, expr: _ } => loc,
            Stmt::If {
                loc,
                cond: _,
                then: _,
                else_ifs: _,
                else_: __,
            } => loc,
            Stmt::While {
                loc,
                cond: _,
                body: _,
            } => loc,
            Stmt::Repeat {
                loc,
                cond: _,
                body: _,
            } => loc,
            Stmt::For {
                name,
                init: _,
                end: _,
                step: _,
                body: _,
            } => name.loc(),
            Stmt::ForIn {
                name,
                range: _,
                body: _,
            } => name.loc(),
            Stmt::Assign {
                loc,
                lhs: _,
                rhs: _,
            } => loc,
            Stmt::Expr { loc, expr: _ } => loc,
            Stmt::Block { loc, stmts: _ } => loc,
            Stmt::Function {
                name,
                args: _,
                body: _,
            } => match name {
                FunctionName::Method {
                    access_chain,
                    method: _,
                } => access_chain[0].loc(),
                FunctionName::Function { name } => name.loc(),
            },
            Stmt::LocalVar { loc, vars: _ } => loc,
            Stmt::LocalFunction {
                name,
                args: _,
                body: _,
            } => name.loc(),
        }
    }
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
        tokens.push(Token::EOF {
            loc: self.loc.clone(),
        });
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
            "break", "goto", "do", "end", "while", "if", "elseif", "then", "else", "repeat",
            "until", "for", "local", "function", "do", "in", "return", "nil", "false", "true",
            "and", "not", "or",
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

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    UnexpectedEOF,
    SyntaxError,
}
pub struct ParseError {
    pub kind: ErrorKind,
    pub msg: String,
    pub loc: SourceLocation,
}
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}
macro_rules! gen_parse_binary_expr {
    ($self:ident, $parse_lhs:expr, $parse_rhs:expr, $($op:literal),+) => {
        {
            let lhs = $parse_lhs?;
            let mut expr = lhs;
            let ops:Vec<_> = vec![$($op),+].into_iter().map(|s|String::from(s)).collect();
            loop {
                let next = $self.peek();
                if !next.is_eof() {
                    let op = next;
                    let new = op.clone();
                    std::mem::drop(op);
                    let op = new;
                    let (value, _loc) = match &op {
                        Token::Symbol { value, loc } => {
                            (value, loc)
                        }
                        Token::Keyword { value, loc } => {
                            (value, loc)
                        }
                        _=>{break;}
                    };

                    if ops.contains(&value) {
                        $self.advance(1);
                        let rhs = $parse_rhs?;//$self.parse_atom();
                        expr = Rc::new(Expr::BinaryExpr {
                            op: op.clone(),
                            lhs: expr,
                            rhs,
                        });

                    } else {
                        break;
                    }

                } else {
                    break;
                }
            };
            Ok(expr)
        }
    };
}
impl Parser {
    fn advance(&mut self, n: usize) {
        assert!(n > 0);
        self.pos += n;
    }
    fn peek(&mut self) -> &Token {
        self.peek_n(0).unwrap()
    }
    fn peek_n(&mut self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
    fn has(&mut self, s: &str) -> bool {
        let token = self.peek();
        if token.is_eof() {
            return false;
        }
        match token {
            Token::Symbol { value, .. } if *value == s => true,
            Token::Keyword { value, .. } if *value == s => true,
            _ => false,
        }
    }
    fn parse_expr_list(
        &mut self,
        is_parallel_assigment: bool,
    ) -> Result<Vec<Rc<Expr>>, ParseError> {
        let loc = self.peek().loc().clone();

        let mut args = vec![];
        if !is_parallel_assigment {
            assert!(self.has("("));
            self.advance(1);
        }
        let mut cnt = 0;
        loop {
            if self.peek().is_eof() {
                break;
            }
            if !is_parallel_assigment {
                if self.has(")") {
                    break;
                }
            } else {
                if self.has("=") {
                    break;
                }
            }
            if cnt > 0 {
                if !self.has(",") {
                    break;
                }
                self.advance(1);
            }
            let arg = self.parse_expr()?;
            args.push(arg);
            // if !is_parallel_assigment {
            //     if self.has(")") {
            //         break;
            //     }
            // } else {
            //     if self.has("=") {
            //         break;
            //     }
            // }
            cnt += 1;
        }
        if !is_parallel_assigment {
            if !self.has(")") {
                if !self.peek().is_eof() {
                    return Err(self.error(
                        ErrorKind::SyntaxError,
                        &format!("expected ')' in expression"),
                        loc,
                    ));
                } else {
                    let loc = self.peek().loc().clone();
                    return Err(self.error(
                        ErrorKind::UnexpectedEOF,
                        &format!("unexpected EOF when parsing call expression"),
                        loc,
                    ));
                }
            } else {
                self.advance(1);
            }
        }
        Ok(args)
    }

    fn parse_postfix_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        let prefix = self.parse_prefix_expr()?;
        let mut expr = prefix;
        loop {
            if self.has(".") {
                let loc = self.peek().loc().clone();
                self.advance(1);
                let rhs = self.parse_atom()?;
                match &*rhs {
                    Expr::Identifier { .. } => {}
                    _ => {
                        return Err(self.error(
                            ErrorKind::SyntaxError,
                            &format!("expected dot expr"),
                            loc,
                        ));
                    }
                }
                expr = Rc::new(Expr::DotExpr {
                    loc,
                    rhs,
                    lhs: expr,
                });
            } else if self.has("[") {
                let loc = self.peek().loc().clone();
                self.advance(1);
                let rhs = self.parse_expr()?;
                if !self.has("]") {
                    let loc = self.peek().loc().clone();
                    if !self.peek().is_eof() {
                        return Err(self.error(
                            ErrorKind::SyntaxError,
                            &format!("expected ']' expression in index expression"),
                            loc,
                        ));
                    } else {
                        return Err(self.error(
                            ErrorKind::UnexpectedEOF,
                            &format!("unexpected EOF when parsing index expression"),
                            loc,
                        ));
                    }
                }
                self.advance(1);
                expr = Rc::new(Expr::IndexExpr {
                    loc,
                    rhs,
                    lhs: expr,
                });
            } else if self.has("(") {
                let callee = expr.clone();
                let args = self.parse_expr_list(false)?;
                expr = Rc::new(Expr::CallExpr { callee, args });
            } else if self.has(":") {
                self.advance(1);
                let callee = expr.clone();
                let method = self.peek().clone();
                if method.is_eof() {
                    return Err(self.error(
                        ErrorKind::UnexpectedEOF,
                        "unexpected eof in method call expression",
                        method.loc().clone(),
                    ));
                }
                match method {
                    Token::Identifier { .. } => {}
                    _ => {
                        return Err(self.error(
                            ErrorKind::SyntaxError,
                            "expected identifier in method name ",
                            method.loc().clone(),
                        ));
                    }
                }
                self.advance(1);
                let args = self.parse_expr_list(false)?;
                expr = Rc::new(Expr::MethodCallExpr {
                    callee,
                    method,
                    args,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }
    fn eof_loc(&mut self) -> SourceLocation {
        self.tokens.last().unwrap().loc().clone()
    }
    fn parse_function_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        assert!(self.has("function"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let args = self.parse_expr_list(false)?;
        let mut func_args = vec![];
        for arg in args {
            match &*arg {
                Expr::Identifier { token } => func_args.push(token.clone()),
                _ => {
                    return Err(self.error(
                        ErrorKind::SyntaxError,
                        "unexpected expression function definition",
                        arg.loc().clone(),
                    ));
                }
            }
        }
        let body = self.parse_block()?;
        if !self.has("end") {
            return Err(self.error(
                ErrorKind::SyntaxError,
                "expected 'end' in function definition",
                loc,
            ));
        }
        self.advance(1);
        Ok(Rc::new(Expr::FunctionExpr {
            loc,
            args: func_args,
            body,
        }))
    }
    fn parse_atom(&mut self) -> Result<Rc<Expr>, ParseError> {
        let token = self.peek();
        if token.is_eof() {
            let loc = self.eof_loc();
            return Err(self.error(
                ErrorKind::UnexpectedEOF,
                "unexpected EOF when parsing atom '('",
                loc,
            ));
        }
        let token = token.clone();
        match token {
            Token::Number { .. } => {
                self.advance(1);
                Ok(Rc::new(Expr::Literal {
                    token: token.clone(),
                }))
            }
            Token::String { .. } => {
                self.advance(1);
                Ok(Rc::new(Expr::Literal {
                    token: token.clone(),
                }))
            }
            Token::Identifier { .. } => {
                self.advance(1);
                Ok(Rc::new(Expr::Identifier {
                    token: token.clone(),
                }))
            }
            Token::Keyword { ref value, .. }
                if value == "nil" || value == "true" || value == "false" =>
            {
                self.advance(1);
                Ok(Rc::new(Expr::Const {
                    token: token.clone(),
                }))
            }
            Token::Symbol { value, .. } if value == "{" => self.parse_table(),
            Token::Symbol { value, loc } if value == "(" => {
                self.advance(1);
                let expr = if self.has("function") {
                    self.parse_function_expr()?
                } else {
                    self.parse_expr()?
                };

                if !self.has(")") {
                    return Err(self.error(ErrorKind::SyntaxError, "expected ')'", loc.clone()));
                }
                self.advance(1);
                Ok(expr)
            }
            Token::EOF { loc } => Err(self.error(
                ErrorKind::UnexpectedEOF,
                "unexpected EOF when parsing atom",
                loc.clone(),
            )),
            t @ _ => Err(self.error(
                ErrorKind::UnexpectedEOF,
                &format!(
                    "expected literals or identifiers when parsing atom but found {:?}",
                    t
                ),
                t.loc().clone(),
            )),
        }
    }
    fn parse_pow_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(
            self,
            self.parse_postfix_expr(),
            self.parse_postfix_expr(),
            "^"
        )
    }
    fn parse_mul_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(
            self,
            self.parse_pow_expr(),
            self.parse_pow_expr(),
            "*",
            "/",
            "%",
            "//"
        )
    }
    fn parse_add_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(self, self.parse_mul_expr(), self.parse_mul_expr(), "+", "-")
    }
    fn parse_cmp_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(
            self,
            self.parse_add_expr(),
            self.parse_add_expr(),
            "<",
            "<=",
            ">",
            ">=",
            "==",
            "!=",
            "~="
        )
    }
    fn parse_prefix_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        if self.has("not") || self.has("-") || self.has("~") {
            let op = self.peek().clone();
            self.advance(1);
            let expr = self.parse_atom()?;
            Ok(Rc::new(Expr::UnaryExpr { op, arg: expr }))
        } else {
            self.parse_atom()
        }
    }
    fn parse_and_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(
            self,
            self.parse_cmp_expr(),
            self.parse_cmp_expr(),
            "and",
            "&"
        )
    }
    fn parse_or_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        gen_parse_binary_expr!(
            self,
            self.parse_and_expr(),
            self.parse_and_expr(),
            "or",
            "|"
        )
    }
    fn parse_binary_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        self.parse_or_expr()
    }
    fn parse_expr(&mut self) -> Result<Rc<Expr>, ParseError> {
        self.parse_binary_expr()
    }
    fn parse_table(&mut self) -> Result<Rc<Expr>, ParseError> {
        assert!(self.has("{"));
        self.advance(1);
        let loc = self.peek().loc().clone();
        let mut fields = vec![];
        while !self.has("}") {
            fields.push(self.parse_table_field()?);
            if self.has("}") || self.peek().is_eof() {
                break;
            }
            if self.has(",") || self.has(";") {
                self.advance(1);
            } else {
                let loc = self.peek().loc().clone();
                return Err(self.error(
                    ErrorKind::SyntaxError,
                    "expected ',' or ';' in table constructor",
                    loc,
                ));
            }
        }
        if self.peek().is_eof() {
            let loc = self.peek().loc().clone();
            return Err(self.error(
                ErrorKind::UnexpectedEOF,
                "unexpected eof in table constructor",
                loc,
            ));
        }
        self.advance(1);
        Ok(Rc::new(Expr::Table { fields, loc }))
    }
    fn parse_table_field(&mut self) -> Result<TableField, ParseError> {
        if self.has("[") {
            self.advance(1);
            let key = self.parse_expr()?;
            if !self.has("]") {
                let loc = self.peek().loc().clone();
                return Err(self.error(ErrorKind::SyntaxError, "expected ']' in table field", loc));
            }
            self.advance(1);
            if !self.has("=") {
                let loc = self.peek().loc().clone();
                return Err(self.error(ErrorKind::SyntaxError, "expected '=' in table field", loc));
            }
            self.advance(1);
            let value = self.parse_expr()?;
            Ok(TableField::ExprPair(key, value))
        } else {
            let name = self.peek().clone();
            let loc = name.loc().clone();
            let mut is_name_pair = false;
            if name.is_eof() {
                return Err(self.error(
                    ErrorKind::UnexpectedEOF,
                    "unexpected eof in table field",
                    loc,
                ));
            }
            match name {
                Token::Identifier { .. } => {
                    if let Some(next) = self.peek_n(1) {
                        match next {
                            Token::Symbol { value, loc: _ } if value == "=" => {
                                is_name_pair = true;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            if is_name_pair {
                self.advance(2);
                let expr = self.parse_expr()?;
                Ok(TableField::NamePair(name, expr))
            } else {
                Ok(TableField::ArrayEntry(self.parse_expr()?))
            }
        }
    }
    fn parse_repeat_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("repeat"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let body = self.parse_block()?;
        if !self.has("until") {
            return Err(self.error(ErrorKind::SyntaxError, "expected 'until' in while", loc));
        }
        let cond = self.parse_expr()?;
        Ok(Rc::new(Stmt::Repeat { loc, cond, body }))
    }
    fn parse_return_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("return"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        if self.has("end") {
            Ok(Rc::new(Stmt::Return { loc, expr:None }))
        } else {
            let expr = self.parse_expr()?;
            Ok(Rc::new(Stmt::Return { loc, expr:Some(expr) }))
        }
    }
    fn parse_for_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("for"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let name = self.peek().clone();
        if name.is_eof() {
            return Err(self.error(ErrorKind::UnexpectedEOF, "unexpected EOF in for loop", loc));
        }
        match &name {
            Token::Symbol { .. } => {}
            _ => {
                return Err(self.error(
                    ErrorKind::SyntaxError,
                    "expected identifier in for loop",
                    name.loc().clone(),
                ));
            }
        }
        self.advance(1);
        if self.has("=") {
            self.advance(1);
            let init = self.parse_expr()?;
            if !self.has(",") {
                let loc = self.peek().loc().clone();
                return Err(self.error(ErrorKind::SyntaxError, "expected ',' in for loop", loc));
            }
            self.advance(1);
            let end = self.parse_expr()?;
            let step = if self.has(",") {
                self.advance(1);
                Some(self.parse_expr()?)
            } else {
                None
            };
            if !self.has("do") {
                return Err(self.error(ErrorKind::SyntaxError, "expected 'do' in for", loc));
            }
            self.advance(1);
            let body = self.parse_block()?;
            let loc = self.peek().loc().clone();
            if !self.has("end") {
                return Err(self.error(ErrorKind::SyntaxError, "expected 'end' in for", loc));
            }
            self.advance(1);
            Ok(Rc::new(Stmt::For {
                name,
                init,
                end,
                step,
                body,
            }))
        } else if self.has("in") {
            self.advance(1);
            let range = self.parse_expr()?;
            let loc = self.peek().loc().clone();
            if !self.has("do") {
                return Err(self.error(ErrorKind::SyntaxError, "expected 'do' in for", loc));
            }
            self.advance(1);
            let body = self.parse_block()?;
            let loc = self.peek().loc().clone();
            if !self.has("end") {
                return Err(self.error(ErrorKind::SyntaxError, "expected 'end' in for", loc));
            }
            self.advance(1);
            Ok(Rc::new(Stmt::ForIn { range, body, name }))
        } else {
            let loc = self.peek().loc().clone();
            Err(self.error(
                ErrorKind::SyntaxError,
                "expected 'in' or '=' in for loop",
                loc,
            ))
        }
    }
    fn parse_while_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("while"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let cond = self.parse_expr()?;
        if !self.has("do") {
            return Err(self.error(ErrorKind::SyntaxError, "expected 'do' in while", loc));
        }
        self.advance(1);
        let body = self.parse_block()?;
        if !self.has("end") {
            return Err(self.error(ErrorKind::SyntaxError, "expected 'end' in while", loc));
        }
        self.advance(1);
        Ok(Rc::new(Stmt::While { loc, cond, body }))
    }
    fn parse_if_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("if"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let cond = self.parse_expr()?;
        if !self.has("then") {
            return Err(self.error(ErrorKind::SyntaxError, "expected 'then' in if", loc));
        }
        self.advance(1);
        let then = self.parse_block()?;
        let mut else_ifs = vec![];
        while self.has("elseif") {
            self.advance(1);
            let loc = self.peek().loc().clone();
            let cond = self.parse_expr()?;
            if !self.has("then") {
                return Err(self.error(ErrorKind::SyntaxError, "expected 'then' in elseif", loc));
            }
            self.advance(1);
            let body = self.parse_block()?;
            else_ifs.push((cond, body));
        }
        let else_ = if self.has("else") {
            self.advance(1);
            Some(self.parse_block()?)
        } else {
            None
        };
        if !self.has("end") {
            return Err(self.error(ErrorKind::SyntaxError, "expected 'end' in if", loc));
        }
        self.advance(1);
        Ok(Rc::new(Stmt::If {
            loc,
            cond,
            then,
            else_ifs,
            else_,
        }))
    }
    fn parse_assignment_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        let loc = self.peek().loc().clone();
        let lhs = self.parse_expr_list(true)?;
        if self.has("=") {
            self.advance(1);
            let rhs = self.parse_expr_list(true)?;
            Ok(Rc::new(Stmt::Assign { loc, lhs, rhs }))
        } else {
            // if lhs.len() > 1 {
            //     return Err(self.error(
            //         ErrorKind::SyntaxError,
            //         "parallel assignment expect right hand side",
            //         loc,
            //     ));
            // }
            Ok(Rc::new(Stmt::Expr {
                loc,
                expr: lhs[0].clone(),
            }))
        }
    }
    fn parse_function(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("function"));
        let loc = self.peek().loc().clone();
        self.advance(1);
        let name = self.peek().clone();
        self.advance(1);
        if name.is_eof() {
            return Err(self.error(
                ErrorKind::UnexpectedEOF,
                "unexpected EOF in function definition",
                loc,
            ));
        }
        match &name {
            Token::Identifier { .. } => {}
            _ => {
                return Err(self.error(
                    ErrorKind::SyntaxError,
                    "expected identifier in function",
                    name.loc().clone(),
                ));
            }
        }
        let mut func_name = FunctionName::Function { name };
        while self.has(".") {
            self.advance(1);
            let name = self.peek().clone();
            self.advance(1);
            if name.is_eof() {
                return Err(self.error(
                    ErrorKind::UnexpectedEOF,
                    "unexpected EOF in function definition",
                    loc,
                ));
            }
            match &name {
                Token::Identifier { .. } => {}
                _ => {
                    return Err(self.error(
                        ErrorKind::SyntaxError,
                        "expected identifier in function",
                        name.loc().clone(),
                    ));
                }
            }
            func_name = match func_name {
                FunctionName::Method {
                    access_chain,
                    method,
                } => {
                    let mut access_chain = access_chain;
                    access_chain.push(name);
                    FunctionName::Method {
                        access_chain,
                        method,
                    }
                }
                FunctionName::Function { name: first } => FunctionName::Method {
                    access_chain: vec![first, name],
                    method: None,
                },
            };
        }
        if self.has(":") {
            self.advance(1);
            let name = self.peek().clone();
            self.advance(1);
            if name.is_eof() {
                return Err(self.error(
                    ErrorKind::UnexpectedEOF,
                    "unexpected EOF in method definition",
                    loc,
                ));
            }
            match &name {
                Token::Identifier { .. } => {}
                _ => {
                    return Err(self.error(
                        ErrorKind::SyntaxError,
                        "expected identifier in method",
                        name.loc().clone(),
                    ));
                }
            }
            func_name = match func_name {
                FunctionName::Method {
                    access_chain,
                    method: _,
                } => FunctionName::Method {
                    access_chain,
                    method: Some(name),
                },
                FunctionName::Function { name: first } => FunctionName::Method {
                    access_chain: vec![first, name.clone()],
                    method: Some(name),
                },
            };
        }

        if !self.has("(") {
            let loc = self.peek().loc().clone();
            return Err(self.error(
                ErrorKind::SyntaxError,
                "expected '(' in function definition",
                loc,
            ));
        }
        let args = self.parse_expr_list(false)?;
        let mut func_args = vec![];
        for arg in args {
            match &*arg {
                Expr::Identifier { token } => func_args.push(token.clone()),
                _ => {
                    return Err(self.error(
                        ErrorKind::SyntaxError,
                        "unexpected expression function definition",
                        arg.loc().clone(),
                    ));
                }
            }
        }
        let body = self.parse_block()?;
        if !self.has("end") {
            return Err(self.error(
                ErrorKind::SyntaxError,
                "expected 'end' in function definition",
                loc,
            ));
        }
        self.advance(1);
        Ok(Rc::new(Stmt::Function {
            name: func_name,
            args: func_args,
            body,
        }))
    }
    fn parse_local(&mut self) -> Result<Rc<Stmt>, ParseError> {
        assert!(self.has("local"));
        self.advance(1);
        if self.has("function") {
            let function = self.parse_function()?;
            match &*function {
                Stmt::Function { name, args, body } => match name {
                    FunctionName::Method { .. } => Err(self.error(
                        ErrorKind::SyntaxError,
                        "method definition not allowed in local function",
                        function.loc().clone(),
                    )),
                    FunctionName::Function { name } => Ok(Rc::new(Stmt::LocalFunction {
                        args: args.clone(),
                        body: body.clone(),
                        name: name.clone(),
                    })),
                },
                _ => unreachable!(),
            }
        } else {
            let assignment = self.parse_assignment_stmt()?;
            Ok(Rc::new(Stmt::LocalVar {
                loc: assignment.loc().clone(),
                vars: assignment,
            }))
        }
    }
    fn parse_stmt(&mut self) -> Result<Rc<Stmt>, ParseError> {
        if self.has("if") {
            self.parse_if_stmt()
        } else if self.has("while") {
            self.parse_while_stmt()
        } else if self.has("for") {
            self.parse_for_stmt()
        } else if self.has("return") {
            self.parse_return_stmt()
        } else if self.has("repeat") {
            self.parse_repeat_stmt()
        } else if self.has("function") {
            self.parse_function()
        } else if self.has("local") {
            self.parse_local()
        } else {
            self.parse_assignment_stmt()
        }
    }
    fn parse_block(&mut self) -> Result<Rc<Stmt>, ParseError> {
        let loc = self.peek().loc().clone();
        let mut stmts = vec![];
        let mut has_return = false;
        while !self.has("end")
            && !self.has("elseif")
            && !self.has("else")
            && !self.has("until")
            && !self.peek().is_eof()
        {
            let loc = self.peek().loc().clone();
            let stmt = self.parse_stmt()?;
            match &*stmt {
                Stmt::Return { .. } => {
                    if has_return {
                        return Err(self.error(
                            ErrorKind::SyntaxError,
                            "multiple return statement in block",
                            loc,
                        ));
                    }
                    has_return = true;
                }
                _ => {}
            }
            stmts.push(stmt);
        }
        Ok(Rc::new(Stmt::Block { loc, stmts }))
    }

    fn error(&mut self, kind: ErrorKind, msg: &str, loc: SourceLocation) -> ParseError {
        ParseError {
            loc,
            kind,
            msg: String::from(msg),
        }
    }
    fn run(&mut self) -> Result<Rc<Stmt>, ParseError> {
        let ret = self.parse_block()?;
        assert!(self.peek().is_eof());
        Ok(ret)
    }
}

pub fn parse_impl(tokens: Vec<Token>) -> Result<Rc<Stmt>, ParseError> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.run()
}
