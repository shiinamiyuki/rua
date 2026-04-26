//! Token types for the Lua lexer.

use std::fmt;

/// Source location: line and column (both 1-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Location { line, column }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A token with its kind and source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, line: u32, column: u32) -> Self {
        Token {
            kind,
            location: Location::new(line, column),
        }
    }
}

/// All possible token kinds in Lua 5.5.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Name(String),

    // Keywords
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Global,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    // Arithmetic operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    Hash,       // #
    SlashSlash, // //

    // Bitwise operators
    Ampersand, // &
    Tilde,     // ~
    Pipe,      // |
    LtLt,      // <<
    GtGt,      // >>

    // Comparison operators
    EqEq,   // ==
    TildeEq, // ~=
    LtEq,   // <=
    GtEq,   // >=
    Lt,     // <
    Gt,     // >

    // Assignment
    Eq, // =

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    ColonColon, // ::

    // Punctuation
    Semicolon, // ;
    Colon,     // :
    Comma,     // ,
    Dot,       // .
    DotDot,    // ..
    DotDotDot, // ...

    // End of file
    Eof,
}

impl TokenKind {
    /// Returns the keyword TokenKind for a given name, or None if not a keyword.
    pub fn keyword(name: &str) -> Option<TokenKind> {
        match name {
            "and" => Some(TokenKind::And),
            "break" => Some(TokenKind::Break),
            "do" => Some(TokenKind::Do),
            "else" => Some(TokenKind::Else),
            "elseif" => Some(TokenKind::ElseIf),
            "end" => Some(TokenKind::End),
            "false" => Some(TokenKind::False),
            "for" => Some(TokenKind::For),
            "function" => Some(TokenKind::Function),
            "global" => Some(TokenKind::Global),
            "goto" => Some(TokenKind::Goto),
            "if" => Some(TokenKind::If),
            "in" => Some(TokenKind::In),
            "local" => Some(TokenKind::Local),
            "nil" => Some(TokenKind::Nil),
            "not" => Some(TokenKind::Not),
            "or" => Some(TokenKind::Or),
            "repeat" => Some(TokenKind::Repeat),
            "return" => Some(TokenKind::Return),
            "then" => Some(TokenKind::Then),
            "true" => Some(TokenKind::True),
            "until" => Some(TokenKind::Until),
            "while" => Some(TokenKind::While),
            _ => None,
        }
    }

    /// Returns a human-readable name for this token kind (for error messages).
    pub fn describe(&self) -> &'static str {
        match self {
            TokenKind::Integer(_) => "<integer>",
            TokenKind::Float(_) => "<number>",
            TokenKind::String(_) => "<string>",
            TokenKind::Name(_) => "<name>",
            TokenKind::And => "'and'",
            TokenKind::Break => "'break'",
            TokenKind::Do => "'do'",
            TokenKind::Else => "'else'",
            TokenKind::ElseIf => "'elseif'",
            TokenKind::End => "'end'",
            TokenKind::False => "'false'",
            TokenKind::For => "'for'",
            TokenKind::Function => "'function'",
            TokenKind::Global => "'global'",
            TokenKind::Goto => "'goto'",
            TokenKind::If => "'if'",
            TokenKind::In => "'in'",
            TokenKind::Local => "'local'",
            TokenKind::Nil => "'nil'",
            TokenKind::Not => "'not'",
            TokenKind::Or => "'or'",
            TokenKind::Repeat => "'repeat'",
            TokenKind::Return => "'return'",
            TokenKind::Then => "'then'",
            TokenKind::True => "'true'",
            TokenKind::Until => "'until'",
            TokenKind::While => "'while'",
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Star => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::Percent => "'%'",
            TokenKind::Caret => "'^'",
            TokenKind::Hash => "'#'",
            TokenKind::SlashSlash => "'//'",
            TokenKind::Ampersand => "'&'",
            TokenKind::Tilde => "'~'",
            TokenKind::Pipe => "'|'",
            TokenKind::LtLt => "'<<'",
            TokenKind::GtGt => "'>>'",
            TokenKind::EqEq => "'=='",
            TokenKind::TildeEq => "'~='",
            TokenKind::LtEq => "'<='",
            TokenKind::GtEq => "'>='",
            TokenKind::Lt => "'<'",
            TokenKind::Gt => "'>'",
            TokenKind::Eq => "'='",
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::LBracket => "'['",
            TokenKind::RBracket => "']'",
            TokenKind::ColonColon => "'::'",
            TokenKind::Semicolon => "';'",
            TokenKind::Colon => "':'",
            TokenKind::Comma => "','",
            TokenKind::Dot => "'.'",
            TokenKind::DotDot => "'..'",
            TokenKind::DotDotDot => "'...'",
            TokenKind::Eof => "<eof>",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.describe())
    }
}
