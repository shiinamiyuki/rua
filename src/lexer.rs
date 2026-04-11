//! Lexical analyzer (tokenizer) for Lua 5.5 source code.
//!
//! Hand-written lexer operating on `&[u8]` (byte slice). Handles all Lua 5.5
//! lexical elements: keywords, operators, short/long strings, numerals, comments.
//! Tracks source location (line, column) for error reporting.

use crate::token::{Location, Token, TokenKind};

/// A lexical error with location info.
#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub location: Location,
}

impl LexError {
    fn new(message: impl Into<String>, line: u32, column: u32) -> Self {
        LexError {
            message: message.into(),
            location: Location::new(line, column),
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.location.line, self.location.column, self.message)
    }
}

impl std::error::Error for LexError {}

/// Lua 5.5 lexer. Operates on a byte slice and produces a stream of tokens.
pub struct Lexer<'src> {
    source: &'src [u8],
    /// Name of the source (for error messages)
    source_name: String,
    /// Current position in the source byte slice
    pos: usize,
    /// Current line (1-based)
    line: u32,
    /// Current column (1-based)
    column: u32,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'src [u8], source_name: impl Into<String>) -> Self {
        Lexer {
            source,
            source_name: source_name.into(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenize the entire source, returning all tokens (including final Eof).
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            let is_eof = tok.kind == TokenKind::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    /// Get the next token from the source.
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments()?;

        let start_line = self.line;
        let start_col = self.column;

        let Some(ch) = self.peek() else {
            return Ok(Token::new(TokenKind::Eof, start_line, start_col));
        };

        match ch {
            // Single-character tokens (unambiguous)
            b'+' => { self.advance(); Ok(Token::new(TokenKind::Plus, start_line, start_col)) }
            b'*' => { self.advance(); Ok(Token::new(TokenKind::Star, start_line, start_col)) }
            b'%' => { self.advance(); Ok(Token::new(TokenKind::Percent, start_line, start_col)) }
            b'^' => { self.advance(); Ok(Token::new(TokenKind::Caret, start_line, start_col)) }
            b'#' => { self.advance(); Ok(Token::new(TokenKind::Hash, start_line, start_col)) }
            b'&' => { self.advance(); Ok(Token::new(TokenKind::Ampersand, start_line, start_col)) }
            b'|' => { self.advance(); Ok(Token::new(TokenKind::Pipe, start_line, start_col)) }
            b'(' => { self.advance(); Ok(Token::new(TokenKind::LParen, start_line, start_col)) }
            b')' => { self.advance(); Ok(Token::new(TokenKind::RParen, start_line, start_col)) }
            b'{' => { self.advance(); Ok(Token::new(TokenKind::LBrace, start_line, start_col)) }
            b'}' => { self.advance(); Ok(Token::new(TokenKind::RBrace, start_line, start_col)) }
            b']' => { self.advance(); Ok(Token::new(TokenKind::RBracket, start_line, start_col)) }
            b';' => { self.advance(); Ok(Token::new(TokenKind::Semicolon, start_line, start_col)) }
            b',' => { self.advance(); Ok(Token::new(TokenKind::Comma, start_line, start_col)) }

            // Multi-character operators and tokens
            b'-' => self.lex_minus(start_line, start_col),
            b'/' => self.lex_slash(start_line, start_col),
            b'~' => self.lex_tilde(start_line, start_col),
            b'<' => self.lex_lt(start_line, start_col),
            b'>' => self.lex_gt(start_line, start_col),
            b'=' => self.lex_eq(start_line, start_col),
            b':' => self.lex_colon(start_line, start_col),
            b'.' => self.lex_dot(start_line, start_col),
            b'[' => self.lex_lbracket(start_line, start_col),

            // Strings
            b'\'' | b'"' => self.lex_short_string(start_line, start_col),

            // Numerals
            b'0'..=b'9' => self.lex_numeral(start_line, start_col),

            // Names and keywords
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_name(start_line, start_col),

            _ => Err(self.error(format!("unexpected character '{}'", ch as char))),
        }
    }

    // ─── Character access helpers ───────────────────────────────────────

    fn peek(&self) -> Option<u8> {
        self.source.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.source.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> u8 {
        let ch = self.source[self.pos];
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn error(&self, message: impl Into<String>) -> LexError {
        LexError::new(message, self.line, self.column)
    }

    fn error_at(&self, message: impl Into<String>, line: u32, col: u32) -> LexError {
        LexError::new(message, line, col)
    }

    // ─── Whitespace and comment skipping ────────────────────────────────

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            // Skip whitespace
            while let Some(ch) = self.peek() {
                match ch {
                    b' ' | b'\t' | b'\x0B' | b'\x0C' | b'\r' => {
                        self.advance();
                    }
                    b'\n' => {
                        self.advance();
                    }
                    _ => break,
                }
            }

            // Check for comments
            if self.peek() == Some(b'-') && self.peek_at(1) == Some(b'-') {
                self.advance(); // skip first -
                self.advance(); // skip second -
                // Check for long comment
                if self.peek() == Some(b'[') {
                    if let Some(level) = self.check_long_bracket_open() {
                        self.read_long_string(level)?;
                        continue; // after long comment, keep skipping whitespace
                    }
                }
                // Short comment: skip to end of line
                while let Some(ch) = self.peek() {
                    if ch == b'\n' {
                        break;
                    }
                    self.advance();
                }
                continue; // after short comment, keep skipping whitespace
            }

            break;
        }
        Ok(())
    }

    // ─── Check for long bracket opening `[=*[` ─────────────────────────

    /// If current position starts with `[=*[`, return the level (number of `=`).
    /// Does NOT consume any characters.
    fn check_long_bracket_open(&self) -> Option<usize> {
        if self.peek() != Some(b'[') {
            return None;
        }
        let mut offset = 1;
        while self.peek_at(offset) == Some(b'=') {
            offset += 1;
        }
        if self.peek_at(offset) == Some(b'[') {
            Some(offset - 1) // number of '=' signs
        } else {
            None
        }
    }

    // ─── Long string/comment reading ────────────────────────────────────

    /// Read a long string `[=*[..]=*]` (opening bracket already detected but not consumed).
    /// Returns the string content.
    fn read_long_string(&mut self, level: usize) -> Result<Vec<u8>, LexError> {
        let start_line = self.line;
        let start_col = self.column;

        // Consume opening `[=*[`
        self.advance(); // [
        for _ in 0..level {
            self.advance(); // =
        }
        self.advance(); // [

        // If the first character is a newline, skip it
        if self.peek() == Some(b'\n') {
            self.advance();
        } else if self.peek() == Some(b'\r') {
            self.advance();
            if self.peek() == Some(b'\n') {
                self.advance();
            }
        }

        let mut buf = Vec::new();
        loop {
            let Some(ch) = self.peek() else {
                return Err(self.error_at(
                    format!("unfinished long string (starting at line {})", start_line),
                    start_line,
                    start_col,
                ));
            };

            if ch == b']' {
                // Check for closing long bracket
                if self.check_long_bracket_close(level) {
                    // Consume closing `]=*]`
                    self.advance(); // ]
                    for _ in 0..level {
                        self.advance(); // =
                    }
                    self.advance(); // ]
                    return Ok(buf);
                }
            }

            // Normalize end-of-line sequences to \n
            if ch == b'\r' {
                self.advance();
                if self.peek() == Some(b'\n') {
                    self.advance();
                }
                buf.push(b'\n');
            } else if ch == b'\n' {
                self.advance();
                // \n\r also gets normalized
                if self.peek() == Some(b'\r') {
                    self.advance();
                }
                buf.push(b'\n');
            } else {
                self.advance();
                buf.push(ch);
            }
        }
    }

    /// Check if current position starts with `]=*]` with given level.
    /// Does NOT consume any characters.
    fn check_long_bracket_close(&self, level: usize) -> bool {
        if self.peek() != Some(b']') {
            return false;
        }
        let mut offset = 1;
        for _ in 0..level {
            if self.peek_at(offset) != Some(b'=') {
                return false;
            }
            offset += 1;
        }
        self.peek_at(offset) == Some(b']')
    }

    // ─── Individual token lexers ────────────────────────────────────────

    fn lex_minus(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '-'
        // Comments are already handled in skip_whitespace_and_comments
        Ok(Token::new(TokenKind::Minus, line, col))
    }

    fn lex_slash(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '/'
        if self.peek() == Some(b'/') {
            self.advance();
            Ok(Token::new(TokenKind::SlashSlash, line, col))
        } else {
            Ok(Token::new(TokenKind::Slash, line, col))
        }
    }

    fn lex_tilde(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '~'
        if self.peek() == Some(b'=') {
            self.advance();
            Ok(Token::new(TokenKind::TildeEq, line, col))
        } else {
            Ok(Token::new(TokenKind::Tilde, line, col))
        }
    }

    fn lex_lt(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '<'
        match self.peek() {
            Some(b'=') => { self.advance(); Ok(Token::new(TokenKind::LtEq, line, col)) }
            Some(b'<') => { self.advance(); Ok(Token::new(TokenKind::LtLt, line, col)) }
            _ => Ok(Token::new(TokenKind::Lt, line, col)),
        }
    }

    fn lex_gt(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '>'
        match self.peek() {
            Some(b'=') => { self.advance(); Ok(Token::new(TokenKind::GtEq, line, col)) }
            Some(b'>') => { self.advance(); Ok(Token::new(TokenKind::GtGt, line, col)) }
            _ => Ok(Token::new(TokenKind::Gt, line, col)),
        }
    }

    fn lex_eq(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume '='
        if self.peek() == Some(b'=') {
            self.advance();
            Ok(Token::new(TokenKind::EqEq, line, col))
        } else {
            Ok(Token::new(TokenKind::Eq, line, col))
        }
    }

    fn lex_colon(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume ':'
        if self.peek() == Some(b':') {
            self.advance();
            Ok(Token::new(TokenKind::ColonColon, line, col))
        } else {
            Ok(Token::new(TokenKind::Colon, line, col))
        }
    }

    fn lex_dot(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        self.advance(); // consume first '.'
        match self.peek() {
            Some(b'.') => {
                self.advance(); // consume second '.'
                if self.peek() == Some(b'.') {
                    self.advance();
                    Ok(Token::new(TokenKind::DotDotDot, line, col))
                } else {
                    Ok(Token::new(TokenKind::DotDot, line, col))
                }
            }
            Some(b'0'..=b'9') => {
                // Float starting with '.'
                self.lex_decimal_number_from_dot(line, col)
            }
            _ => Ok(Token::new(TokenKind::Dot, line, col)),
        }
    }

    fn lex_lbracket(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        // Could be long string `[=*[` or just `[`
        if let Some(level) = self.check_long_bracket_open() {
            let content = self.read_long_string(level)?;
            Ok(Token::new(TokenKind::String(content), line, col))
        } else {
            self.advance(); // consume '['
            Ok(Token::new(TokenKind::LBracket, line, col))
        }
    }

    // ─── Name / keyword lexer ───────────────────────────────────────────

    fn lex_name(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }
        let name = std::str::from_utf8(&self.source[start..self.pos])
            .expect("identifiers are ASCII");

        if let Some(kw) = TokenKind::keyword(name) {
            Ok(Token::new(kw, line, col))
        } else {
            Ok(Token::new(TokenKind::Name(name.to_string()), line, col))
        }
    }

    // ─── Short string lexer ────────────────────────────────────────────

    fn lex_short_string(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        let quote = self.advance(); // consume opening quote
        let mut buf = Vec::new();

        loop {
            let Some(ch) = self.peek() else {
                return Err(self.error_at("unfinished string", line, col));
            };

            match ch {
                c if c == quote => {
                    self.advance(); // consume closing quote
                    return Ok(Token::new(TokenKind::String(buf), line, col));
                }
                b'\n' | b'\r' => {
                    return Err(self.error_at("unfinished string", line, col));
                }
                b'\\' => {
                    self.advance(); // consume backslash
                    let esc = self.peek().ok_or_else(|| {
                        self.error_at("unfinished string", line, col)
                    })?;
                    match esc {
                        b'a' => { self.advance(); buf.push(0x07); }
                        b'b' => { self.advance(); buf.push(0x08); }
                        b'f' => { self.advance(); buf.push(0x0C); }
                        b'n' => { self.advance(); buf.push(b'\n'); }
                        b'r' => { self.advance(); buf.push(b'\r'); }
                        b't' => { self.advance(); buf.push(b'\t'); }
                        b'v' => { self.advance(); buf.push(0x0B); }
                        b'\\' => { self.advance(); buf.push(b'\\'); }
                        b'\'' => { self.advance(); buf.push(b'\''); }
                        b'"' => { self.advance(); buf.push(b'"'); }
                        b'\n' => {
                            self.advance();
                            if self.peek() == Some(b'\r') { self.advance(); }
                            buf.push(b'\n');
                        }
                        b'\r' => {
                            self.advance();
                            if self.peek() == Some(b'\n') { self.advance(); }
                            buf.push(b'\n');
                        }
                        b'x' => {
                            self.advance(); // consume 'x'
                            let hi = self.read_hex_digit()?;
                            let lo = self.read_hex_digit()?;
                            buf.push((hi << 4) | lo);
                        }
                        b'u' => {
                            self.advance(); // consume 'u'
                            if self.peek() != Some(b'{') {
                                return Err(self.error("missing '{' in \\u{xxxx}"));
                            }
                            self.advance(); // consume '{'
                            let code = self.read_unicode_escape()?;
                            if self.peek() != Some(b'}') {
                                return Err(self.error("missing '}' in \\u{xxxx}"));
                            }
                            self.advance(); // consume '}'
                            self.encode_utf8(code, &mut buf)?;
                        }
                        b'z' => {
                            self.advance(); // consume 'z'
                            // Skip following whitespace
                            while let Some(c) = self.peek() {
                                match c {
                                    b' ' | b'\t' | b'\x0B' | b'\x0C' | b'\r' | b'\n' => {
                                        self.advance();
                                    }
                                    _ => break,
                                }
                            }
                        }
                        b'0'..=b'9' => {
                            // Decimal escape: up to 3 digits
                            let mut val: u16 = 0;
                            for _ in 0..3 {
                                if let Some(d) = self.peek() {
                                    if d.is_ascii_digit() {
                                        val = val * 10 + (d - b'0') as u16;
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                            if val > 255 {
                                return Err(self.error("decimal escape too large"));
                            }
                            buf.push(val as u8);
                        }
                        _ => {
                            return Err(self.error(format!(
                                "invalid escape sequence '\\{}'",
                                esc as char
                            )));
                        }
                    }
                }
                _ => {
                    self.advance();
                    buf.push(ch);
                }
            }
        }
    }

    fn read_hex_digit(&mut self) -> Result<u8, LexError> {
        let Some(ch) = self.peek() else {
            return Err(self.error("invalid escape sequence"));
        };
        let val = match ch {
            b'0'..=b'9' => ch - b'0',
            b'a'..=b'f' => ch - b'a' + 10,
            b'A'..=b'F' => ch - b'A' + 10,
            _ => return Err(self.error("invalid escape sequence")),
        };
        self.advance();
        Ok(val)
    }

    fn read_unicode_escape(&mut self) -> Result<u32, LexError> {
        // Read hex digits until '}'
        let mut val: u32 = 0;
        let mut count = 0;
        loop {
            let Some(ch) = self.peek() else {
                return Err(self.error("missing '}' in \\u{xxxx}"));
            };
            match ch {
                b'0'..=b'9' => { val = val * 16 + (ch - b'0') as u32; }
                b'a'..=b'f' => { val = val * 16 + (ch - b'a' + 10) as u32; }
                b'A'..=b'F' => { val = val * 16 + (ch - b'A' + 10) as u32; }
                b'}' => break,
                _ => return Err(self.error("invalid Unicode escape")),
            }
            count += 1;
            self.advance();
            if val > 0x7FFF_FFFF {
                return Err(self.error("UTF-8 value too large"));
            }
        }
        if count == 0 {
            return Err(self.error("missing Unicode value in \\u{xxxx}"));
        }
        Ok(val)
    }

    fn encode_utf8(&self, code: u32, buf: &mut Vec<u8>) -> Result<(), LexError> {
        if code > 0x7FFF_FFFF {
            return Err(self.error("UTF-8 value too large"));
        }
        if code < 0x80 {
            buf.push(code as u8);
        } else if code < 0x800 {
            buf.push(0xC0 | (code >> 6) as u8);
            buf.push(0x80 | (code & 0x3F) as u8);
        } else if code < 0x10000 {
            buf.push(0xE0 | (code >> 12) as u8);
            buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
            buf.push(0x80 | (code & 0x3F) as u8);
        } else if code < 0x200000 {
            buf.push(0xF0 | (code >> 18) as u8);
            buf.push(0x80 | ((code >> 12) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
            buf.push(0x80 | (code & 0x3F) as u8);
        } else if code < 0x4000000 {
            buf.push(0xF8 | (code >> 24) as u8);
            buf.push(0x80 | ((code >> 18) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 12) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
            buf.push(0x80 | (code & 0x3F) as u8);
        } else {
            buf.push(0xFC | (code >> 30) as u8);
            buf.push(0x80 | ((code >> 24) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 18) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 12) & 0x3F) as u8);
            buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
            buf.push(0x80 | (code & 0x3F) as u8);
        }
        Ok(())
    }

    // ─── Numeral lexer ─────────────────────────────────────────────────

    fn lex_numeral(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        let start = self.pos;

        // Check for hex: 0x or 0X
        if self.peek() == Some(b'0') && matches!(self.peek_at(1), Some(b'x' | b'X')) {
            self.advance(); // '0'
            self.advance(); // 'x' or 'X'
            return self.lex_hex_numeral(start, line, col);
        }

        // Decimal numeral
        self.lex_decimal_numeral(start, line, col)
    }

    fn lex_decimal_numeral(&mut self, start: usize, line: u32, col: u32) -> Result<Token, LexError> {
        // Read integer part
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }

        let mut is_float = false;

        // Fractional part
        if self.peek() == Some(b'.') {
            // Make sure it's not `..` or `...`
            if self.peek_at(1) != Some(b'.') {
                is_float = true;
                self.advance(); // consume '.'
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() || ch == b'_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        // Exponent part
        if matches!(self.peek(), Some(b'e' | b'E')) {
            is_float = true;
            self.advance(); // consume 'e'/'E'
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.advance();
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err(self.error_at("malformed number", line, col));
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Collect the text (stripping underscores for parsing)
        let raw = &self.source[start..self.pos];
        let clean: String = raw.iter()
            .filter(|&&b| b != b'_')
            .map(|&b| b as char)
            .collect();

        if is_float {
            let val: f64 = clean.parse().map_err(|_| {
                self.error_at("malformed number", line, col)
            })?;
            Ok(Token::new(TokenKind::Float(val), line, col))
        } else {
            // Try integer first, fall back to float on overflow
            if let Ok(val) = clean.parse::<i64>() {
                Ok(Token::new(TokenKind::Integer(val), line, col))
            } else {
                // Decimal integer overflow → float
                let val: f64 = clean.parse().map_err(|_| {
                    self.error_at("malformed number", line, col)
                })?;
                Ok(Token::new(TokenKind::Float(val), line, col))
            }
        }
    }

    /// Lex a decimal number that started with `.` (dot already consumed).
    fn lex_decimal_number_from_dot(&mut self, line: u32, col: u32) -> Result<Token, LexError> {
        // The dot was already consumed before calling this; build the number starting from '0.'
        let start = self.pos;

        // Read fractional digits
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }

        // Exponent part
        if matches!(self.peek(), Some(b'e' | b'E')) {
            self.advance();
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.advance();
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err(self.error_at("malformed number", line, col));
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let frac_raw = &self.source[start..self.pos];
        let mut clean = String::from("0.");
        for &b in frac_raw {
            if b != b'_' {
                clean.push(b as char);
            }
        }

        let val: f64 = clean.parse().map_err(|_| {
            self.error_at("malformed number", line, col)
        })?;
        Ok(Token::new(TokenKind::Float(val), line, col))
    }

    fn lex_hex_numeral(&mut self, start: usize, line: u32, col: u32) -> Result<Token, LexError> {
        // We've consumed '0x' / '0X'
        // Read hex digits
        let hex_start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_hexdigit() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }

        let mut is_float = false;

        // Optional fractional part
        if self.peek() == Some(b'.') {
            is_float = true;
            self.advance(); // consume '.'
            while let Some(ch) = self.peek() {
                if ch.is_ascii_hexdigit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Either integer or fractional hex digits must be present
        let digits_end = self.pos;
        let digit_count = self.source[hex_start..digits_end]
            .iter()
            .filter(|b| b.is_ascii_hexdigit())
            .count();
        if digit_count == 0 {
            return Err(self.error_at("malformed number", line, col));
        }

        // Optional binary exponent
        if matches!(self.peek(), Some(b'p' | b'P')) {
            is_float = true;
            self.advance(); // consume 'p'/'P'
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.advance();
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err(self.error_at("malformed number", line, col));
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let raw = &self.source[start..self.pos];
        let clean: String = raw.iter()
            .filter(|&&b| b != b'_')
            .map(|&b| b as char)
            .collect();

        if is_float {
            let val = parse_hex_float(&clean).map_err(|e| {
                self.error_at(e, line, col)
            })?;
            Ok(Token::new(TokenKind::Float(val), line, col))
        } else {
            // Hex integer: wraps on overflow
            let hex_digits: String = clean[2..].chars().collect(); // skip "0x"
            let val = parse_hex_integer(&hex_digits);
            Ok(Token::new(TokenKind::Integer(val), line, col))
        }
    }
}

// ─── Hex float parsing ─────────────────────────────────────────────────────

/// Parse a hexadecimal floating-point string like "0x1.fp10".
/// Accepts the full syntax: 0x[digits][.digits][p[+-]digits]
fn parse_hex_float(s: &str) -> Result<f64, String> {
    // Strip 0x/0X prefix
    let s = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))
        .ok_or("malformed number")?;

    // Split at the exponent
    let (mantissa_str, exp_str) = match s.find(|c: char| c == 'p' || c == 'P') {
        Some(idx) => (&s[..idx], Some(&s[idx + 1..])),
        None => (s, None),
    };

    // Split mantissa into integer and fractional parts
    let (int_str, frac_str) = match mantissa_str.find('.') {
        Some(idx) => (&mantissa_str[..idx], &mantissa_str[idx + 1..]),
        None => (mantissa_str, ""),
    };

    // Parse mantissa as a high-precision float
    let mut value: f64 = 0.0;
    for ch in int_str.chars() {
        let digit = hex_char_to_u8(ch).ok_or("malformed number")?;
        value = value * 16.0 + digit as f64;
    }

    let mut frac_mult = 1.0 / 16.0;
    for ch in frac_str.chars() {
        let digit = hex_char_to_u8(ch).ok_or("malformed number")?;
        value += digit as f64 * frac_mult;
        frac_mult /= 16.0;
    }

    // Apply binary exponent
    if let Some(exp) = exp_str {
        let exp_val: i32 = exp.parse().map_err(|_| "malformed number".to_string())?;
        value = value * (2.0_f64).powi(exp_val);
    }

    Ok(value)
}

fn hex_char_to_u8(ch: char) -> Option<u8> {
    match ch {
        '0'..='9' => Some(ch as u8 - b'0'),
        'a'..='f' => Some(ch as u8 - b'a' + 10),
        'A'..='F' => Some(ch as u8 - b'A' + 10),
        _ => None,
    }
}

/// Parse hex integer digits with wrapping on overflow.
fn parse_hex_integer(hex_digits: &str) -> i64 {
    let mut val: u64 = 0;
    for ch in hex_digits.chars() {
        let digit = match ch {
            '0'..='9' => ch as u64 - '0' as u64,
            'a'..='f' => ch as u64 - 'a' as u64 + 10,
            'A'..='F' => ch as u64 - 'A' as u64 + 10,
            _ => 0,
        };
        val = val.wrapping_mul(16).wrapping_add(digit);
    }
    val as i64
}

// ─── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input.as_bytes(), "test").tokenize().unwrap()
    }

    fn lex_err(input: &str) -> LexError {
        Lexer::new(input.as_bytes(), "test").tokenize().unwrap_err()
    }

    fn kinds(tokens: &[Token]) -> Vec<&TokenKind> {
        tokens.iter().map(|t| &t.kind).collect()
    }

    // ── Keywords ──

    #[test]
    fn test_keywords() {
        let tokens = lex("and break do else elseif end false for function global goto if in local nil not or repeat return then true until while");
        let expected = vec![
            TokenKind::And, TokenKind::Break, TokenKind::Do, TokenKind::Else,
            TokenKind::ElseIf, TokenKind::End, TokenKind::False, TokenKind::For,
            TokenKind::Function, TokenKind::Global, TokenKind::Goto, TokenKind::If,
            TokenKind::In, TokenKind::Local, TokenKind::Nil, TokenKind::Not,
            TokenKind::Or, TokenKind::Repeat, TokenKind::Return, TokenKind::Then,
            TokenKind::True, TokenKind::Until, TokenKind::While, TokenKind::Eof,
        ];
        assert_eq!(kinds(&tokens), expected.iter().collect::<Vec<_>>());
    }

    // ── Names ──

    #[test]
    fn test_names() {
        let tokens = lex("foo _bar baz123 _VERSION");
        assert_eq!(tokens[0].kind, TokenKind::Name("foo".into()));
        assert_eq!(tokens[1].kind, TokenKind::Name("_bar".into()));
        assert_eq!(tokens[2].kind, TokenKind::Name("baz123".into()));
        assert_eq!(tokens[3].kind, TokenKind::Name("_VERSION".into()));
    }

    // ── Operators ──

    #[test]
    fn test_operators() {
        let tokens = lex("+ - * / % ^ # & ~ | << >> // == ~= <= >= < > =");
        let expected = vec![
            TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash,
            TokenKind::Percent, TokenKind::Caret, TokenKind::Hash, TokenKind::Ampersand,
            TokenKind::Tilde, TokenKind::Pipe, TokenKind::LtLt, TokenKind::GtGt,
            TokenKind::SlashSlash, TokenKind::EqEq, TokenKind::TildeEq, TokenKind::LtEq,
            TokenKind::GtEq, TokenKind::Lt, TokenKind::Gt, TokenKind::Eq,
            TokenKind::Eof,
        ];
        assert_eq!(kinds(&tokens), expected.iter().collect::<Vec<_>>());
    }

    // ── Delimiters ──

    #[test]
    fn test_delimiters() {
        let tokens = lex("( ) { } [ ] :: ; : , . .. ...");
        let expected = vec![
            TokenKind::LParen, TokenKind::RParen, TokenKind::LBrace, TokenKind::RBrace,
            TokenKind::LBracket, TokenKind::RBracket, TokenKind::ColonColon,
            TokenKind::Semicolon, TokenKind::Colon, TokenKind::Comma,
            TokenKind::Dot, TokenKind::DotDot, TokenKind::DotDotDot,
            TokenKind::Eof,
        ];
        assert_eq!(kinds(&tokens), expected.iter().collect::<Vec<_>>());
    }

    // ── Integer numerals ──

    #[test]
    fn test_decimal_integers() {
        let tokens = lex("3 345 0");
        assert_eq!(tokens[0].kind, TokenKind::Integer(3));
        assert_eq!(tokens[1].kind, TokenKind::Integer(345));
        assert_eq!(tokens[2].kind, TokenKind::Integer(0));
    }

    #[test]
    fn test_hex_integers() {
        let tokens = lex("0xff 0xBEBADA 0X10");
        assert_eq!(tokens[0].kind, TokenKind::Integer(0xff));
        assert_eq!(tokens[1].kind, TokenKind::Integer(0xBEBADA));
        assert_eq!(tokens[2].kind, TokenKind::Integer(0x10));
    }

    #[test]
    fn test_hex_integer_overflow_wraps() {
        let tokens = lex("0xFFFFFFFFFFFFFFFF");
        assert_eq!(tokens[0].kind, TokenKind::Integer(-1)); // wraps to -1
    }

    // ── Float numerals ──

    #[test]
    fn test_decimal_floats() {
        let tokens = lex("3.0 3.1416 314.16e-2 0.31416E1 34e1");
        assert_eq!(tokens[0].kind, TokenKind::Float(3.0));
        assert_eq!(tokens[1].kind, TokenKind::Float(3.1416));
        assert_eq!(tokens[2].kind, TokenKind::Float(314.16e-2));
        assert_eq!(tokens[3].kind, TokenKind::Float(0.31416e1));
        assert_eq!(tokens[4].kind, TokenKind::Float(34e1));
    }

    #[test]
    fn test_dot_leading_float() {
        let tokens = lex(".5 .123e2");
        assert_eq!(tokens[0].kind, TokenKind::Float(0.5));
        assert_eq!(tokens[1].kind, TokenKind::Float(0.123e2));
    }

    #[test]
    fn test_hex_floats() {
        let tokens = lex("0x0.1E 0xA23p-4 0X1.921FB54442D18P+1");
        // 0x0.1E = 0x1E / 0x100 = 30/256 = 0.1171875
        assert_eq!(tokens[0].kind, TokenKind::Float(30.0 / 256.0));
        // 0xA23p-4 = 2595 * 2^-4 = 162.1875
        assert_eq!(tokens[1].kind, TokenKind::Float(2595.0 / 16.0));
        // pi ≈ 3.141592653589793
        let pi_val = match &tokens[2].kind {
            TokenKind::Float(v) => *v,
            _ => panic!("expected float"),
        };
        assert!((pi_val - std::f64::consts::PI).abs() < 1e-15);
    }

    // ── Short strings ──

    #[test]
    fn test_simple_strings() {
        let tokens = lex(r#""hello" 'world'"#);
        assert_eq!(tokens[0].kind, TokenKind::String(b"hello".to_vec()));
        assert_eq!(tokens[1].kind, TokenKind::String(b"world".to_vec()));
    }

    #[test]
    fn test_string_escape_sequences() {
        let tokens = lex(r#""\a\b\f\n\r\t\v\\\"\'""#);
        assert_eq!(
            tokens[0].kind,
            TokenKind::String(vec![0x07, 0x08, 0x0C, b'\n', b'\r', b'\t', 0x0B, b'\\', b'"', b'\''])
        );
    }

    #[test]
    fn test_string_hex_escape() {
        let tokens = lex(r#""\x41\x42\x43""#);
        assert_eq!(tokens[0].kind, TokenKind::String(b"ABC".to_vec()));
    }

    #[test]
    fn test_string_decimal_escape() {
        let tokens = lex(r#""\97\10\04923""#);
        // \97 = 'a', \10 = '\n', \049 = '1', then literal "23"
        assert_eq!(tokens[0].kind, TokenKind::String(b"a\n123".to_vec()));
    }

    #[test]
    fn test_string_unicode_escape() {
        let tokens = lex(r#""\u{41}\u{1F600}""#);
        let mut expected = vec![0x41]; // 'A'
        // U+1F600 in UTF-8: F0 9F 98 80
        expected.extend_from_slice(&[0xF0, 0x9F, 0x98, 0x80]);
        assert_eq!(tokens[0].kind, TokenKind::String(expected));
    }

    #[test]
    fn test_string_z_escape() {
        let tokens = lex("\"abc\\z   \n   def\"");
        assert_eq!(tokens[0].kind, TokenKind::String(b"abcdef".to_vec()));
    }

    #[test]
    fn test_string_newline_escape() {
        let tokens = lex("\"line1\\\nline2\"");
        assert_eq!(tokens[0].kind, TokenKind::String(b"line1\nline2".to_vec()));
    }

    // ── Long strings ──

    #[test]
    fn test_long_string_level0() {
        let tokens = lex("[[hello]]");
        assert_eq!(tokens[0].kind, TokenKind::String(b"hello".to_vec()));
    }

    #[test]
    fn test_long_string_level1() {
        let tokens = lex("[=[hello]=]");
        assert_eq!(tokens[0].kind, TokenKind::String(b"hello".to_vec()));
    }

    #[test]
    fn test_long_string_multiline() {
        let tokens = lex("[[\nline1\nline2\n]]");
        assert_eq!(tokens[0].kind, TokenKind::String(b"line1\nline2\n".to_vec()));
    }

    #[test]
    fn test_long_string_first_newline_stripped() {
        // First newline after [[ is stripped
        let t1 = lex("[[\nhello]]");
        let t2 = lex("[[hello]]");
        assert_eq!(t1[0].kind, TokenKind::String(b"hello".to_vec()));
        assert_eq!(t2[0].kind, TokenKind::String(b"hello".to_vec()));
    }

    #[test]
    fn test_long_string_nested_brackets() {
        // [==[ opens at level 2; the first ]==] closes it
        let tokens = lex("[==[contains ]=] and ]==]");
        assert_eq!(tokens[0].kind, TokenKind::String(b"contains ]=] and ".to_vec()));
    }

    // ── Comments ──

    #[test]
    fn test_short_comment() {
        let tokens = lex("x -- this is a comment\ny");
        assert_eq!(tokens[0].kind, TokenKind::Name("x".into()));
        assert_eq!(tokens[1].kind, TokenKind::Name("y".into()));
        assert_eq!(tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn test_long_comment() {
        let tokens = lex("x --[[this is\na long comment]] y");
        assert_eq!(tokens[0].kind, TokenKind::Name("x".into()));
        assert_eq!(tokens[1].kind, TokenKind::Name("y".into()));
        assert_eq!(tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn test_long_comment_with_level() {
        let tokens = lex("x --[==[long\ncomment]==] y");
        assert_eq!(tokens[0].kind, TokenKind::Name("x".into()));
        assert_eq!(tokens[1].kind, TokenKind::Name("y".into()));
    }

    #[test]
    fn test_not_a_long_comment() {
        // --[ is not a long comment (no matching bracket)
        let tokens = lex("x --[not a long comment\ny");
        assert_eq!(tokens[0].kind, TokenKind::Name("x".into()));
        assert_eq!(tokens[1].kind, TokenKind::Name("y".into()));
    }

    // ── Source location ──

    #[test]
    fn test_source_location() {
        let tokens = lex("x = 1\ny = 2");
        assert_eq!(tokens[0].location, Location::new(1, 1)); // x
        assert_eq!(tokens[1].location, Location::new(1, 3)); // =
        assert_eq!(tokens[2].location, Location::new(1, 5)); // 1
        assert_eq!(tokens[3].location, Location::new(2, 1)); // y
        assert_eq!(tokens[4].location, Location::new(2, 3)); // =
        assert_eq!(tokens[5].location, Location::new(2, 5)); // 2
    }

    // ── Error cases ──

    #[test]
    fn test_unfinished_string() {
        let err = lex_err("\"hello");
        assert!(err.message.contains("unfinished string"));
    }

    #[test]
    fn test_unfinished_long_string() {
        let err = lex_err("[[hello");
        assert!(err.message.contains("unfinished long string"));
    }

    #[test]
    fn test_invalid_escape() {
        let err = lex_err(r#""\q""#);
        assert!(err.message.contains("invalid escape"));
    }

    #[test]
    fn test_decimal_escape_too_large() {
        let err = lex_err(r#""\256""#);
        assert!(err.message.contains("too large"));
    }

    // ── Complex expressions ──

    #[test]
    fn test_complex_expression() {
        let tokens = lex("local x = 1 + 2.5 * (y - 3)");
        assert_eq!(tokens[0].kind, TokenKind::Local);
        assert_eq!(tokens[1].kind, TokenKind::Name("x".into()));
        assert_eq!(tokens[2].kind, TokenKind::Eq);
        assert_eq!(tokens[3].kind, TokenKind::Integer(1));
        assert_eq!(tokens[4].kind, TokenKind::Plus);
        assert_eq!(tokens[5].kind, TokenKind::Float(2.5));
        assert_eq!(tokens[6].kind, TokenKind::Star);
        assert_eq!(tokens[7].kind, TokenKind::LParen);
        assert_eq!(tokens[8].kind, TokenKind::Name("y".into()));
        assert_eq!(tokens[9].kind, TokenKind::Minus);
        assert_eq!(tokens[10].kind, TokenKind::Integer(3));
        assert_eq!(tokens[11].kind, TokenKind::RParen);
        assert_eq!(tokens[12].kind, TokenKind::Eof);
    }

    #[test]
    fn test_function_definition() {
        let tokens = lex("function foo(a, b) return a + b end");
        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[1].kind, TokenKind::Name("foo".into()));
        assert_eq!(tokens[2].kind, TokenKind::LParen);
        assert_eq!(tokens[3].kind, TokenKind::Name("a".into()));
        assert_eq!(tokens[4].kind, TokenKind::Comma);
        assert_eq!(tokens[5].kind, TokenKind::Name("b".into()));
        assert_eq!(tokens[6].kind, TokenKind::RParen);
        assert_eq!(tokens[7].kind, TokenKind::Return);
    }

    #[test]
    fn test_table_constructor() {
        let tokens = lex("{1, 2, [3] = 4, x = 5}");
        assert_eq!(tokens[0].kind, TokenKind::LBrace);
        assert_eq!(tokens[1].kind, TokenKind::Integer(1));
        assert_eq!(tokens[2].kind, TokenKind::Comma);
        assert_eq!(tokens[3].kind, TokenKind::Integer(2));
    }

    #[test]
    fn test_method_call() {
        let tokens = lex("obj:method(x)");
        assert_eq!(tokens[0].kind, TokenKind::Name("obj".into()));
        assert_eq!(tokens[1].kind, TokenKind::Colon);
        assert_eq!(tokens[2].kind, TokenKind::Name("method".into()));
        assert_eq!(tokens[3].kind, TokenKind::LParen);
    }

    #[test]
    fn test_label() {
        let tokens = lex("::label::");
        assert_eq!(tokens[0].kind, TokenKind::ColonColon);
        assert_eq!(tokens[1].kind, TokenKind::Name("label".into()));
        assert_eq!(tokens[2].kind, TokenKind::ColonColon);
    }

    #[test]
    fn test_vararg() {
        let tokens = lex("function f(...) return ... end");
        assert_eq!(tokens[3].kind, TokenKind::DotDotDot);
        assert_eq!(tokens[6].kind, TokenKind::DotDotDot);
    }

    #[test]
    fn test_empty_source() {
        let tokens = lex("");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_only_whitespace() {
        let tokens = lex("  \t\n  \r\n  ");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_only_comments() {
        let tokens = lex("-- comment1\n-- comment2\n");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_five_equivalent_strings() {
        // From the Lua reference: five literal strings that denote the same string
        let t1 = lex(r#"'alo\n123"'"#);
        let t2 = lex(r#""alo\n123\"""#);
        let t3 = lex(r#"'\97lo\10\04923"'"#);
        let t4 = lex("[[alo\n123\"]]");
        let t5 = lex("[==[\nalo\n123\"]==]");

        let expected = b"alo\n123\"".to_vec();
        assert_eq!(t1[0].kind, TokenKind::String(expected.clone()));
        assert_eq!(t2[0].kind, TokenKind::String(expected.clone()));
        assert_eq!(t3[0].kind, TokenKind::String(expected.clone()));
        assert_eq!(t4[0].kind, TokenKind::String(expected.clone()));
        assert_eq!(t5[0].kind, TokenKind::String(expected));
    }

    #[test]
    fn test_concat_vs_dot_numeral() {
        // "1..2" should be Integer(1), DotDot, Integer(2)
        let tokens = lex("1..2");
        assert_eq!(tokens[0].kind, TokenKind::Integer(1));
        assert_eq!(tokens[1].kind, TokenKind::DotDot);
        assert_eq!(tokens[2].kind, TokenKind::Integer(2));
    }

    #[test]
    fn test_concat_vs_float() {
        // "1 .5" -> Integer(1), Float(0.5)
        let tokens = lex("1 .5");
        assert_eq!(tokens[0].kind, TokenKind::Integer(1));
        assert_eq!(tokens[1].kind, TokenKind::Float(0.5));
    }

    #[test]
    fn test_decimal_overflow_becomes_float() {
        // A very large decimal integer that overflows i64
        let tokens = lex("99999999999999999999999999999");
        assert!(matches!(tokens[0].kind, TokenKind::Float(_)));
    }

    #[test]
    fn test_shebang_line() {
        // While not in the spec, many Lua files have #!/usr/bin/lua
        // The standard Lua lexer handles this by skipping the first line if it starts with #
        // Our lexer treats # as the Hash token, so #!/usr/bin/lua would need pre-processing
        // or we handle it specially. For now, # is just Hash.
        let tokens = lex("# 42");
        assert_eq!(tokens[0].kind, TokenKind::Hash);
        assert_eq!(tokens[1].kind, TokenKind::Integer(42));
    }
}
