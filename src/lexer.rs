use std::borrow::Cow;

use enum_as_inner::EnumAsInner;

use crate::{
    error::{Diagnostics, Error, FileId},
    THIS_PARAM, THIS_TYPE,
};

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Token<'a> {
    LCurly,
    RCurly,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LAngle,
    RAngle,

    Dot,
    Range,
    RangeInclusive,
    Ellipses,
    ScopeRes,
    Arrow,
    FatArrow,
    Comma,
    Colon,
    Semicolon,
    Hash,
    HashLCurly,
    At,

    Plus,
    AddAssign,
    Increment,
    Minus,
    SubAssign,
    Decrement,
    Asterisk,
    MulAssign,
    Div,
    DivAssign,
    Rem,
    RemAssign,
    Ampersand,
    AndAssign,
    LogicalAnd,
    Or,
    OrAssign,
    LogicalOr,
    Caret,
    XorAssign,
    Question,
    NoneCoalesce,
    NoneCoalesceAssign,
    Exclamation,
    NotEqual,
    GtEqual,
    Shr,
    ShrAssign,
    LtEqual,
    Shl,
    ShlAssign,
    Assign,
    Equal,

    Let,
    Mut,
    Fn,
    Keyword,
    Pub,
    Struct,
    Union,
    Enum,
    Trait,
    Dyn,
    Type,
    This,
    ThisType,
    Impl,
    If,
    Else,
    Loop,
    For,
    In,
    Is,
    As,
    While,
    Match,
    Extension,
    Extern,
    Mod,
    Async,
    Await,
    Break,
    Continue,
    Yield,
    Raise,
    Return,
    True,
    False,
    Static,
    Void,
    None,
    Super,
    Unsafe,
    Shared,
    Use,
    Try,
    Catch,

    Ident(&'a str),
    Int {
        base: u8,
        value: &'a str,
        width: Option<&'a str>,
    },
    Float(&'a str),
    String(Cow<'a, str>),
    ByteString(Cow<'a, str>),
    Char(char),
    ByteChar(u8),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Min,
    Assignment,   // =
    Range,        // .. ..=
    LogicalOr,    // ||
    LogicalAnd,   // &&
    Eq,           // != ==
    Comparison,   // < > <= >= is
    NoneCoalesce, // ??
    Or,           // |
    And,          // &
    Xor,          // ^
    Shift,        // << >>
    Term,         // + -
    Factor,       // * / %
    Cast,         // as as!
    Prefix,       // !x ++x --x +x -x
    Postfix,      // x++ x-- ! ?
    Call,         // x() x[] x.
}

impl Token<'_> {
    pub fn precedence(&self) -> Precedence {
        use Token::*;

        match self {
            LBrace | LParen | Dot => Precedence::Call,
            Range | RangeInclusive => Precedence::Range,
            Plus | Minus => Precedence::Term,
            Asterisk | Div | Rem => Precedence::Factor,
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign | AndAssign
            | OrAssign | XorAssign | NoneCoalesceAssign | ShrAssign | ShlAssign => {
                Precedence::Assignment
            }
            Increment | Decrement | Exclamation | Question => Precedence::Postfix,
            Ampersand => Precedence::And,
            Or => Precedence::Or,
            Caret => Precedence::Xor,
            Shr | Shl => Precedence::Shift,
            LogicalAnd => Precedence::LogicalAnd,
            LogicalOr => Precedence::LogicalOr,
            Equal | NotEqual => Precedence::Eq,
            LAngle | RAngle | GtEqual | LtEqual | Is => Precedence::Comparison,
            NoneCoalesce => Precedence::NoneCoalesce,
            As => Precedence::Cast,
            _ => Precedence::Min,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub pos: usize,
    pub len: usize,
    pub file: FileId,
}

impl Span {
    pub fn extend_to(&mut self, b: impl Into<Span>) {
        *self = self.extended_to(b);
    }

    pub fn extended_to(self, b: impl Into<Span>) -> Self {
        let b: Span = b.into();
        debug_assert!(b.pos >= self.pos);
        Self {
            len: b.pos - self.pos + b.len,
            ..self
        }
    }
}

#[derive(Clone, derive_more::Constructor)]
pub struct Located<T> {
    pub span: Span,
    pub data: T,
}

impl<T> Located<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located::new(self.span, f(self.data))
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    file: FileId,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file: FileId) -> Self {
        Self { src, file, pos: 0 }
    }

    pub fn token(&mut self, diag: &mut Diagnostics) -> Located<Token<'a>> {
        self.next(diag)
            .unwrap_or_else(|| Located::new(self.here(0), Token::Eof))
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    #[inline]
    fn peek_next(&self) -> Option<char> {
        self.src[self.pos..].chars().nth(1)
    }

    fn here(&self, len: usize) -> Span {
        Span {
            pos: self.pos,
            file: self.file,
            len,
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.peek() {
            self.pos += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn advance_if(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance_while(&mut self, mut f: impl FnMut(char) -> bool) -> &'a str {
        let start = self.pos;
        while self.peek().is_some_and(&mut f) {
            self.advance();
        }
        &self.src[start..self.pos]
    }

    fn advance_match(&mut self, s: &str) -> bool {
        let source = &self.src[self.pos..];
        if s.len() <= source.len() && s == &source[..s.len()] {
            for _ in 0..s.len() {
                self.advance();
            }

            true
        } else {
            false
        }
    }

    fn expect(&mut self, diag: &mut Diagnostics, c: char) {
        if !matches!(self.peek(), Some(ch) if ch == c) {
            diag.error(Error::new(format!("expected '{c}'"), self.here(1)));
        } else {
            self.advance();
        }
    }

    fn escape_char(&mut self, diag: &mut Diagnostics) -> char {
        match self.advance() {
            Some('"') => '"',
            Some('\'') => '\'',
            Some('\\') => '\\',
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('0') => '\0',
            Some('u') => {
                self.expect(diag, '{');
                let chars = self.advance_while(|ch| ch.is_ascii_hexdigit());
                if chars.is_empty() {
                    diag.error(Error::new("expected hexadecimal digits", self.here(1)));
                    '\0'
                } else if let Some(ch) =
                    u32::from_str_radix(chars, 16).ok().and_then(char::from_u32)
                {
                    self.expect(diag, '}');
                    ch
                } else {
                    diag.error(Error::new(
                        "invalid char escape",
                        Span {
                            pos: self.pos - chars.len(),
                            file: self.file,
                            len: chars.len(),
                        },
                    ));
                    '\0'
                }
            }
            _ => {
                diag.error(Error::new("invalid escape sequence", self.here(1)));
                '\0'
            }
        }
    }

    fn string_literal(&mut self, diag: &mut Diagnostics, start: usize) -> Token<'a> {
        let mut result = Cow::from("");
        loop {
            match self.advance() {
                Some('\\') => {
                    result.to_mut().push(self.escape_char(diag));
                }
                Some('"') => break Token::String(result),
                Some(ch) => match &mut result {
                    Cow::Borrowed(str) => *str = &self.src[start + 1..self.pos],
                    Cow::Owned(str) => str.push(ch),
                },
                None => {
                    diag.error(Error::new("unterminated string literal", self.here(1)));
                    break Token::String(result);
                }
            }
        }
    }

    fn char_literal(&mut self, diag: &mut Diagnostics) -> Token<'a> {
        let ch = match self.advance() {
            Some('\\') => self.escape_char(diag),
            Some('\'') => {
                diag.error(Error::new("empty char literal", self.here(1)));
                '\0'
            }
            Some(any) => any,
            None => {
                diag.error(Error::new("unterminated char literal", self.here(1)));
                '\0'
            }
        };

        if !self.advance_if('\'') {
            let ch = self.advance_while(|c| c != '\'');
            self.advance();
            diag.error(Error::new(
                "char literal must only contain one character",
                Span {
                    pos: self.pos - ch.len() - 1,
                    len: ch.len(),
                    file: self.file,
                },
            ));
        }
        Token::Char(ch)
    }

    fn numeric_suffix(&mut self) -> Option<&'a str> {
        let suffix = self.advance_while(|s| s.is_ascii_alphanumeric());
        if !suffix.is_empty() {
            Some(suffix)
        } else {
            None
        }
    }

    fn numeric_literal(&mut self, start: usize) -> Token<'a> {
        if self.src[start..].starts_with('0') {
            if self.advance_if('x') {
                return Token::Int {
                    base: 16,
                    value: self.advance_while(|ch| ch.is_ascii_hexdigit()),
                    width: self.numeric_suffix(),
                };
            } else if self.advance_if('o') {
                return Token::Int {
                    base: 8,
                    value: self.advance_while(|ch| ch.is_digit(8)),
                    width: self.numeric_suffix(),
                };
            } else if self.advance_if('b') {
                return Token::Int {
                    base: 2,
                    value: self.advance_while(|ch| ch.is_digit(2)),
                    width: self.numeric_suffix(),
                };
            }
        }

        self.advance_while(|ch| ch.is_ascii_digit());
        if self.peek() == Some('.') && self.peek_next().is_some_and(|f| f.is_ascii_digit()) {
            self.advance();
            self.advance_while(|ch| ch.is_ascii_digit());
            Token::Float(&self.src[start..self.pos])
        } else {
            let src = &self.src[start..self.pos];
            if src.len() == 1 {
                // TODO: warn about leading zero, dont make it an error
            }

            Token::Int {
                base: 10,
                value: src,
                width: self.numeric_suffix(),
            }
        }
    }

    fn identifier(&mut self, start: usize) -> Token<'a> {
        self.advance_while(Self::is_identifier_char);
        match &self.src[start..self.pos] {
            "as" => Token::As,
            "async" => Token::Async,
            "await" => Token::Await,
            "break" => Token::Break,
            "catch" => Token::Catch,
            "continue" => Token::Continue,
            "dyn" => Token::Dyn,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "extension" => Token::Extension,
            "extern" => Token::Extern,
            "false" => Token::False,
            "fn" => Token::Fn,
            "for" => Token::For,
            "if" => Token::If,
            "impl" => Token::Impl,
            "in" => Token::In,
            "is" => Token::Is,
            "kw" => Token::Keyword,
            "let" => Token::Let,
            "loop" => Token::Loop,
            "match" => Token::Match,
            "mod" => Token::Mod,
            "mut" => Token::Mut,
            "null" => Token::None,
            "pub" => Token::Pub,
            "raise" => Token::Raise,
            "return" => Token::Return,
            "shared" => Token::Shared,
            "static" => Token::Static,
            "struct" => Token::Struct,
            "super" => Token::Super,
            "union" => Token::Union,
            "unsafe" => Token::Unsafe,
            "use" => Token::Use,
            "trait" => Token::Trait,
            "true" => Token::True,
            "try" => Token::Try,
            "type" => Token::Type,
            "void" => Token::Void,
            "while" => Token::While,
            "yield" => Token::Yield,
            x if x == THIS_PARAM => Token::This,
            x if x == THIS_TYPE => Token::ThisType,
            id => Token::Ident(id),
        }
    }

    fn maybe_byte_string(&mut self, diag: &mut Diagnostics, start: usize) -> Token<'a> {
        match self.peek() {
            Some('\'') => {
                self.advance();
                let Token::Char(c) = self.char_literal(diag) else {
                    unreachable!()
                };
                match c.try_into() {
                    Ok(c) => Token::ByteChar(c),
                    Err(_) => {
                        diag.error(Error::new(
                            "invalid character in ascii string literal",
                            self.here(1),
                        ));
                        Token::ByteChar(0)
                    }
                }
            }
            Some('"') => {
                self.advance();
                let Token::String(s) = self.string_literal(diag, start) else {
                    unreachable!()
                };
                if s.chars().any(|c| !c.is_ascii()) {
                    diag.error(Error::new(
                        "invalid character in byte string",
                        Span {
                            pos: start,
                            len: self.pos - start,
                            file: self.file,
                        },
                    ));
                }
                Token::ByteString(s)
            }
            _ => self.identifier(start),
        }
    }

    pub fn is_identifier_char(ch: char) -> bool {
        matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
    }

    pub fn is_identifier_first_char(ch: char) -> bool {
        matches!(ch, '_' | 'a'..='z' | 'A'..='Z')
    }

    fn next(&mut self, diag: &mut Diagnostics) -> Option<Located<Token<'a>>> {
        loop {
            self.advance_while(char::is_whitespace);
            if self.advance_match("//") {
                self.advance_while(|ch| ch != '\n');
            } else if self.advance_match("/*") {
                let mut count = 1;
                while count > 0 {
                    if self.advance_match("*/") {
                        count -= 1;
                    } else if self.advance_match("/*") {
                        count += 1;
                    } else if self.pos >= self.src.len() {
                        diag.error(Error::new("unterminated block comment", self.here(0)));
                        return None;
                    } else {
                        self.advance();
                    }
                }
            } else {
                break;
            }
        }

        let start = self.pos;
        let token = match self.advance()? {
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '[' => Token::LBrace,
            ']' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '@' => Token::At,
            '#' => {
                if self.advance_if('{') {
                    Token::HashLCurly
                } else {
                    Token::Hash
                }
            }
            '.' => {
                if self.advance_if('.') {
                    if self.advance_if('.') {
                        Token::Ellipses
                    } else if self.advance_if('=') {
                        Token::RangeInclusive
                    } else {
                        Token::Range
                    }
                } else {
                    Token::Dot
                }
            }
            ':' => {
                if self.advance_if(':') {
                    Token::ScopeRes
                } else {
                    Token::Colon
                }
            }
            '+' => {
                if self.advance_if('=') {
                    Token::AddAssign
                } else if self.advance_if('+') {
                    Token::Increment
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.advance_if('=') {
                    Token::SubAssign
                } else if self.advance_if('-') {
                    Token::Decrement
                } else if self.advance_if('>') {
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '*' => {
                if self.advance_if('=') {
                    Token::MulAssign
                } else {
                    Token::Asterisk
                }
            }
            '/' => {
                if self.advance_if('=') {
                    Token::DivAssign
                } else {
                    Token::Div
                }
            }
            '%' => {
                if self.advance_if('=') {
                    Token::RemAssign
                } else {
                    Token::Rem
                }
            }
            '&' => {
                if self.advance_if('=') {
                    Token::AndAssign
                } else if self.advance_if('&') {
                    Token::LogicalAnd
                } else {
                    Token::Ampersand
                }
            }
            '|' => {
                if self.advance_if('=') {
                    Token::OrAssign
                } else if self.advance_if('|') {
                    Token::LogicalOr
                } else {
                    Token::Or
                }
            }
            '^' => {
                if self.advance_if('=') {
                    Token::XorAssign
                } else {
                    Token::Caret
                }
            }
            '?' => {
                if self.advance_if('?') {
                    if self.advance_if('=') {
                        Token::NoneCoalesceAssign
                    } else {
                        Token::NoneCoalesce
                    }
                } else {
                    Token::Question
                }
            }
            '!' => {
                if self.advance_if('=') {
                    Token::NotEqual
                } else {
                    Token::Exclamation
                }
            }
            '>' => {
                if self.advance_if('=') {
                    Token::GtEqual
                } else if self.advance_if('>') {
                    if self.advance_if('=') {
                        Token::ShrAssign
                    } else {
                        Token::Shr
                    }
                } else {
                    Token::RAngle
                }
            }
            '<' => {
                if self.advance_if('=') {
                    Token::LtEqual
                } else if self.advance_if('<') {
                    if self.advance_if('=') {
                        Token::ShlAssign
                    } else {
                        Token::Shl
                    }
                } else {
                    Token::LAngle
                }
            }
            '=' => {
                if self.advance_if('=') {
                    Token::Equal
                } else if self.advance_if('>') {
                    Token::FatArrow
                } else {
                    Token::Assign
                }
            }
            '"' => self.string_literal(diag, start),
            '\'' => self.char_literal(diag),
            '0'..='9' => self.numeric_literal(start),
            'b' => self.maybe_byte_string(diag, start),
            ch if Self::is_identifier_first_char(ch) => self.identifier(start),
            _ => {
                diag.error(Error::new("unexpected character", self.here(0)));
                return self.next(diag);
            }
        };

        Some(Located::new(
            Span {
                pos: start,
                len: self.pos - start,
                file: self.file,
            },
            token,
        ))
    }
}
