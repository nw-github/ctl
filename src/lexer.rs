use std::borrow::Cow;

use enum_as_inner::EnumAsInner;

use crate::{error::FileId, THIS_PARAM, THIS_TYPE};

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

#[derive(Debug, Clone, Copy)]
pub enum Error {
    UnterminatedStr,
    UnterminatedChar,
    CharTooLong,
    BadByteChar,
    BadByteStr,
    EmptyChar,
    UnterminatedComment,
    UnrecognizedChar,
    InvalidEscape,
}

impl Error {
    pub fn tell(&self) -> &'static str {
        match self {
            Error::UnterminatedStr => "unterminated string literal",
            Error::UnterminatedChar => "unterminated char literal",
            Error::EmptyChar => "empty char literal",
            Error::CharTooLong => "char literal must only contain one character",
            Error::BadByteStr => "invalid character in byte string",
            Error::BadByteChar => "invalid character in byte literal (must be ascii)",
            Error::UnterminatedComment => "unterminated block comment",
            Error::UnrecognizedChar => "unexpected character",
            Error::InvalidEscape => "invalid UTF-8 escape sequence",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Location {
    pub row: usize,
    pub col: usize,
    pub pos: usize,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub loc: Location,
    pub len: usize,
    pub file: FileId,
}

impl Span {
    pub fn extend_to(&mut self, b: impl Into<Span>) {
        *self = self.extended_to(b);
    }

    pub fn extended_to(&self, b: impl Into<Span>) -> Self {
        let b: Span = b.into();
        assert!(b.loc.pos >= self.loc.pos);
        Self {
            loc: self.loc,
            len: b.loc.pos - self.loc.pos + b.len,
            file: self.file,
        }
    }

    pub fn text<'a>(&self, text: &'a str) -> &'a str {
        &text[self.loc.pos..][..self.len]
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

pub struct Lexer<'a> {
    src: &'a str,
    loc: Location,
    file: FileId,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file: FileId) -> Self {
        Self {
            src,
            loc: Location {
                row: 1,
                col: 1,
                pos: 0,
            },
            file,
        }
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        self.src[self.loc.pos..].chars().next()
    }

    #[inline]
    fn peek_next(&self) -> Option<char> {
        self.src[self.loc.pos..].chars().nth(1)
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.peek() {
            self.loc.pos += ch.len_utf8();
            if ch == '\n' {
                self.loc.col = 1;
                self.loc.row += 1;
            } else {
                self.loc.col += 1;
            }

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
        let start = self.loc.pos;
        while self.peek().map_or(false, &mut f) {
            self.advance();
        }
        &self.src[start..self.loc.pos]
    }

    fn advance_match(&mut self, s: &str) -> bool {
        let source = &self.src[self.loc.pos..];
        if s.len() <= source.len() && s == &source[..s.len()] {
            for _ in 0..s.len() {
                self.advance();
            }

            true
        } else {
            false
        }
    }

    fn escape_char(&mut self) -> Result<char, Located<Error>> {
        Ok(match self.advance() {
            Some('"') => '"',
            Some('\'') => '\'',
            Some('\\') => '\\',
            Some('n') => '\n',
            Some('t') => '\t',
            _ => {
                return Err(Located::new(
                    Span {
                        loc: self.loc,
                        len: 1,
                        file: self.file,
                    },
                    Error::InvalidEscape,
                ));
            }
        })
    }

    fn string_literal(&mut self, start: usize) -> Result<Token<'a>, Located<Error>> {
        let mut result = Cow::from("");
        loop {
            match self.advance() {
                Some('\\') => {
                    result.to_mut().push(self.escape_char()?);
                }
                Some('"') => break Ok(Token::String(result)),
                Some(ch) => match &mut result {
                    Cow::Borrowed(str) => *str = &self.src[start + 1..self.loc.pos],
                    Cow::Owned(str) => str.push(ch),
                },
                None => {
                    return Err(Located::new(
                        Span {
                            loc: self.loc,
                            len: 0,
                            file: self.file,
                        },
                        Error::UnterminatedStr,
                    ));
                }
            }
        }
    }

    fn char_literal(&mut self) -> Result<Token<'a>, Located<Error>> {
        let ch = match self.advance() {
            Some('\\') => self.escape_char()?,
            Some('\'') => {
                return Err(Located::new(
                    Span {
                        loc: self.loc,
                        len: 0,
                        file: self.file,
                    },
                    Error::EmptyChar,
                ))
            }
            Some(any) => any,
            None => {
                return Err(Located::new(
                    Span {
                        loc: self.loc,
                        len: 0,
                        file: self.file,
                    },
                    Error::UnterminatedChar,
                ))
            }
        };

        if !self.advance_if('\'') {
            self.advance_while(|c| c != '\'');
            self.advance();
            Err(Located::new(
                Span {
                    loc: self.loc,
                    len: 0,
                    file: self.file,
                },
                Error::CharTooLong,
            ))
        } else {
            Ok(Token::Char(ch))
        }
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
        if self.peek() == Some('.') && self.peek_next().map_or(false, |f| f.is_ascii_digit()) {
            self.advance();
            self.advance_while(|ch| ch.is_ascii_digit());
            Token::Float(&self.src[start..self.loc.pos])
        } else {
            let src = &self.src[start..self.loc.pos];
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
        match &self.src[start..self.loc.pos] {
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

    fn maybe_byte_string(&mut self, start: usize) -> Result<Token<'a>, Located<Error>> {
        match self.peek() {
            Some('\'') => {
                self.advance();
                let Token::Char(c) = self.char_literal()? else {
                    unreachable!()
                };
                match c.try_into() {
                    Ok(c) => Ok(Token::ByteChar(c)),
                    Err(_) => Err(Located::new(
                        Span {
                            loc: self.loc,
                            len: 0,
                            file: self.file,
                        },
                        Error::BadByteChar,
                    )),
                }
            }
            Some('"') => {
                self.advance();
                let Token::String(s) = self.string_literal(start)? else {
                    unreachable!()
                };
                if s.chars().any(|c| !c.is_ascii()) {
                    Err(Located::new(
                        Span {
                            loc: self.loc,
                            len: 0,
                            file: self.file,
                        },
                        Error::BadByteStr,
                    ))
                } else {
                    Ok(Token::ByteString(s))
                }
            }
            _ => Ok(self.identifier(start)),
        }
    }

    pub fn is_identifier_char(ch: char) -> bool {
        matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
    }

    pub fn is_identifier_first_char(ch: char) -> bool {
        matches!(ch, '_' | 'a'..='z' | 'A'..='Z')
    }

    fn next_internal(&mut self) -> Option<<Self as Iterator>::Item> {
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
                    } else if self.loc.pos >= self.src.len() {
                        return Some(Err(Located::new(
                            Span {
                                loc: self.loc,
                                len: 0,
                                file: self.file,
                            },
                            Error::UnterminatedComment,
                        )));
                    } else {
                        self.advance();
                    }
                }
            } else {
                break;
            }
        }

        let start = self.loc;
        let token = match self.advance()? {
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '[' => Token::LBrace,
            ']' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '#' => Token::Hash,
            '@' => Token::At,
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
            '"' => match self.string_literal(start.pos) {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            },
            '\'' => match self.char_literal() {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            },
            '0'..='9' => self.numeric_literal(start.pos),
            'b' => match self.maybe_byte_string(start.pos) {
                Ok(token) => token,
                Err(err) => return Some(Err(err)),
            },
            ch if Self::is_identifier_first_char(ch) => self.identifier(start.pos),
            _ => {
                return Some(Err(Located::new(
                    Span {
                        loc: self.loc,
                        len: 0,
                        file: self.file,
                    },
                    Error::UnrecognizedChar,
                )))
            }
        };

        Some(Ok(Located::new(
            Span {
                loc: start,
                len: self.loc.pos - start.pos,
                file: self.file,
            },
            token,
        )))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Located<Token<'a>>, Located<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_internal().unwrap_or(Ok(Located::new(
            Span {
                loc: self.loc,
                len: 0,
                file: self.file,
            },
            Token::Eof,
        ))))
    }
}
