use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    FatArrow,
    Comma,
    Colon,
    Semicolon,

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
    ErrCoalesce,
    GtEqual,
    LtEqual,
    Assign,
    Equal,

    Let,
    Mut,
    Fn,
    Keyword,
    Pub,
    Struct,
    Enum,
    Interface,
    Dyn,
    Type,
    This,
    If,
    Loop,
    For,
    While,
    Match,
    Sizeof,
    Extern,
    Mod,
    Async,
    Await,
    Break,
    Continue,
    Yield,
    True,
    False,
    Static,

    Ident(&'a str),
    Int(u8, &'a str),
    Float(&'a str),
    String(Cow<'a, str>),
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    UnterminatedStr,
    UnterminatedComment,
    UnrecognizedChar,
    InvalidEscape,
    LeadingZero,
}

impl Error {
    pub fn tell(&self) -> &'static str {
        match self {
            Error::UnterminatedStr => "unterminated string literal",
            Error::UnterminatedComment => "unterminated block comment",
            Error::UnrecognizedChar => "unexpected character",
            Error::InvalidEscape => "invalid UTF-8 escape sequence",
            Error::LeadingZero => "invalid integer literal (leading zero without x, o, or b)",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub row: usize,
    pub col: usize,
    pub pos: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub loc: Location,
    pub len: usize,
}

impl Span {
    pub fn combine(a: impl Into<Span>, b: impl Into<Span>) -> Self {
        a.into().extended_to(b)
    }

    pub fn extend_to(&mut self, b: impl Into<Span>) {
        *self = self.extended_to(b);
    }

    pub fn extended_to(&mut self, b: impl Into<Span>) -> Self {
        let b: Span = b.into();
        assert!(b.loc.pos >= self.loc.pos);
        Self {
            loc: self.loc,
            len: b.loc.pos - self.loc.pos + b.len,
        }
    }

    pub fn text<'a>(&self, text: &'a str) -> &'a str {
        &text[self.loc.pos..][..self.len]
    }
}

#[derive(Debug, Clone, derive_more::Constructor)]
pub struct Located<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Located<T> {
    pub fn map<U>(&self, u: U) -> Located<U> {
        Located { data: u, span: self.span }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    loc: Location,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            loc: Location {
                row: 1,
                col: 1,
                pos: 0,
            },
        }
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.loc.pos)
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.peek() {
            self.loc.pos += 1;
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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Located<Token<'a>>, Located<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
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
                            Error::UnterminatedComment,
                            Span {
                                loc: self.loc,
                                len: 0,
                            },
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
                } else if self.advance_if('!') {
                    Token::ErrCoalesce
                } else {
                    Token::Exclamation
                }
            }
            '>' => {
                if self.advance_if('=') {
                    Token::GtEqual
                } else {
                    Token::RAngle
                }
            }
            '<' => {
                if self.advance_if('=') {
                    Token::LtEqual
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
            '"' => {
                let mut result = Cow::from("");
                loop {
                    match self.advance() {
                        Some('\\') => {
                            result.to_mut().push(match self.advance() {
                                Some('"') => '"',
                                Some('\\') => '\\',
                                Some('n') => '\n',
                                Some('t') => '\t',
                                _ => {
                                    return Some(Err(Located::new(
                                        Error::InvalidEscape,
                                        Span {
                                            loc: self.loc,
                                            len: 1,
                                        },
                                    )))
                                }
                            });
                        }
                        Some('"') => break Token::String(result),
                        Some(ch) => match &mut result {
                            Cow::Borrowed(str) => *str = &self.src[start.pos + 1..self.loc.pos],
                            Cow::Owned(str) => str.push(ch),
                        },
                        None => {
                            return Some(Err(Located::new(
                                Error::UnterminatedStr,
                                Span {
                                    loc: self.loc,
                                    len: 0,
                                },
                            )))
                        }
                    }
                }
            }
            c @ '0'..='9' => {
                if c == '0' {
                    if self.advance_if('x') {
                        Token::Int(16, self.advance_while(|ch| ch.is_ascii_hexdigit()))
                    } else if self.advance_if('o') {
                        Token::Int(8, self.advance_while(|ch| ch.is_digit(8)))
                    } else if self.advance_if('b') {
                        Token::Int(2, self.advance_while(|ch| ch.is_digit(2)))
                    } else {
                        return Some(Err(Located::new(
                            Error::LeadingZero,
                            Span {
                                loc: self.loc,
                                len: 0,
                            },
                        )));
                    }
                } else {
                    self.advance_while(|ch| ch.is_ascii_digit());
                    if self.advance_if('.') {
                        self.advance_while(|ch| ch.is_ascii_digit());
                        Token::Float(self.src[start.pos..self.loc.pos].into())
                    } else {
                        Token::Int(10, self.src[start.pos..self.loc.pos].into())
                    }
                }
            }
            '_' | 'a'..='z' | 'A'..='Z' => {
                self.advance_while(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'));
                match &self.src[start.pos..self.loc.pos] {
                    "let" => Token::Let,
                    "mut" => Token::Mut,
                    "fn" => Token::Fn,
                    "keyword" => Token::Keyword,
                    "pub" => Token::Pub,
                    "struct" => Token::Struct,
                    "enum" => Token::Enum,
                    "interface" => Token::Interface,
                    "dyn" => Token::Dyn,
                    "type" => Token::Type,
                    "this" => Token::This,
                    "if" => Token::If,
                    "loop" => Token::Loop,
                    "for" => Token::For,
                    "while" => Token::While,
                    "match" => Token::Match,
                    "sizeof" => Token::Sizeof,
                    "extern" => Token::Extern,
                    "mod" => Token::Mod,
                    "async" => Token::Async,
                    "await" => Token::Await,
                    "break" => Token::Break,
                    "continue" => Token::Continue,
                    "yield" => Token::Yield,
                    "true" => Token::True,
                    "false" => Token::False,
                    "static" => Token::Static,
                    id => Token::Ident(id),
                }
            }
            _ => {
                return Some(Err(Located::new(
                    Error::UnrecognizedChar,
                    Span {
                        loc: self.loc,
                        len: 0,
                    },
                )))
            }
        };

        Some(Ok(Located::new(
            token,
            Span {
                loc: start,
                len: self.loc.pos - start.pos,
            },
        )))
    }
}
