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
    Interface,
    Dyn,
    Type,
    This,
    If,
    Else,
    Loop,
    For,
    In,
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
    Return,
    True,
    False,
    Static,
    Void,
    None,
    Unsafe,

    Ident(&'a str),
    Int {
        base: u8,
        value: &'a str,
        width: Option<&'a str>,
    },
    Float(&'a str),
    String(Cow<'a, str>),
}

impl Token<'_> {
    pub fn is_assignment(&self) -> bool {
        use Token::*;
        matches!(
            self,
            Assign
                | AddAssign
                | SubAssign
                | MulAssign
                | DivAssign
                | RemAssign
                | AndAssign
                | OrAssign
                | XorAssign
                | ShlAssign
                | ShrAssign
                | NoneCoalesceAssign
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    UnterminatedStr,
    UnterminatedComment,
    UnrecognizedChar,
    InvalidEscape,
}

impl Error {
    pub fn tell(&self) -> &'static str {
        match self {
            Error::UnterminatedStr => "unterminated string literal",
            Error::UnterminatedComment => "unterminated block comment",
            Error::UnrecognizedChar => "unexpected character",
            Error::InvalidEscape => "invalid UTF-8 escape sequence",
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

    #[inline]
    fn peek_next(&self) -> Option<char> {
        self.src.chars().nth(self.loc.pos + 1)
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

    fn string_literal(&mut self, start: usize) -> Result<Token<'a>, Located<Error>> {
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
                            return Err(Located::new(
                                Error::InvalidEscape,
                                Span {
                                    loc: self.loc,
                                    len: 1,
                                },
                            ));
                        }
                    });
                }
                Some('"') => break Ok(Token::String(result)),
                Some(ch) => match &mut result {
                    Cow::Borrowed(str) => *str = &self.src[start + 1..self.loc.pos],
                    Cow::Owned(str) => str.push(ch),
                },
                None => {
                    return Err(Located::new(
                        Error::UnterminatedStr,
                        Span {
                            loc: self.loc,
                            len: 0,
                        },
                    ));
                }
            }
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
        if self.src.chars().nth(start) == Some('0') {
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
        self.advance_while(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'));
        match &self.src[start..self.loc.pos] {
            "let" => Token::Let,
            "mut" => Token::Mut,
            "fn" => Token::Fn,
            "kw" => Token::Keyword,
            "pub" => Token::Pub,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "enum" => Token::Enum,
            "interface" => Token::Interface,
            "dyn" => Token::Dyn,
            "type" => Token::Type,
            "this" => Token::This,
            "if" => Token::If,
            "else" => Token::Else,
            "loop" => Token::Loop,
            "for" => Token::For,
            "in" => Token::In,
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
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            "static" => Token::Static,
            "void" => Token::Void,
            "none" => Token::None,
            "unsafe" => Token::Unsafe,
            id => Token::Ident(id),
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
            '0'..='9' => self.numeric_literal(start.pos),
            '_' | 'a'..='z' | 'A'..='Z' => self.identifier(start.pos),
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
