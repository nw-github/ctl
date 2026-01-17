use std::borrow::Cow;

use enum_as_inner::EnumAsInner;
use unicode_xid::UnicodeXID;

use crate::{
    error::{Diagnostics, Error, FileId},
    intern::{ByteStrId, StrId, Strings},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumAsInner)]
pub enum Token {
    LineComment(StrId),
    BlockComment(StrId),

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
    AtLBrace,
    At,
    Dollar,

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
    BitAndAssign,
    BitOr,
    BitOrAssign,
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
    Spaceship,

    Let,
    Mut,
    Raw,
    Fn,
    Keyword,
    Pub,
    Struct,
    Union,
    Enum,
    Trait,
    UnitTest,
    Dyn,
    Type,
    This,
    ThisType,
    Impl,
    If,
    Else,
    Loop,
    For,
    Guard,
    In,
    Is,
    As,
    My,
    While,
    Match,
    Extension,
    Packed,
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
    Sealed,
    Static,
    Const,
    Void,
    Super,
    Unsafe,
    Shared,
    Use,
    Then,
    Try,
    Catch,
    Defer,
    And,
    Or,

    Ident(StrId),
    Int { base: u8, value: StrId, width: Option<StrId> },
    Float { value: StrId, suffix: Option<StrId> },
    String(StrId),
    StringPart(StrId),

    ByteString(ByteStrId),
    Char(char),
    ByteChar(u8),
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LCurly => write!(f, "{{"),
            Token::RCurly => write!(f, "}}"),
            Token::LBrace => write!(f, "["),
            Token::RBrace => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Dollar => write!(f, "$"),
            Token::Dot => write!(f, "."),
            Token::Range => write!(f, ".."),
            Token::RangeInclusive => write!(f, "..="),
            Token::Ellipses => write!(f, "..."),
            Token::ScopeRes => write!(f, "::"),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Hash => write!(f, "#"),
            Token::AtLBrace => write!(f, "@["),
            Token::Assign => write!(f, "="),
            Token::Fn => write!(f, "fn"),
            Token::Extern => write!(f, "extern"),
            Token::Unsafe => write!(f, "unsafe"),
            Token::Pub => write!(f, "pub"),
            Token::Mut => write!(f, "mut"),
            Token::Keyword => write!(f, "kw"),
            Token::My => write!(f, "my"),
            Token::This => write!(f, "This"),
            Token::Struct => write!(f, "struct"),
            Token::Packed => write!(f, "packed"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Eof => write!(f, "EOF"),
            _ => write!(f, "FIXME: {self:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Min,
    Assignment,   // =
    Range,        // .. ..=
    Ternary,      // then else
    LogicalOr,    // ||
    LogicalAnd,   // &&
    Eq,           // != ==
    Comparison,   // < > <= >= <=> is
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

impl Token {
    pub fn precedence(&self) -> Precedence {
        use Token::*;

        match self {
            LBrace | LParen | Dot => Precedence::Call,
            Range | RangeInclusive => Precedence::Range,
            Plus | Minus => Precedence::Term,
            Asterisk | Div | Rem => Precedence::Factor,
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign | BitAndAssign
            | BitOrAssign | XorAssign | NoneCoalesceAssign | ShrAssign | ShlAssign => {
                Precedence::Assignment
            }
            Increment | Decrement | Exclamation | Question => Precedence::Postfix,
            Ampersand => Precedence::And,
            BitOr => Precedence::Or,
            Caret => Precedence::Xor,
            Shr | Shl => Precedence::Shift,
            And => Precedence::LogicalAnd,
            Or => Precedence::LogicalOr,
            Equal | NotEqual => Precedence::Eq,
            LAngle | RAngle | GtEqual | LtEqual | Spaceship | Is => Precedence::Comparison,
            NoneCoalesce => Precedence::NoneCoalesce,
            As => Precedence::Cast,
            Then => Precedence::Ternary,
            _ => Precedence::Min,
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Span {
    pub pos: u32,
    pub len: u32,
    pub file: FileId,
}

impl Span {
    pub const fn nowhere() -> Self {
        unsafe { std::mem::zeroed() }
    }

    pub fn extend_to(&mut self, b: impl Into<Span>) {
        *self = self.extended_to(b);
    }

    pub fn extended_to(self, b: impl Into<Span>) -> Self {
        let b: Span = b.into();
        debug_assert!(b.pos >= self.pos);
        Self { len: b.pos - self.pos + b.len, ..self }
    }

    pub fn includes(&self, pos: u32) -> bool {
        // include the right hand side since the cursor is often placed one character after, like
        // when selecting
        pos >= self.pos && pos <= self.pos + self.len
    }

    pub fn text<'a>(&self, src: &'a str) -> &'a str {
        &src[self.pos as usize..][..self.len as usize]
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, derive_more::Constructor)]
pub struct Located<T> {
    pub span: Span,
    pub data: T,
}

impl<T> Located<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located::new(self.span, f(self.data))
    }

    pub fn nowhere(data: T) -> Self {
        Self { span: Span::default(), data }
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
    interpolating: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file: FileId) -> Self {
        Self { src, file, pos: 0, interpolating: 0 }
    }

    pub fn is_identifier_char(ch: char) -> bool {
        ch.is_xid_continue()
    }

    pub fn is_identifier_first_char(ch: char) -> bool {
        ch == '_' || ch.is_xid_start()
    }

    pub fn make_reserved_ident(s: &str) -> Option<Token> {
        Some(match s {
            "and" => Token::And,
            "as" => Token::As,
            "async" => Token::Async,
            "await" => Token::Await,
            "break" => Token::Break,
            "catch" => Token::Catch,
            "const" => Token::Const,
            "continue" => Token::Continue,
            "defer" => Token::Defer,
            "dyn" => Token::Dyn,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "extension" => Token::Extension,
            "extern" => Token::Extern,
            "false" => Token::False,
            "fn" => Token::Fn,
            "for" => Token::For,
            "guard" => Token::Guard,
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
            "my" => Token::My,
            "or" => Token::Or,
            "packed" => Token::Packed,
            "pub" => Token::Pub,
            "raise" => Token::Raise,
            "raw" => Token::Raw,
            "return" => Token::Return,
            "sealed" => Token::Sealed,
            "shared" => Token::Shared,
            "static" => Token::Static,
            "struct" => Token::Struct,
            "super" => Token::Super,
            "union" => Token::Union,
            "unittest" => Token::UnitTest,
            "unsafe" => Token::Unsafe,
            "use" => Token::Use,
            "then" => Token::Then,
            "trait" => Token::Trait,
            "true" => Token::True,
            "try" => Token::Try,
            "type" => Token::Type,
            "void" => Token::Void,
            "while" => Token::While,
            "yield" => Token::Yield,
            crate::intern::THIS_PARAM => Token::This,
            crate::intern::THIS_TYPE => Token::ThisType,
            _ => return None,
        })
    }

    pub fn next(&mut self, diag: &mut Diagnostics, strings: &mut Strings) -> Located<Token> {
        self.advance_while(char::is_whitespace);

        let start = self.pos;
        let Some(next) = self.advance() else {
            return Located::new(self.here(0), Token::Eof);
        };
        let token = match next {
            '{' => Token::LCurly,
            '}' => {
                if self.interpolating > 0 {
                    self.string_literal(diag, strings, start, true)
                } else {
                    Token::RCurly
                }
            }
            '[' => Token::LBrace,
            ']' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '#' => {
                if self.advance_if('"') {
                    Token::String(strings.get_or_intern(self.raw_string_literal(diag, start + 1)))
                } else {
                    Token::Hash
                }
            }
            '$' => Token::Dollar,
            '@' => {
                if self.advance_if('[') {
                    Token::AtLBrace
                } else {
                    Token::At
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
            '/' => match self.peek() {
                Some('/') => {
                    self.advance();
                    Token::LineComment(strings.get_or_intern(self.advance_while(|ch| ch != '\n')))
                }
                Some('*') => {
                    self.advance();
                    let start = self.pos;
                    let mut count = 1;
                    while count > 0 {
                        match self.advance() {
                            Some('*') => {
                                if self.advance_if('/') {
                                    count -= 1;
                                }
                            }
                            Some('/') => {
                                if self.advance_if('*') {
                                    count += 1;
                                }
                            }
                            None => {
                                diag.report(Error::new("unterminated block comment", self.here(0)));
                                break;
                            }
                            _ => {}
                        }
                    }
                    Token::BlockComment(strings.get_or_intern(&self.src[start..self.pos]))
                }
                Some('=') => {
                    self.advance();
                    Token::DivAssign
                }
                _ => Token::Div,
            },
            '%' => {
                if self.advance_if('=') {
                    Token::RemAssign
                } else {
                    Token::Rem
                }
            }
            '&' => {
                if self.advance_if('=') {
                    Token::BitAndAssign
                } else {
                    Token::Ampersand
                }
            }
            '|' => {
                if self.advance_if('=') {
                    Token::BitOrAssign
                } else {
                    Token::BitOr
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
                    if self.advance_if('=') { Token::ShrAssign } else { Token::Shr }
                } else {
                    Token::RAngle
                }
            }
            '<' => {
                if self.advance_if('=') {
                    if self.advance_if('>') { Token::Spaceship } else { Token::LtEqual }
                } else if self.advance_if('<') {
                    if self.advance_if('=') { Token::ShlAssign } else { Token::Shl }
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
            '"' => self.string_literal(diag, strings, start, false),
            '\'' => Token::Char(self.char_literal(diag, false)),
            ch @ '0'..='9' => self.numeric_literal(strings, ch, start),
            'b' => self.maybe_byte_string(diag, strings, start),
            ch if Self::is_identifier_first_char(ch) => self.identifier(strings, start),
            ch => {
                diag.report(Error::new(format!("unexpected character '{ch}'"), self.here(0)));
                return self.next(diag, strings);
            }
        };

        Located::new(
            Span { pos: start as u32, len: (self.pos - start) as u32, file: self.file },
            token,
        )
    }

    pub fn next_skip_comments(
        &mut self,
        diag: &mut Diagnostics,
        strings: &mut Strings,
    ) -> Located<Token> {
        loop {
            let t = self.next(diag, strings);
            if !matches!(t.data, Token::LineComment(_) | Token::BlockComment(_)) {
                break t;
            }
        }
    }

    pub fn create_format_lexer(&self) -> FormatLexer<'a> {
        FormatLexer::new(self.src, self.pos, self.file)
    }

    pub fn advance_to(&mut self, pos: Option<u32>) {
        self.pos = pos.map(|p| p as usize).unwrap_or(self.src.len());
    }

    pub fn source(&self) -> &'a str {
        self.src
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.src[self.pos..].chars().nth(1)
    }

    fn here(&self, len: usize) -> Span {
        Span { pos: self.pos as u32, file: self.file, len: len as u32 }
    }

    fn advance(&mut self) -> Option<char> {
        self.peek().inspect(|ch| self.pos += ch.len_utf8())
    }

    fn advance_if(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance_if_pred(&mut self, pred: impl FnOnce(char) -> bool) -> bool {
        if let Some(next) = self.peek()
            && pred(next)
        {
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

    fn expect(&mut self, diag: &mut Diagnostics, c: char) {
        if !matches!(self.peek(), Some(ch) if ch == c) {
            diag.report(Error::new(format!("expected '{c}'"), self.here(1)));
        } else {
            self.advance();
        }
    }

    fn escape_char(&mut self, diag: &mut Diagnostics, byte: bool) -> char {
        match self.advance() {
            Some('"') => '"',
            Some('\'') => '\'',
            Some('\\') => '\\',
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('e') => '\x1b',
            Some('0') => '\0',
            Some('{') => '{',
            Some('}') => '}',
            Some('x') => {
                let mut count = 0;
                let chars = self.advance_while(|ch| {
                    let result = count < 2 && ch.is_ascii_hexdigit();
                    count += 1;
                    result
                });
                let span = Span {
                    pos: (self.pos - chars.len()) as u32,
                    file: self.file,
                    len: chars.len() as u32,
                };
                if chars.len() < 2 {
                    diag.report(Error::new("hexadecimal literal is too short", span));
                    '\0'
                } else if let Ok(ch) = u8::from_str_radix(chars, 16) {
                    let ch = char::from(ch);
                    if !byte && !ch.is_ascii() {
                        diag.report(Error::non_ascii_char(span));
                    }
                    ch
                } else {
                    diag.report(Error::non_ascii_char(span));
                    '\0'
                }
            }
            Some('u') => {
                let start = self.pos;
                self.expect(diag, '{');
                let chars = self.advance_while(|ch| ch.is_ascii_hexdigit());
                if chars.is_empty() {
                    diag.report(Error::new("expected hexadecimal digits", self.here(1)));
                    '\0'
                } else if let Some(ch) =
                    u32::from_str_radix(chars, 16).ok().and_then(char::from_u32)
                {
                    self.expect(diag, '}');
                    if byte {
                        diag.report(Error::new(
                            "cannot include unicode escape in byte string",
                            Span {
                                pos: start as u32,
                                file: self.file,
                                len: (self.pos - start) as u32,
                            },
                        ));
                        '\0'
                    } else {
                        ch
                    }
                } else {
                    diag.report(Error::new(
                        "invalid unicode literal",
                        Span {
                            pos: (self.pos - chars.len()) as u32,
                            file: self.file,
                            len: chars.len() as u32,
                        },
                    ));
                    self.expect(diag, '}');
                    '\0'
                }
            }
            ch => {
                let mut span = self.here(1);
                if let Some(ch) = ch {
                    span.pos -= ch.len_utf8() as u32;
                }
                diag.report(Error::new("invalid escape sequence", span));
                '\0'
            }
        }
    }

    fn string_literal(
        &mut self,
        diag: &mut Diagnostics,
        strings: &mut Strings,
        start: usize,
        interp: bool,
    ) -> Token {
        let mut result = Cow::from("");
        loop {
            match self.advance() {
                Some('"') => {
                    if interp {
                        self.interpolating -= 1;
                    }
                    break Token::String(strings.get_or_intern(result));
                }
                Some('\\') => {
                    result.to_mut().push(self.escape_char(diag, false));
                }
                Some('{') => {
                    if !interp {
                        self.interpolating += 1;
                    }
                    break Token::StringPart(strings.get_or_intern(result));
                }
                Some(ch) => match &mut result {
                    Cow::Borrowed(str) => *str = &self.src[start + 1..self.pos],
                    Cow::Owned(str) => str.push(ch),
                },
                None => {
                    diag.report(Error::new("unterminated string literal", self.here(1)));
                    break Token::String(strings.get_or_intern(result));
                }
            }
        }
    }

    fn char_literal(&mut self, diag: &mut Diagnostics, byte: bool) -> char {
        let mut here =
            Span { pos: self.pos as u32 - 1 - byte as u32, len: 1 + byte as u32, file: self.file };
        let ch = match self.advance() {
            Some('\\') => self.escape_char(diag, byte),
            Some('\'') => {
                here.len += 1;
                diag.report(Error::new("empty char literal", here));
                return '\0';
            }
            Some(any) => any,
            None => {
                diag.report(Error::new("unterminated char literal", here));
                '\0'
            }
        };

        if !self.advance_if('\'') {
            let ch = self.advance_while(|c| c != '\'');
            self.advance();
            diag.report(Error::new(
                "char literal must only contain one character",
                Span {
                    pos: (self.pos - ch.len() - 1) as u32,
                    len: ch.len() as u32,
                    file: self.file,
                },
            ));
        }
        ch
    }

    fn raw_string_literal(&mut self, diag: &mut Diagnostics, start: usize) -> &'a str {
        let mut result = "";
        loop {
            match self.advance() {
                Some('"') if self.advance_if('#') => break result,
                Some(_) => result = &self.src[start + 1..self.pos],
                None => {
                    diag.report(Error::new("unterminated string literal", self.here(1)));
                    break result;
                }
            }
        }
    }

    fn numeric_literal(&mut self, strings: &mut Strings, ch: char, start: usize) -> Token {
        let mut value = |value: &str| strings.get_or_intern(value);
        let suffix = |this: &mut Self, strings: &mut Strings| {
            let suffix = this.advance_while(|s| s.is_ascii_alphanumeric());
            if !suffix.is_empty() { Some(strings.get_or_intern(suffix)) } else { None }
        };

        if ch == '0' {
            match self.peek() {
                Some('x') => {
                    self.advance();
                    return Token::Int {
                        base: 16,
                        value: value(self.advance_while(|ch| ch.is_ascii_hexdigit() || ch == '_')),
                        width: suffix(self, strings),
                    };
                }
                Some('o') => {
                    self.advance();
                    return Token::Int {
                        base: 8,
                        value: value(self.advance_while(|ch| ch.is_digit(8) || ch == '_')),
                        width: suffix(self, strings),
                    };
                }
                Some('b') => {
                    self.advance();
                    return Token::Int {
                        base: 2,
                        value: value(self.advance_while(|ch| ch.is_digit(2) || ch == '_')),
                        width: suffix(self, strings),
                    };
                }
                _ => {}
            }
        }

        self.advance_while(|ch| ch.is_ascii_digit() || ch == '_');

        let mut float = false;
        if self.peek() == Some('.')
            && self.peek_next().is_some_and(|f| {
                f.is_ascii_digit() || (f != '.' && !Self::is_identifier_first_char(f))
            })
        {
            self.advance();
            self.advance_while(|ch| ch.is_ascii_digit() || ch == '_');
            float = true;
        }

        if self.advance_if_pred(|ch| matches!(ch, 'e' | 'E')) {
            self.advance_if_pred(|ch| matches!(ch, '+' | '-'));
            self.advance_while(|ch| ch.is_ascii_digit() || ch == '_');
            float = true;
        }

        let value = value(&self.src[start..self.pos]);
        if float {
            return Token::Float { value, suffix: suffix(self, strings) };
        }

        Token::Int { value, base: 10, width: suffix(self, strings) }
    }

    fn identifier(&mut self, strings: &mut Strings, start: usize) -> Token {
        self.advance_while(Self::is_identifier_char);
        let data = &self.src[start..self.pos];
        if let Some(tok) = Lexer::make_reserved_ident(data) {
            tok
        } else {
            Token::Ident(strings.get_or_intern(data))
        }
    }

    fn maybe_byte_string(
        &mut self,
        diag: &mut Diagnostics,
        strings: &mut Strings,
        start: usize,
    ) -> Token {
        fn bchar(diag: &mut Diagnostics, c: char, name: &str, span: Span) -> u8 {
            match c.try_into() {
                Ok(c) => c,
                Err(_) => {
                    diag.report(Error::new(
                        format!("invalid character '{c}' ({:#x}) in byte {name} literal", c as u8),
                        span,
                    ));
                    0
                }
            }
        }

        match self.peek() {
            Some('#') => {
                self.advance();
                self.expect(diag, '"');
                Token::ByteString(strings.intern_byte_str(self.raw_string_literal(diag, start)))
            }
            Some('\'') => {
                self.advance();
                let prev = self.here(0);
                let ch = self.char_literal(diag, true);
                Token::ByteChar(bchar(diag, ch, "char", prev.extended_to(self.here(0))))
            }
            Some('"') => {
                self.advance();
                let mut result = vec![];
                loop {
                    let prev = self.here(0);
                    match self.advance() {
                        Some('"') => break Token::ByteString(strings.intern_byte_str(result)),
                        Some('\\') => {
                            let ch = self.escape_char(diag, true);
                            result.push(bchar(diag, ch, "string", prev.extended_to(self.here(0))))
                        }
                        Some(ch) => {
                            result.push(bchar(diag, ch, "string", prev.extended_to(self.here(0))))
                        }
                        None => {
                            diag.report(Error::unterminated_str(self.here(1)));
                            break Token::ByteString(strings.intern_byte_str(result));
                        }
                    }
                }
            }
            _ => self.identifier(strings, start),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatToken<'a> {
    Space,
    Dollar,
    Dot,
    LAngle,
    RAngle,
    Caret,
    Plus,
    Minus,
    Hash,
    Question,
    LeadingZero,
    Number(&'a str),
    Ident(&'a str),
    Char(char),
}

#[derive(Clone)]
pub struct FormatLexer<'a> {
    src: &'a str,
    pos: usize,
    file: FileId,
}

impl<'a> FormatLexer<'a> {
    fn new(src: &'a str, pos: usize, file: FileId) -> Self {
        Self { src, file, pos }
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    pub fn peek_next(&self) -> Option<char> {
        self.src[self.pos..].chars().nth(1)
    }

    pub fn advance(&mut self) -> Option<char> {
        self.peek().inspect(|ch| self.pos += ch.len_utf8())
    }

    fn advance_while(&mut self, mut f: impl FnMut(char) -> bool) -> &'a str {
        let start = self.pos;
        while self.peek().is_some_and(&mut f) {
            self.advance();
        }
        &self.src[start..self.pos]
    }
}

impl<'a> Iterator for FormatLexer<'a> {
    type Item = Located<FormatToken<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.pos;
        let token = match self.advance()? {
            ' ' => FormatToken::Space,
            '$' => FormatToken::Dollar,
            '.' => FormatToken::Dot,
            '<' => FormatToken::LAngle,
            '>' => FormatToken::RAngle,
            '^' => FormatToken::Caret,
            '+' => FormatToken::Plus,
            '-' => FormatToken::Minus,
            '#' => FormatToken::Hash,
            '?' => FormatToken::Question,
            '0' => FormatToken::LeadingZero,
            ch if ch.is_ascii_digit() => {
                self.advance_while(|ch| ch.is_ascii_digit());
                let value = &self.src[start..self.pos];
                FormatToken::Number(value)
            }
            ch if Lexer::is_identifier_first_char(ch) => {
                self.advance_while(Lexer::is_identifier_char);
                let value = &self.src[start..self.pos];
                FormatToken::Ident(value)
            }
            ch => FormatToken::Char(ch),
        };

        Some(Located::new(
            Span { pos: start as u32, len: (self.pos - start) as u32, file: self.file },
            token,
        ))
    }
}
