use std::borrow::Cow;

use crate::{
    ds::ComptimeInt,
    intern::{StrId, Strings},
    lexer::{Located, Token},
};
use derive_more::{Deref, DerefMut, Display};

use self::parsed::OperatorFnType;

pub mod checked;
pub mod declared;
pub mod parsed;

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum AttrName {
    Str(StrId),
    Int(ComptimeInt),
}

impl AttrName {
    pub fn is_str_eq(&self, name: StrId) -> bool {
        self.as_str().is_some_and(|&n| n == name)
    }

    pub fn as_str_data<'a>(&self, strings: &'a Strings) -> Cow<'a, str> {
        match self {
            AttrName::Str(id) => Cow::Borrowed(strings.resolve(id)),
            AttrName::Int(int) => Cow::Owned(format!("{int}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Located<AttrName>,
    pub props: Vec<Attribute>,
}

#[derive(Default, Debug, Clone, Deref, DerefMut)]
pub struct Attributes {
    attrs: Vec<Attribute>,
}

impl Attributes {
    pub fn new(attrs: Vec<Attribute>) -> Self {
        Self { attrs }
    }

    pub fn val(&self, name: StrId) -> Option<StrId> {
        self.attrs
            .iter()
            .find(|attr| attr.name.data.is_str_eq(name))
            .and_then(|attr| attr.props.first())
            .and_then(|attr| attr.name.data.as_str().copied())
    }

    pub fn has(&self, name: StrId) -> bool {
        self.attrs.iter().any(|attr| attr.name.data.is_str_eq(name))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, Hash)]
pub enum BinaryOp {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
    #[display(fmt = "&")]
    BitAnd,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "|")]
    BitOr,
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = ">>")]
    Shr,
    #[display(fmt = "??")]
    NoneCoalesce,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = ">=")]
    GtEqual,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = "<=")]
    LtEqual,
    #[display(fmt = "<=>")]
    Cmp,
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
    #[display(fmt = "or")]
    LogicalOr,
    #[display(fmt = "and")]
    LogicalAnd,
    #[display(fmt = "=")]
    Assign,
    #[display(fmt = "+=")]
    AddAssign,
    #[display(fmt = "-=")]
    SubAssign,
    #[display(fmt = "*=")]
    MulAssign,
    #[display(fmt = "/=")]
    DivAssign,
    #[display(fmt = "%=")]
    RemAssign,
    #[display(fmt = "&=")]
    BitAndAssign,
    #[display(fmt = "^=")]
    XorAssign,
    #[display(fmt = "|=")]
    BitOrAssign,
    #[display(fmt = "<<=")]
    ShlAssign,
    #[display(fmt = ">>=")]
    ShrAssign,
    #[display(fmt = "??=")]
    NoneCoalesceAssign,
    Call,
}

impl TryFrom<Token> for BinaryOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(BinaryOp::Add),
            Token::Minus => Ok(BinaryOp::Sub),
            Token::Asterisk => Ok(BinaryOp::Mul),
            Token::Div => Ok(BinaryOp::Div),
            Token::Rem => Ok(BinaryOp::Rem),
            Token::Ampersand => Ok(BinaryOp::BitAnd),
            Token::Caret => Ok(BinaryOp::Xor),
            Token::BitOr => Ok(BinaryOp::BitOr),
            Token::NoneCoalesce => Ok(BinaryOp::NoneCoalesce),
            Token::RAngle => Ok(BinaryOp::Gt),
            Token::GtEqual => Ok(BinaryOp::GtEqual),
            Token::LAngle => Ok(BinaryOp::Lt),
            Token::Shl => Ok(BinaryOp::Shl),
            Token::Shr => Ok(BinaryOp::Shr),
            Token::LtEqual => Ok(BinaryOp::LtEqual),
            Token::Equal => Ok(BinaryOp::Equal),
            Token::NotEqual => Ok(BinaryOp::NotEqual),
            Token::And => Ok(BinaryOp::LogicalAnd),
            Token::Or => Ok(BinaryOp::LogicalOr),
            Token::Spaceship => Ok(BinaryOp::Cmp),
            Token::Assign => Ok(BinaryOp::Assign),
            Token::AddAssign => Ok(BinaryOp::AddAssign),
            Token::SubAssign => Ok(BinaryOp::SubAssign),
            Token::MulAssign => Ok(BinaryOp::MulAssign),
            Token::DivAssign => Ok(BinaryOp::DivAssign),
            Token::RemAssign => Ok(BinaryOp::RemAssign),
            Token::BitAndAssign => Ok(BinaryOp::BitAndAssign),
            Token::BitOrAssign => Ok(BinaryOp::BitOrAssign),
            Token::XorAssign => Ok(BinaryOp::XorAssign),
            Token::ShlAssign => Ok(BinaryOp::ShlAssign),
            Token::ShrAssign => Ok(BinaryOp::ShrAssign),
            Token::NoneCoalesceAssign => Ok(BinaryOp::NoneCoalesceAssign),
            _ => Err(()),
        }
    }
}

impl TryFrom<OperatorFnType> for BinaryOp {
    type Error = ();

    fn try_from(value: OperatorFnType) -> Result<Self, Self::Error> {
        match value {
            OperatorFnType::Plus => Ok(BinaryOp::Add),
            OperatorFnType::Minus => Ok(BinaryOp::Sub),
            OperatorFnType::Mul => Ok(BinaryOp::Mul),
            OperatorFnType::Div => Ok(BinaryOp::Div),
            OperatorFnType::Rem => Ok(BinaryOp::Rem),
            OperatorFnType::BitAnd => Ok(BinaryOp::BitAnd),
            OperatorFnType::BitOr => Ok(BinaryOp::BitOr),
            OperatorFnType::Xor => Ok(BinaryOp::Xor),
            OperatorFnType::Shl => Ok(BinaryOp::Shl),
            OperatorFnType::Shr => Ok(BinaryOp::Shr),
            OperatorFnType::Eq => Ok(BinaryOp::Equal),
            OperatorFnType::Cmp => Ok(BinaryOp::Cmp),
            OperatorFnType::AddAssign => Ok(BinaryOp::AddAssign),
            OperatorFnType::SubAssign => Ok(BinaryOp::SubAssign),
            OperatorFnType::MulAssign => Ok(BinaryOp::MulAssign),
            OperatorFnType::DivAssign => Ok(BinaryOp::DivAssign),
            OperatorFnType::RemAssign => Ok(BinaryOp::RemAssign),
            OperatorFnType::BitAndAssign => Ok(BinaryOp::BitAndAssign),
            OperatorFnType::BitOrAssign => Ok(BinaryOp::BitOrAssign),
            OperatorFnType::XorAssign => Ok(BinaryOp::XorAssign),
            OperatorFnType::ShlAssign => Ok(BinaryOp::ShlAssign),
            OperatorFnType::ShrAssign => Ok(BinaryOp::ShrAssign),
            OperatorFnType::Increment => Err(()),
            OperatorFnType::Decrement => Err(()),
            OperatorFnType::Bang => Err(()),
            OperatorFnType::Subscript => Err(()),
            OperatorFnType::SubscriptAssign => Err(()),
        }
    }
}

impl BinaryOp {
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            BinaryOp::Assign
                | BinaryOp::AddAssign
                | BinaryOp::SubAssign
                | BinaryOp::MulAssign
                | BinaryOp::DivAssign
                | BinaryOp::RemAssign
                | BinaryOp::BitAndAssign
                | BinaryOp::BitOrAssign
                | BinaryOp::XorAssign
                | BinaryOp::ShlAssign
                | BinaryOp::ShrAssign
                | BinaryOp::NoneCoalesceAssign
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, Hash)]
pub enum UnaryOp {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Neg,
    #[display(fmt = "++")]
    PostIncrement,
    #[display(fmt = "--")]
    PostDecrement,
    #[display(fmt = "++")]
    PreIncrement,
    #[display(fmt = "--")]
    PreDecrement,
    #[display(fmt = "!")]
    Not,
    #[display(fmt = "*")]
    Deref,
    #[display(fmt = "&")]
    Addr,
    #[display(fmt = "&mut")]
    AddrMut,
    #[display(fmt = "&raw")]
    AddrRaw,
    #[display(fmt = "&raw mut")]
    AddrRawMut,
    #[display(fmt = "!")]
    Unwrap,
    #[display(fmt = "?")]
    Try,
    #[display(fmt = "?")]
    Option,
}

impl UnaryOp {
    pub fn try_from_postfix(value: Token) -> Option<Self> {
        match value {
            Token::Increment => Some(UnaryOp::PostIncrement),
            Token::Decrement => Some(UnaryOp::PostDecrement),
            Token::Exclamation => Some(UnaryOp::Unwrap),
            Token::Question => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn try_from_prefix(value: Token) -> Option<Self> {
        match value {
            Token::Plus => Some(UnaryOp::Plus),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Asterisk => Some(UnaryOp::Deref),
            Token::Ampersand => Some(UnaryOp::Addr),
            Token::Increment => Some(UnaryOp::PreIncrement),
            Token::Decrement => Some(UnaryOp::PreDecrement),
            Token::Exclamation => Some(UnaryOp::Not),
            Token::Question => Some(UnaryOp::Option),
            _ => None,
        }
    }

    pub fn try_from_postfix_fn(value: OperatorFnType) -> Option<Self> {
        match value {
            OperatorFnType::Plus => Some(UnaryOp::Plus),
            OperatorFnType::Minus => Some(UnaryOp::Neg),
            OperatorFnType::Increment => Some(UnaryOp::PostIncrement),
            OperatorFnType::Decrement => Some(UnaryOp::PostDecrement),
            OperatorFnType::Bang => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Alignment {
    Left,
    Right,
    Center,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefaultCapturePolicy {
    #[default]
    None,
    ByVal,
    ByValMut,
    ByPtr,
    ByMutPtr,
    Auto,
}
