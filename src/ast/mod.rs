use crate::lexer::{Located, Token};
use derive_more::{Deref, Display};

use self::parsed::OperatorFnType;

pub mod checked;
pub mod declared;
pub mod parsed;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Located<String>,
    pub props: Vec<Attribute>,
}

#[derive(Default, Debug, Clone, Deref)]
pub struct Attributes {
    attrs: Vec<Attribute>,
}

impl Attributes {
    pub fn new(attrs: Vec<Attribute>) -> Self {
        Self { attrs }
    }

    pub fn val(&self, name: &str) -> Option<&str> {
        self.attrs
            .iter()
            .find(|attr| attr.name.data == name)
            .and_then(|attr| attr.props.first())
            .map(|attr| &attr.name.data[..])
    }

    pub fn has(&self, name: &str) -> bool {
        self.attrs.iter().any(|attr| attr.name.data == name)
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
    And,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "|")]
    Or,
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
    #[display(fmt = "||")]
    LogicalOr,
    #[display(fmt = "&&")]
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
    AndAssign,
    #[display(fmt = "^=")]
    XorAssign,
    #[display(fmt = "|=")]
    OrAssign,
    #[display(fmt = "<<=")]
    ShlAssign,
    #[display(fmt = ">>=")]
    ShrAssign,
    #[display(fmt = "??=")]
    NoneCoalesceAssign,
}

impl TryFrom<Token<'_>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(BinaryOp::Add),
            Token::Minus => Ok(BinaryOp::Sub),
            Token::Asterisk => Ok(BinaryOp::Mul),
            Token::Div => Ok(BinaryOp::Div),
            Token::Rem => Ok(BinaryOp::Rem),
            Token::Ampersand => Ok(BinaryOp::And),
            Token::Caret => Ok(BinaryOp::Xor),
            Token::Or => Ok(BinaryOp::Or),
            Token::NoneCoalesce => Ok(BinaryOp::NoneCoalesce),
            Token::RAngle => Ok(BinaryOp::Gt),
            Token::GtEqual => Ok(BinaryOp::GtEqual),
            Token::LAngle => Ok(BinaryOp::Lt),
            Token::Shl | Token::ShlAssign => Ok(BinaryOp::Shl),
            Token::Shr | Token::ShrAssign => Ok(BinaryOp::Shr),
            Token::LtEqual => Ok(BinaryOp::LtEqual),
            Token::Equal => Ok(BinaryOp::Equal),
            Token::NotEqual => Ok(BinaryOp::NotEqual),
            Token::LogicalAnd => Ok(BinaryOp::LogicalAnd),
            Token::LogicalOr => Ok(BinaryOp::LogicalOr),
            Token::Spaceship => Ok(BinaryOp::Cmp),
            Token::Assign => Ok(BinaryOp::Assign),
            Token::AddAssign => Ok(BinaryOp::AddAssign),
            Token::SubAssign => Ok(BinaryOp::SubAssign),
            Token::MulAssign => Ok(BinaryOp::MulAssign),
            Token::DivAssign => Ok(BinaryOp::DivAssign),
            Token::RemAssign => Ok(BinaryOp::RemAssign),
            Token::AndAssign => Ok(BinaryOp::AndAssign),
            Token::XorAssign => Ok(BinaryOp::XorAssign),
            Token::OrAssign => Ok(BinaryOp::OrAssign),
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
            OperatorFnType::And => Ok(BinaryOp::And),
            OperatorFnType::Or => Ok(BinaryOp::Or),
            OperatorFnType::Xor => Ok(BinaryOp::Xor),
            OperatorFnType::Shl => Ok(BinaryOp::Shl),
            OperatorFnType::Shr => Ok(BinaryOp::Shr),
            OperatorFnType::Eq => Ok(BinaryOp::Equal),
            OperatorFnType::Cmp => Ok(BinaryOp::Cmp),
            OperatorFnType::Increment => Err(()),
            OperatorFnType::Decrement => Err(()),
            OperatorFnType::Bang => Err(()),
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
                | BinaryOp::AndAssign
                | BinaryOp::OrAssign
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
    #[display(fmt = "!")]
    Unwrap,
    #[display(fmt = "?")]
    Try,
}

impl UnaryOp {
    pub fn try_from_postfix(value: Token<'_>) -> Option<Self> {
        match value {
            Token::Increment => Some(UnaryOp::PostIncrement),
            Token::Decrement => Some(UnaryOp::PostDecrement),
            Token::Exclamation => Some(UnaryOp::Unwrap),
            Token::Question => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn try_from_prefix(value: Token<'_>) -> Option<Self> {
        match value {
            Token::Plus => Some(UnaryOp::Plus),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Asterisk => Some(UnaryOp::Deref),
            Token::Ampersand => Some(UnaryOp::Addr),
            Token::Increment => Some(UnaryOp::PreIncrement),
            Token::Decrement => Some(UnaryOp::PreDecrement),
            Token::Exclamation => Some(UnaryOp::Not),
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
