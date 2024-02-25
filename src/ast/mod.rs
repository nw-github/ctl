use crate::lexer::{Located, Token};
use derive_more::{Deref, Display};

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
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
    #[display(fmt = "||")]
    LogicalOr,
    #[display(fmt = "&&")]
    LogicalAnd,
}

impl TryFrom<Token<'_>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
        match value {
            Token::Plus | Token::AddAssign => Ok(BinaryOp::Add),
            Token::Minus | Token::SubAssign => Ok(BinaryOp::Sub),
            Token::Asterisk | Token::MulAssign => Ok(BinaryOp::Mul),
            Token::Div | Token::DivAssign => Ok(BinaryOp::Div),
            Token::Rem | Token::RemAssign => Ok(BinaryOp::Rem),
            Token::Ampersand | Token::AndAssign => Ok(BinaryOp::And),
            Token::Caret | Token::XorAssign => Ok(BinaryOp::Xor),
            Token::Or | Token::OrAssign => Ok(BinaryOp::Or),
            Token::NoneCoalesce | Token::NoneCoalesceAssign => Ok(BinaryOp::NoneCoalesce),
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
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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
}
