use crate::lexer::Located;

use self::stmt::TypeHint;

pub type Expr = Located<expr::Expr>;
pub type Stmt = Located<stmt::Stmt>;

#[derive(Debug)]
pub struct Path {
    pub components: Vec<(String, Vec<TypeHint>)>,
    pub root: bool,
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Self {
            components: vec![(value, Vec::new())],
            root: false,
        }
    }
}

impl Path {
    pub fn as_symbol(&self) -> Option<&str> {
        (self.components.len() == 1 && self.components[0].1.is_empty())
            .then_some(&self.components[0].0)
    }
}

pub mod expr {
    use derive_more::Display;

    use crate::lexer::Token;

    use super::{Path, stmt::TypeHint};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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
        #[display(fmt = "!!")]
        ErrCoalesce,
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
                Token::ErrCoalesce => Ok(BinaryOp::ErrCoalesce),
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
        #[display(fmt = "sizeof")]
        Sizeof,
    }

    impl TryFrom<Token<'_>> for UnaryOp {
        type Error = ();

        fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
            match value {
                Token::Plus => Ok(UnaryOp::Plus),
                Token::Minus => Ok(UnaryOp::Neg),
                Token::Asterisk => Ok(UnaryOp::Deref),
                Token::Ampersand => Ok(UnaryOp::Addr),
                Token::Increment => Ok(UnaryOp::PreIncrement),
                Token::Decrement => Ok(UnaryOp::PreDecrement),
                Token::Exclamation => Ok(UnaryOp::Not),
                Token::Sizeof => Ok(UnaryOp::Sizeof),
                _ => Err(()),
            }
        }
    }

    #[derive(Debug)]
    pub enum Expr {
        Binary {
            op: BinaryOp,
            left: Box<super::Expr>,
            right: Box<super::Expr>,
        },
        Unary {
            op: UnaryOp,
            expr: Box<super::Expr>,
        },
        Call {
            callee: Box<super::Expr>,
            args: Vec<(Option<String>, super::Expr)>,
        },
        Array(Vec<super::Expr>),
        ArrayWithInit {
            init: Box<super::Expr>,
            count: Box<super::Expr>,
        },
        Tuple(Vec<super::Expr>),
        Map(Vec<(super::Expr, super::Expr)>),
        Bool(bool),
        Integer {
            base: u8,
            value: String,
            width: Option<String>,
        },
        Float(String),
        String(String),
        Path(Path),
        Void,
        None,
        Assign {
            target: Box<super::Expr>,
            binary: Option<BinaryOp>,
            value: Box<super::Expr>,
        },
        Block(Vec<super::Stmt>),
        If {
            cond: Box<super::Expr>,
            if_branch: Box<super::Expr>,
            else_branch: Option<Box<super::Expr>>,
        },
        Loop {
            cond: Box<super::Expr>,
            body: Vec<super::Stmt>,
            do_while: bool,
        },
        For {
            var: String,
            iter: Box<super::Expr>,
            body: Vec<super::Stmt>,
        },
        Member {
            source: Box<super::Expr>,
            generics: Vec<TypeHint>,
            member: String,
        },
        Subscript {
            callee: Box<super::Expr>,
            args: Vec<super::Expr>,
        },
        Return(Box<super::Expr>),
        Yield(Box<super::Expr>),
        Break(Box<super::Expr>),
        Range {
            start: Option<Box<super::Expr>>,
            end: Option<Box<super::Expr>>,
            inclusive: bool,
        },
        Continue,
    }
}

pub mod stmt {
    use crate::lexer::Located;

    use super::Path;

    #[derive(Debug)]
    pub enum TypeHint {
        Regular { is_dyn: bool, path: Located<Path> },
        Array(Box<TypeHint>, Box<super::Expr>),
        Slice(Box<TypeHint>),
        Tuple(Vec<TypeHint>),
        Map(Box<TypeHint>, Box<TypeHint>),
        Option(Box<TypeHint>),
        Result(Box<TypeHint>, Box<TypeHint>),
        Ref(Box<TypeHint>),
        RefMut(Box<TypeHint>),
        Anon(Box<ParsedUserType>),
        Void,
        This,
        MutThis,
    }

    #[derive(Debug)]
    pub struct Param {
        pub mutable: bool,
        pub keyword: bool,
        pub name: String,
        pub ty: TypeHint,
        pub default: Option<super::Expr>,
    }

    #[derive(Debug)]
    pub struct Prototype {
        pub public: bool,
        pub name: String,
        pub is_async: bool,
        pub is_extern: bool,
        pub type_params: Vec<String>,
        pub params: Vec<Param>,
        pub ret: TypeHint,
    }

    #[derive(Debug)]
    pub struct Fn {
        pub proto: Prototype,
        pub body: Vec<super::Stmt>,
    }

    #[derive(Debug)]
    pub struct MemVar {
        pub public: bool,
        pub ty: TypeHint,
        pub default: Option<super::Expr>,
    }

    #[derive(Debug)]
    pub struct Struct {
        pub public: bool,
        pub name: String,
        pub type_params: Vec<String>,
        pub members: Vec<(String, MemVar)>,
        pub impls: Vec<Located<Path>>,
        pub functions: Vec<Fn>,
    }

    #[derive(Debug)]
    pub enum ParsedUserType {
        Struct(Struct),
        Union {
            tag: Option<Located<Path>>,
            base: Struct,
        },
        Interface {
            public: bool,
            name: String,
            type_params: Vec<String>,
            impls: Vec<Located<Path>>,
            functions: Vec<Prototype>,
        },
        Enum {
            public: bool,
            name: String,
            impls: Vec<Located<Path>>,
            variants: Vec<(String, Option<super::Expr>)>,
            functions: Vec<Fn>,
        },
    }

    #[derive(Debug)]
    pub enum Stmt {
        Expr(super::Expr),
        Let {
            name: String,
            ty: Option<TypeHint>,
            mutable: bool,
            value: Option<super::Expr>,
        },
        Fn(Fn),
        UserType(ParsedUserType),
        Static {
            public: bool,
            name: String,
            ty: Option<TypeHint>,
            value: super::Expr,
        },
        Module {
            public: bool,
            name: String,
            body: Vec<super::Stmt>,
        },
    }
}
