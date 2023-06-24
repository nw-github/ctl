use crate::lexer::Located;

type Expr = Located<expr::Expr>;
type Stmt = Located<stmt::Stmt>;

pub mod expr {
    pub enum BinaryOp {
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        And,
        Xor,
        Or,
        NullCoalesce,
        ErrCoalesce,
        Gt,
        GtEqual,
        Lt,
        LtEqual,
        Equal,
        NotEqual,
    }

    pub enum UnaryOp {
        Plus,
        Neg,
        PostIncrement,
        PostDecrement,
        PreIncrement,
        PreDecrement,
        Not,
        Deref,
        AddrOf,
    }
    
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
            args: Vec<super::Expr>,
        },
        Array(Vec<super::Expr>),
        Tuple(Vec<super::Expr>),
        Map(Vec<(super::Expr, super::Expr)>),
        Literal/*(???)*/,
        Symbol(String),
        Assign {
            target: String,
            binary: Option<BinaryOp>,
            value: Box<Expr>,
        },
        Block(Vec<super::Stmt>),
        If {
            cond: Box<super::Expr>,
            if_branch: Box<super::Expr>,
            else_branch: Box<super::Expr>,
        },
        Loop {
            cond: Box<super::Expr>,
            body: Vec<super::Stmt>,
        },
        Member {
            source: Box<super::Expr>,
            member: String,
        },
        MemberAssign {
            source: Box<super::Expr>,
            member: String,
            value: Box<super::Expr>,
            binary: Option<BinaryOp>,
        },
        Subscript {
            callee: Box<Expr>,
            args: Vec<Expr>,
        },
        SubscriptAssign {
            callee: Box<Expr>,
            args: Vec<Expr>,
            value: Box<Expr>,
            binary: Option<BinaryOp>,
        },
        Return(Box<Expr>),
        Yield(Box<Expr>),
        Break(Box<Expr>),
        Continue,
    }
}

pub mod stmt {
    pub enum Type {
        Ident,
        Array(Box<Type>, usize),
        Slice(Box<Type>),
        Tuple(Vec<Type>),
        Map(Box<Type>, Box<Type>),
        Option(Box<Type>),
        Result(Box<Type>, Box<Type>),
        Reference(Box<Type>),
        Anon(super::Stmt),
    }

    pub struct Param {
        pub mutable: bool,
        pub name: String,
        pub ty: Type,
    }

    pub enum Stmt {
        Expr(super::Expr),
        Let {
            name: String,
            mutable: bool,
            value: Option<super::Expr>,
        },
        Fn {
            name: String,
            is_async: bool,
            params: Vec<Param>,
            body: Vec<super::Stmt>,
        },
        Struct(Vec<super::Stmt>),
        Interface(Vec<super::Stmt>),
        Enum(Vec<super::Stmt>),
    }
}