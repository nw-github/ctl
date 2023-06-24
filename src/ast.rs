use crate::lexer::Located;

pub type Expr = Located<expr::Expr>;
pub type Stmt = Located<stmt::Stmt>;

pub mod expr {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum UnaryOp {
        Plus,
        Neg,
        PostIncrement,
        PostDecrement,
        PreIncrement,
        PreDecrement,
        Not,
        Deref,
        Addr,
        AddrMut,
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
            args: Vec<super::Expr>,
        },
        Array(Vec<super::Expr>),
        Tuple(Vec<super::Expr>),
        Map(Vec<(super::Expr, super::Expr)>),
        Literal, /*(???)*/
        Symbol(String),
        Assign {
            target: String,
            binary: Option<BinaryOp>,
            value: Box<super::Expr>,
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
            callee: Box<super::Expr>,
            args: Vec<super::Expr>,
        },
        SubscriptAssign {
            callee: Box<super::Expr>,
            args: Vec<super::Expr>,
            value: Box<super::Expr>,
            binary: Option<BinaryOp>,
        },
        Return(Box<super::Expr>),
        Yield(Box<super::Expr>),
        Break(Box<super::Expr>),
        Continue,
    }
}

pub mod stmt {
    #[derive(Debug)]
    pub enum Type {
        Regular {
            is_dyn: bool,
            name: String,
            type_params: Vec<String>,
        },
        Array(Box<Type>, usize),
        Slice(Box<Type>),
        Tuple(Vec<Type>),
        Map(Box<Type>, Box<Type>),
        Option(Box<Type>),
        Result(Box<Type>, Box<Type>),
        Ref(Box<Type>),
        RefMut(Box<Type>),
        Anon(Box<super::Stmt>),
        Void,
        This,
    }

    #[derive(Debug)]
    pub struct Param {
        pub mutable: bool,
        pub name: String,
        pub ty: Type,
    }

    #[derive(Debug)]
    pub struct FnDecl {
        pub name: String,
        pub is_async: bool,
        pub is_extern: bool,
        pub type_params: Vec<String>,
        pub params: Vec<Param>,
        pub ret: Type,
    }

    #[derive(Debug)]
    pub struct Fn {
        pub header: FnDecl,
        pub body: Vec<super::Stmt>,
    }

    #[derive(Debug)]
    pub struct MemVar {
        pub name: String,
        pub ty: Option<Type>,
        pub value: Option<super::Expr>,
    }

    #[derive(Debug)]
    pub struct Struct {
        pub name: String,
        pub type_params: Vec<String>,
        pub members: Vec<MemVar>,
        pub impls: Vec<String>,
        pub functions: Vec<Fn>,
    }

    #[derive(Debug)]
    pub enum Stmt {
        Expr(super::Expr),
        Let {
            name: String,
            ty: Option<Type>,
            mutable: bool,
            value: Option<super::Expr>,
        },
        Fn(Fn),
        Struct(Struct),
        Union {
            tag: Option<String>,
            base: Struct,
        },
        Interface {
            name: String,
            type_params: Vec<String>,
            impls: Vec<String>,
            functions: Vec<FnDecl>,
        },
        Enum {
            name: String,
            impls: Vec<String>,
            variants: Vec<(String, Option<super::Expr>)>,
            functions: Vec<Fn>,
        },
        Static {
            name: String,
            ty: Option<Type>,
            value: super::Expr,
        },
    }
}
