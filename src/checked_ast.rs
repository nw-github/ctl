use self::stmt::CheckedStmt;

pub type ScopeId = usize;

pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

pub mod expr {
    use crate::{
        ast::expr::{BinaryOp, UnaryOp},
        typecheck::TypeId,
    };

    use super::Block;

    #[derive(Default)]
    pub enum ExprData {
        Binary {
            op: BinaryOp,
            left: Box<CheckedExpr>,
            right: Box<CheckedExpr>,
        },
        Unary {
            op: UnaryOp,
            expr: Box<CheckedExpr>,
        },
        Call {
            callee: Box<CheckedExpr>,
            args: Vec<CheckedExpr>,
        },
        Array(Vec<CheckedExpr>),
        ArrayWithInit {
            init: Box<CheckedExpr>,
            count: Box<CheckedExpr>,
        },
        Tuple(Vec<CheckedExpr>),
        Map(Vec<(CheckedExpr, CheckedExpr)>),
        Bool(bool),
        Signed(i128),
        Unsigned(u128),
        Float(String),
        String(String),
        Symbol(String),
        Instance {
            name: String,
            members: Vec<(String, CheckedExpr)>,
        },
        None,
        Assign {
            target: Box<CheckedExpr>,
            binary: Option<BinaryOp>,
            value: Box<CheckedExpr>,
        },
        Block(Block),
        If {
            cond: Box<CheckedExpr>,
            if_branch: Box<CheckedExpr>,
            else_branch: Option<Box<CheckedExpr>>,
        },
        Loop {
            cond: Box<CheckedExpr>,
            body: Block,
            do_while: bool,
        },
        For {
            var: String,
            iter: Box<CheckedExpr>,
            body: Block,
        },
        Member {
            source: Box<CheckedExpr>,
            member: String,
        },
        Subscript {
            callee: Box<CheckedExpr>,
            args: Vec<CheckedExpr>,
        },
        Return(Box<CheckedExpr>),
        Yield(Box<CheckedExpr>),
        Break(Box<CheckedExpr>),
        Range {
            start: Option<Box<CheckedExpr>>,
            end: Option<Box<CheckedExpr>>,
            inclusive: bool,
        },
        Continue,
        #[default]
        Error,
    }

    #[derive(Default, derive_more::Constructor)]
    pub struct CheckedExpr {
        pub ty: TypeId,
        pub data: ExprData,
    }
}

pub mod stmt {
    use crate::typecheck::TypeId;

    use super::{expr::CheckedExpr, Block};

    pub struct CheckedParam {
        pub mutable: bool,
        pub keyword: bool,
        pub name: String,
        pub ty: TypeId,
    }

    pub struct CheckedFnDecl {
        pub public: bool,
        pub name: String,
        pub is_async: bool,
        pub is_extern: bool,
        pub type_params: Vec<String>,
        pub params: Vec<CheckedParam>,
        pub ret: TypeId,
    }

    pub struct CheckedFn {
        pub header: CheckedFnDecl,
        pub body: Block,
    }

    pub struct CheckedMemVar {
        pub public: bool,
        pub name: String,
        pub ty: TypeId,
        pub value: Option<CheckedExpr>,
    }

    pub struct CheckedStruct {
        pub public: bool,
        pub name: String,
        pub members: Vec<CheckedMemVar>,
        pub functions: Vec<CheckedFn>,
    }

    pub enum CheckedUserType {
        Struct(CheckedStruct),
        Union {
            tag: Option<String>,
            base: CheckedStruct,
        },
        Interface {
            public: bool,
            name: String,
            type_params: Vec<String>,
            impls: Vec<String>,
            functions: Vec<CheckedFnDecl>,
        },
        Enum {
            public: bool,
            name: String,
            impls: Vec<String>,
            variants: Vec<(String, Option<CheckedExpr>)>,
            functions: Vec<CheckedFn>,
        },
    }

    #[derive(Default)]
    pub enum CheckedStmt {
        Expr(CheckedExpr),
        Let {
            name: String,
            mutable: bool,
            value: Result<CheckedExpr, TypeId>,
        },
        Fn(CheckedFn),
        UserType(CheckedUserType),
        Static {
            public: bool,
            name: String,
            value: CheckedExpr,
        },
        Module {
            public: bool,
            name: String,
            body: Block,
        },
        #[default]
        Error,
    }
}
