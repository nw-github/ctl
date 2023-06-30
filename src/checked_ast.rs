use crate::scope::ScopeId;

use self::stmt::CheckedStmt;

#[derive(Debug)]
pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

pub mod expr {
    use crate::{
        ast::expr::{BinaryOp, UnaryOp},
        typecheck::TypeId,
    };

    use super::{Block, ScopeId};

    #[derive(Default, Debug)]
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
        MemberCall {
            source: Box<CheckedExpr>,
            member: String,
            ty: TypeId,
            args: Vec<CheckedExpr>,
        },
        Array(Vec<CheckedExpr>),
        ArrayWithInit {
            init: Box<CheckedExpr>,
            count: usize,
        },
        Tuple(Vec<CheckedExpr>),
        Map(Vec<(CheckedExpr, CheckedExpr)>),
        Bool(bool),
        Signed(i128),
        Unsigned(u128),
        Float(String),
        String(String),
        Symbol {
            scope: Option<ScopeId>,
            symbol: String,
        },
        Instance {
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

    #[derive(Debug, Default, derive_more::Constructor)]
    pub struct CheckedExpr {
        pub ty: TypeId,
        pub data: ExprData,
    }
}

pub mod stmt {
    use crate::typecheck::TypeId;

    use super::{expr::CheckedExpr, Block};

    #[derive(Debug)]
    pub struct CheckedParam {
        pub mutable: bool,
        pub keyword: bool,
        pub name: String,
        pub ty: TypeId,
    }

    #[derive(Debug)]
    pub struct CheckedFnDecl {
        pub public: bool,
        pub name: String,
        pub is_async: bool,
        pub is_extern: bool,
        pub type_params: Vec<String>,
        pub params: Vec<CheckedParam>,
        pub ret: TypeId,
    }

    #[derive(Debug)]
    pub struct CheckedFn {
        pub header: CheckedFnDecl,
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct CheckedMemVar {
        pub public: bool,
        pub name: String,
        pub ty: TypeId,
        pub value: Option<CheckedExpr>,
    }

    #[derive(Debug)]
    pub struct CheckedStruct {
        pub public: bool,
        pub name: String,
        pub members: Vec<CheckedMemVar>,
        pub functions: Vec<CheckedFn>,
    }

    #[derive(Debug)]
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

    #[derive(Debug, Default)]
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
            name: String,
            body: Block,
        },
        #[default]
        Error,
    }
}
