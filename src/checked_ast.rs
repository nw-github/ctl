use self::stmt::CheckedStmt;

pub type TypeId = usize;
pub type ScopeId = usize;

pub struct Member {
    pub public: bool,
    pub name: String,
    pub ty: TypeId,
}

pub struct ResolvedStruct {
    pub members: Vec<Member>,
}

pub enum ResolvedType {
    Void,
    Never,
    Int(u8),
    Uint(u8),
    F32,
    F64,
    IntGeneric,
    FloatGeneric,
    Struct(ResolvedStruct),
    Union {
        tag: Option<Option<TypeId>>,
        base: ResolvedStruct,
    },
    Enum {},
    Interface {},
    Function {},
}

pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

pub mod expr {
    use crate::ast::expr::{BinaryOp, UnaryOp};

    use super::{stmt::CheckedStmt, TypeId, Block};

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
            args: Vec<(Option<String>, CheckedExpr)>,
        },
        Array(Vec<CheckedExpr>),
        ArrayWithInit {
            init: Box<CheckedExpr>,
            count: Box<CheckedExpr>,
        },
        Tuple(Vec<CheckedExpr>),
        Map(Vec<(CheckedExpr, CheckedExpr)>),
        Bool(bool),
        Integer(u8, String),
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
    }

    pub struct CheckedExpr {
        pub ty: TypeId,
        pub data: ExprData,
    }
}

pub mod stmt {
    use super::{expr::CheckedExpr, TypeId, ScopeId, Block};

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

    pub struct MemVar {
        pub public: bool,
        pub name: String,
        pub ty: TypeId,
        pub value: Option<CheckedExpr>,
    }

    pub struct Struct {
        pub public: bool,
        pub name: String,
        pub type_params: Vec<String>,
        pub members: Vec<MemVar>,
        pub impls: Vec<String>,
        pub functions: Vec<CheckedFn>,
    }

    pub enum UserType {
        Struct(Struct),
        Union {
            tag: Option<String>,
            base: Struct,
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
        }
    }

    pub enum CheckedStmt {
        Expr(CheckedExpr),
        Let {
            name: String,
            ty: TypeId,
            mutable: bool,
            value: Option<CheckedExpr>,
        },
        Fn(CheckedFn),
        UserType(UserType),
        Static {
            public: bool,
            name: String,
            ty: TypeId,
            value: CheckedExpr,
        },
        Module {
            public: bool,
            name: String,
            body: Block,
        },
        Error,
    }
}
