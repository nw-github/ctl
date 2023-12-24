use crate::{
    ast::parsed::{Expr, Path, Pattern, TypeHint},
    ast::Attribute,
    lexer::Span,
    typecheck::{FunctionId, ScopeId, UserTypeId, VariableId},
};

pub struct DeclaredStmt {
    pub data: DeclaredStmtData,
    pub span: Span,
    pub attrs: Vec<Attribute>,
}

pub struct DeclaredFn {
    pub id: FunctionId,
    pub body: Option<Vec<DeclaredStmt>>,
}

pub struct DeclaredImplBlock {
    pub impl_index: usize,
    pub span: Span,
    pub scope: ScopeId,
    pub functions: Vec<DeclaredFn>,
}

pub enum DeclaredStmtData {
    Expr(Expr),
    Use {
        public: bool,
        path: Path,
        all: bool,
    },
    Let {
        patt: Pattern,
        ty: Option<TypeHint>,
        mutable: bool,
        value: Option<Expr>,
    },
    Fn(DeclaredFn),
    Struct {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        functions: Vec<DeclaredFn>,
        init: DeclaredFn,
    },
    Union {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        functions: Vec<DeclaredFn>,
        member_cons: Vec<DeclaredFn>,
    },
    Trait {
        id: UserTypeId,
        functions: Vec<DeclaredFn>,
    },
    Enum {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        variants: Vec<(String, Option<Expr>)>,
        functions: Vec<DeclaredFn>,
    },
    Static {
        id: VariableId,
        value: Expr,
    },
    Module {
        id: ScopeId,
        body: Vec<DeclaredStmt>,
    },
    Error,
}