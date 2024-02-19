use crate::{
    ast::parsed::{Expr, Pattern, TypeHint},
    lexer::{Located, Span},
    sym::{ExtensionId, FunctionId, ScopeId, TraitId, UserTypeId, VariableId},
};

pub struct DeclaredFn {
    pub public: bool,
    pub id: FunctionId,
    pub body: Option<Vec<DeclaredStmt>>,
}

pub struct DeclaredImplBlock {
    pub impl_index: usize,
    pub span: Span,
    pub scope: ScopeId,
    pub fns: Vec<DeclaredFn>,
}

pub enum DeclaredStmt {
    Expr(Expr),
    Let {
        patt: Located<Pattern>,
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Static {
        id: VariableId,
        value: Expr,
    },
    Fn(DeclaredFn),
    Struct {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        fns: Vec<DeclaredFn>,
        init: DeclaredFn,
    },
    Union {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        fns: Vec<DeclaredFn>,
    },
    Trait {
        id: TraitId,
        fns: Vec<DeclaredFn>,
    },
    Extension {
        id: ExtensionId,
        impls: Vec<DeclaredImplBlock>,
        fns: Vec<DeclaredFn>,
    },
    Module {
        id: ScopeId,
        body: Vec<DeclaredStmt>,
    },
    None,
}
