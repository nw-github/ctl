use enum_as_inner::EnumAsInner;

use crate::{
    ast::parsed::{Expr, Pattern, TypeHint},
    lexer::{Located, Span},
    sym::{ExtensionId, FunctionId, ScopeId, TraitId, UserTypeId, VariableId},
};

pub struct DeclaredFn {
    pub id: FunctionId,
    pub body: Option<Expr>,
}

pub struct DeclaredImplBlock {
    pub span: Span,
    pub scope: ScopeId,
    pub fns: Vec<DeclaredFn>,
}

#[derive(EnumAsInner)]
pub enum DeclaredStmt {
    Expr(Expr),
    Defer(Expr),
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
