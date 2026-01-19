use crate::{
    ast::parsed::{Expr, Pattern, TypeHint, UsePathComponent},
    intern::StrId,
    lexer::{Located, Span},
    sym::{AliasId, ExtensionId, FunctionId, ScopeId, TraitId, UserTypeId, VariableId},
};

pub struct Fn {
    pub id: FunctionId,
    pub body: Option<Expr>,
}

pub struct ImplBlock {
    pub span: Span,
    pub scope: ScopeId,
    pub fns: Vec<Fn>,
}

pub enum Stmt {
    Expr(Expr),
    Defer(Expr),
    Guard { cond: Expr, body: Expr },
    Let { patt: Located<Pattern>, ty: Option<TypeHint>, value: Option<Expr> },
    Binding { id: VariableId, value: Option<Expr> },
    Fn(Fn),
    Struct { id: UserTypeId, impls: Vec<ImplBlock>, fns: Vec<Fn>, init: Fn },
    Union { id: UserTypeId, impls: Vec<ImplBlock>, fns: Vec<Fn> },
    Trait { id: TraitId, fns: Vec<Fn> },
    Alias { id: AliasId },
    Extension { id: ExtensionId, impls: Vec<ImplBlock>, fns: Vec<Fn> },
    Module { id: ScopeId, body: Vec<Stmt> },
    ModuleOOL { name: Located<StrId> },
}

pub struct UsePath {
    pub public: bool,
    pub in_type: bool,
    pub scope: Option<ScopeId>,
    pub comp: UsePathComponent,
}
