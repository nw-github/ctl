use enum_as_inner::EnumAsInner;

use crate::{
    ast::parsed::{Expr, Pattern, TypeHint},
    lexer::{Located, Span},
    sym::{ExtensionId, FunctionId, ScopeId, TraitId, UserTypeId, VariableId},
};

pub struct Fn {
    pub id: FunctionId,
    pub body: Option<Expr>,
}

pub struct ImplBlock {
    pub span: Span,
    pub scope: ScopeId,
    pub fns: Vec<Fn>,
    pub type_params: Vec<UserTypeId>,
}

#[derive(EnumAsInner)]
pub enum Stmt {
    Expr(Expr),
    Defer(Expr),
    Guard { cond: Expr, body: Expr },
    Let { patt: Located<Pattern>, ty: Option<TypeHint>, value: Option<Expr> },
    Binding { id: VariableId, value: Option<Expr>, constant: bool },
    Fn(Fn),
    Struct { id: UserTypeId, impls: Vec<ImplBlock>, fns: Vec<Fn>, init: Fn },
    Union { id: UserTypeId, impls: Vec<ImplBlock>, fns: Vec<Fn> },
    Trait { id: TraitId, fns: Vec<Fn> },
    Extension { id: ExtensionId, impls: Vec<ImplBlock>, fns: Vec<Fn> },
    Module { id: ScopeId, body: Vec<Stmt> },
    None,
}
