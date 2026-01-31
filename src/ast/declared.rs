use crate::{
    ast::parsed::{Expr, Pattern, TypeHint, UsePathComponent},
    intern::StrId,
    lexer::Located,
    sym::{AliasId, ExtensionId, FunctionId, ScopeId, TraitId, UserTypeId, VariableId},
};

pub struct Fn {
    pub id: FunctionId,
    pub body: Option<Expr>,
}

pub enum Stmt {
    Expr(Expr),
    Defer(Expr),
    Guard { cond: Expr, body: Expr },
    Let { patt: Located<Pattern>, ty: Option<TypeHint>, value: Option<Expr> },
    Binding { id: VariableId, value: Option<Expr> },
    Fn(Fn),
    UserType { id: UserTypeId, fns: Vec<Fn> },
    Extension { id: ExtensionId, fns: Vec<Fn> },
    Trait { id: TraitId, fns: Vec<Fn> },
    Alias { id: AliasId },
    Module { id: ScopeId, body: Vec<Stmt> },
    ModuleOOL { name: Located<StrId> },
}

pub struct UsePath {
    pub public: bool,
    pub in_type: bool,
    pub scope: Option<ScopeId>,
    pub comp: UsePathComponent,
}
