use crate::{
    dgraph::DependencyGraph,
    sym::{FunctionId, ScopeId, Scopes, UserTypeId, VariableId},
    typecheck::{Completions, LspItem},
    typeid::{TypeId, Types},
    Diagnostics, Span,
};

pub enum SpanSemanticToken {
    Variant(Span),
}

impl SpanSemanticToken {
    pub fn span(&self) -> &Span {
        match self {
            SpanSemanticToken::Variant(span) => span,
        }
    }
}

#[derive(Default)]
pub struct Project {
    pub scope: ScopeId,
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub hover: Option<LspItem>,
    pub completions: Option<Completions>,
    pub tokens: Vec<SpanSemanticToken>,
    pub main: Option<FunctionId>,
    pub deps: DependencyGraph<TypeId>,
    pub static_deps: DependencyGraph<VariableId>,
    pub trait_deps: DependencyGraph<UserTypeId>,
}

impl Project {
    pub fn new(diag: Diagnostics) -> Self {
        Self {
            diag,
            ..Default::default()
        }
    }
}
