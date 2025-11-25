use std::collections::HashSet;

use crate::{
    ast::Attributes,
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
    pub conf: Configuration,
}

impl Project {
    pub fn new(conf: Configuration, diag: Diagnostics) -> Self {
        Self {
            diag,
            conf,
            ..Default::default()
        }
    }
}

pub struct Configuration {
    features: HashSet<String>,
}

impl Configuration {
    pub fn is_disabled_by_attrs(&self, attrs: &Attributes) -> bool {
        attrs
            .val("feature")
            .is_some_and(|v| !self.features.contains(&v.to_lowercase()))
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            features: [
                "alloc".to_string(),
                "io".to_string(),
                "hosted".to_string(),
            ]
            .into(),
        }
    }
}
