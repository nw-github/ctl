use std::collections::HashMap;

use crate::{
    sym::{FunctionId, ScopeId, Scopes},
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

pub enum Dependencies {
    Resolving,
    Resolved(Vec<TypeId>),
    Recursive,
}

#[derive(Default)]
pub struct Project {
    pub scope: ScopeId,
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub hover: Option<LspItem>,
    pub completions: Option<Completions>,
    pub deps: HashMap<TypeId, Dependencies>,
    pub tokens: Vec<SpanSemanticToken>,
    pub main: Option<FunctionId>,
}

impl Project {
    pub fn new(diag: Diagnostics) -> Self {
        Self {
            diag,
            ..Default::default()
        }
    }
}
