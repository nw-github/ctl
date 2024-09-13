use std::collections::HashMap;

use crate::{
    sym::{FunctionId, ScopeId, Scopes, TypeItem, ValueItem, Vis},
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
    // these should really be a part of Scopes or Typechecker, but it can't be in typechecker or the
    // information would be lost when Codegen calls with_project, and putting it in Scopes causes
    // needless clones in Typechecker::check when resolving autouses.
    pub autouse_tns: HashMap<String, Vis<TypeItem>>,
    pub autouse_vns: HashMap<String, Vis<ValueItem>>,
}

impl Project {
    pub fn new(diag: Diagnostics) -> Self {
        Self {
            diag,
            ..Default::default()
        }
    }
}
