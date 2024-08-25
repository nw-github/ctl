use std::collections::HashMap;

use crate::{
    sym::{ScopeId, Scopes},
    typecheck::{Completions, LspItem},
    typeid::{TypeId, Types},
    Diagnostics,
};

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
}

impl Project {
    pub fn new(diag: Diagnostics) -> Self {
        Self {
            diag,
            ..Default::default()
        }
    }
}
