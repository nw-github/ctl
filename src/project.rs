use crate::{
    sym::{ScopeId, Scopes},
    typecheck::{Completions, LspItem},
    typeid::Types,
    Diagnostics,
};

#[derive(Default)]
pub struct Project {
    pub scope: ScopeId,
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub hover: Option<LspItem>,
    pub completions: Option<Completions>,
}

impl Project {
    pub fn new(diag: Diagnostics) -> Self {
        Self {
            diag,
            ..Default::default()
        }
    }
}
