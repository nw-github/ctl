use std::collections::HashSet;

use crate::{
    ast::{Attribute, Attributes},
    dgraph::DependencyGraph,
    sym::{FunctionId, Scopes, UserTypeId, VariableId},
    typecheck::{Completions, LspItem},
    typeid::{TypeId, Types},
    CodegenFlags, Diagnostics, Span,
};

pub enum SpanSemanticTokenKind {
    Var,
    Variant,
    Type,
}

pub enum SemanticTokenModifiers {
    None,
    Mutable,
}

pub struct SpanSemanticToken {
    pub kind: SpanSemanticTokenKind,
    pub span: Span,
    pub mods: SemanticTokenModifiers,
}

impl SpanSemanticToken {
    pub fn new(kind: SpanSemanticTokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
            mods: SemanticTokenModifiers::None,
        }
    }

    pub fn with_mods(
        kind: SpanSemanticTokenKind,
        span: Span,
        mods: SemanticTokenModifiers,
    ) -> Self {
        Self { kind, span, mods }
    }
}

#[derive(Default)]
pub struct Project {
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub completions: Option<Completions>,
    pub lsp_items: Vec<(LspItem, Span)>,
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

#[derive(Debug, Clone)]
pub struct Configuration {
    features: HashSet<String>,
    pub flags: CodegenFlags,
}

impl Configuration {
    pub fn is_disabled_by_attrs(&self, attrs: &Attributes) -> bool {
        attrs
            .iter()
            .filter(|f| f.name.data == "feature")
            .any(|v| v.props.iter().any(|v| !self.has_attr_features(v)))
    }

    pub fn has_feature(&self, feat: &str) -> bool {
        self.features.contains(&feat.to_lowercase())
    }

    pub fn has_attr_features(&self, attr: &Attribute) -> bool {
        if attr.name.data != "not" || attr.props.is_empty() {
            self.has_feature(&attr.name.data)
        } else {
            !attr.props.iter().any(|v| self.has_feature(&v.name.data))
        }
    }

    pub fn set_feature(&mut self, feat: String) {
        self.features.insert(feat);
    }

    pub fn remove_feature(&mut self, feat: &str) {
        self.features.remove(feat);
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            features: [
                "alloc".to_string(),
                "io".into(),
                "hosted".into(),
                "boehm".into(),
            ]
            .into(),
            flags: Default::default(),
        }
    }
}
