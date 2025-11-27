use std::collections::HashSet;

use crate::{
    ast::{Attribute, Attributes},
    dgraph::DependencyGraph,
    sym::{FunctionId, Scopes, UserTypeId, VariableId},
    typecheck::{Completions, LspItem},
    typeid::{TypeId, Types},
    CodegenFlags, Diagnostics, Span,
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
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            features: ["alloc".to_string(), "io".to_string(), "hosted".to_string()].into(),
            flags: Default::default(),
        }
    }
}
