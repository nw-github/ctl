use std::{collections::HashSet, path::PathBuf};

use crate::{
    CodegenFlags, Diagnostics, Span,
    ast::{Attribute, Attributes},
    dgraph::DependencyGraph,
    intern::{StrId, Strings},
    sym::{FunctionId, Scopes, UserTypeId, VariableId},
    typecheck::{Completions, LspItem},
    typeid::{TypeId, Types},
};

#[derive(Default)]
pub struct Project {
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub completions: Option<Completions>,
    pub lsp_items: Option<Vec<(LspItem, Span)>>,
    pub main: Option<FunctionId>,
    pub panic_handler: Option<FunctionId>,
    pub deps: DependencyGraph<TypeId>,
    pub static_deps: DependencyGraph<VariableId>,
    pub trait_deps: DependencyGraph<UserTypeId>,
    pub conf: Configuration,
    pub strings: Strings,
}

impl Project {
    pub fn new(conf: Configuration, diag: Diagnostics, strings: Strings, lsp: bool) -> Self {
        Self { diag, conf, strings, lsp_items: lsp.then(Vec::new), ..Default::default() }
    }
}

#[derive(Debug, Clone)]
pub struct Configuration {
    features: HashSet<StrId>,
    pub flags: CodegenFlags,
    pub build: Option<PathBuf>,
    pub libs: Option<Vec<String>>,
    pub name: Option<String>,
}

impl Configuration {
    pub fn is_disabled_by_attrs(&self, attrs: &Attributes) -> bool {
        attrs
            .iter()
            .filter(|f| f.name.data == Strings::ATTR_FEATURE)
            .any(|v| v.props.iter().any(|v| !self.has_attr_features(v)))
    }

    pub fn has_feature(&self, feat: StrId) -> bool {
        self.features.contains(&feat)
    }

    pub fn has_attr_features(&self, attr: &Attribute) -> bool {
        if attr.name.data != Strings::ATTR_NOT || attr.props.is_empty() {
            self.has_feature(attr.name.data)
        } else {
            !attr.props.iter().any(|v| self.has_feature(v.name.data))
        }
    }

    pub fn set_feature(&mut self, feat: StrId) {
        self.features.insert(feat);
    }

    pub fn remove_feature(&mut self, feat: StrId) {
        self.features.remove(&feat);
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            features: [
                Strings::FEAT_ALLOC,
                Strings::FEAT_IO,
                Strings::FEAT_HOSTED,
                Strings::FEAT_BOEHM,
            ]
            .into(),
            flags: Default::default(),
            build: None,
            libs: None,
            name: None,
        }
    }
}
