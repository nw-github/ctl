use std::{collections::HashSet, path::PathBuf};

use crate::{
    CodegenFlags, Diagnostics, Span,
    ast::{Attribute, Attributes},
    ds::{DependencyGraph, HashMap},
    format::{FmtTy, FmtUt},
    intern::{StrId, Strings},
    sym::{FunctionId, ScopeId, Scopes, TypeItem, UserTypeId, ValueItem, VariableId, Vis},
    typecheck::{Completions, LspItem},
    typeid::{GenericUserType, TypeId, Types},
};

#[derive(Default)]
pub struct Project {
    pub scopes: Scopes,
    pub types: Types,
    pub diag: Diagnostics,
    pub completions: Option<Completions>,
    pub lsp_items: Option<Vec<(LspItem, Span)>>,
    pub main: Option<FunctionId>,
    pub main_module: Option<ScopeId>,
    pub panic_handler: Option<FunctionId>,
    pub test_runner: Option<FunctionId>,
    pub deps: DependencyGraph<TypeId>,
    pub static_deps: DependencyGraph<VariableId>,
    pub trait_deps: DependencyGraph<UserTypeId>,
    pub conf: Configuration,
    pub strings: Strings,
    pub autouse_tns: HashMap<StrId, Vis<TypeItem>>,
    pub autouse_vns: HashMap<StrId, Vis<ValueItem>>,
}

impl Project {
    pub fn new(conf: Configuration, diag: Diagnostics, strings: Strings, lsp: bool) -> Self {
        Self { diag, conf, strings, lsp_items: lsp.then(Vec::new), ..Default::default() }
    }

    pub fn fmt_ty(&self, ty: TypeId) -> FmtTy<'_> {
        FmtTy::new(ty, self)
    }

    pub fn fmt_ut<'b>(&self, ty: &'b GenericUserType) -> FmtUt<'b, '_> {
        FmtUt::new(ty, self)
    }

    pub fn str(&self, id: StrId) -> &str {
        self.strings.resolve(&id)
    }
}

#[derive(Debug, Clone)]
pub struct TestArgs {
    pub test: Option<String>,
    pub modules: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct Configuration {
    features: HashSet<StrId>,
    pub flags: CodegenFlags,
    pub build: Option<PathBuf>,
    pub libs: Option<Vec<String>>,
    pub name: Option<String>,
    pub no_std: bool,
    pub test_args: Option<TestArgs>,
}

impl Configuration {
    pub fn is_disabled_by_attrs(&self, attrs: &Attributes) -> bool {
        attrs
            .iter()
            .filter(|f| f.name.data.is_str_eq(Strings::ATTR_FEATURE))
            .any(|v| v.props.iter().any(|v| !self.has_attr_features(v)))
    }

    pub fn has_feature(&self, feat: StrId) -> bool {
        self.features.contains(&feat)
    }

    pub fn has_attr_features(&self, attr: &Attribute) -> bool {
        if !attr.name.data.is_str_eq(Strings::ATTR_NOT) || attr.props.is_empty() {
            let Some(name) = attr.name.data.as_str() else {
                return false;
            };
            self.has_feature(*name)
        } else {
            !attr.props.iter().flat_map(|v| v.name.data.as_str()).any(|v| self.has_feature(*v))
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
                Strings::FEAT_OVERFLOW_CHECKS,
                #[cfg(target_os = "linux")]
                Strings::FEAT_BACKTRACE,
            ]
            .into(),
            flags: Default::default(),
            build: None,
            libs: None,
            name: None,
            test_args: None,
            no_std: false,
        }
    }
}
