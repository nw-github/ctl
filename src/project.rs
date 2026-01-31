use std::collections::HashSet;

use crate::{
    Diagnostics, Span,
    ds::{DependencyGraph, HashMap},
    format::{FmtTr, FmtTy, FmtUt, FmtWta},
    intern::{StrId, Strings},
    package::ConstraintArgs,
    sym::{FunctionId, ScopeId, Scopes, TypeItem, ValueItem, VariableId, Vis},
    typecheck::{Completions, LspItem},
    typeid::{GenericTrait, GenericUserType, TypeId, Types, WithTypeArgs},
};

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
    pub conf: Configuration,
    pub strings: Strings,
    pub autouse_tns: HashMap<StrId, Vis<TypeItem>>,
    pub autouse_vns: HashMap<StrId, Vis<ValueItem>>,
}

impl Project {
    pub fn new(conf: Configuration, diag: Diagnostics, strings: Strings, lsp: bool) -> Self {
        Self {
            diag,
            conf,
            strings,
            lsp_items: lsp.then(Vec::new),
            scopes: Default::default(),
            types: Default::default(),
            completions: Default::default(),
            main: Default::default(),
            main_module: Default::default(),
            panic_handler: Default::default(),
            test_runner: Default::default(),
            deps: Default::default(),
            static_deps: Default::default(),
            autouse_tns: Default::default(),
            autouse_vns: Default::default(),
        }
    }

    pub fn fmt_ty(&self, ty: TypeId) -> FmtTy<'_> {
        FmtTy::new(ty, self)
    }

    pub fn fmt_ut<'a>(&self, ty: &'a GenericUserType) -> FmtUt<'a, '_> {
        FmtUt::new(ty, self)
    }

    pub fn fmt_tr<'a>(&self, ty: &'a GenericTrait) -> FmtTr<'a, '_> {
        FmtTr::new(ty, self)
    }

    pub fn dbg_wta<'a, T>(&self, wta: &'a WithTypeArgs<T>) -> FmtWta<'a, '_, T> {
        FmtWta::new(wta, self)
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
    pub build: crate::package::Build,
    pub libs: HashSet<crate::package::Lib>,
    pub name: String,
    pub test_args: Option<TestArgs>,
    pub is_library: bool,
    pub args: ConstraintArgs,
}

impl Configuration {
    pub fn in_test_mode(&self) -> bool {
        self.test_args.is_some()
    }
}
