mod ast;
mod codegen;
mod ds;
mod error;
mod format;
mod intern;
mod lexer;
mod lsp;
pub mod package;
mod parser;
mod pretty;
mod project;
mod source;
mod sym;
mod typecheck;
mod typeid;
mod utils;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub use intern::Strings;
pub use project::{Configuration, TestArgs};

use crate::package::Input;
use crate::{
    ast::checked::ExprArena as CExprArena,
    ast::parsed::{ExprArena as PExprArena, Stmt, StmtData},
    codegen::Codegen,
    parser::{ModuleAttributes, Parser},
    project::Project,
    typecheck::{LspInput, TypeChecker},
};
use anyhow::{Context, Result};

pub use error::*;
pub use lexer::*;
pub use lsp::LspBackend;
pub use source::*;

pub trait CompileState {}

pub struct Source<T>(T);
pub struct Parsed {
    modules: Vec<Stmt>,
    feature_sets: Vec<FeatureSet>,
    diag: Diagnostics,
    conf: Configuration,
    strings: Strings,
    arena: PExprArena,
}
pub struct Checked(Project, CExprArena);

impl<T> CompileState for Source<T> {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

pub struct Compiler<S: CompileState> {
    state: S,
}

impl Compiler<Source<FileSourceProvider>> {
    pub fn new() -> Self {
        Self::with_provider(FileSourceProvider)
    }
}

impl<T: SourceProvider> Compiler<Source<T>> {
    pub fn with_provider(provider: T) -> Self {
        Self { state: Source(provider) }
    }

    pub fn parse_project(mut self, proj: package::Project) -> Result<Compiler<Parsed>> {
        let (packages, conf) = load_packages(proj)?;
        let mut diag = Diagnostics::default();
        let mut strings = Strings::new();
        let mut arena = PExprArena::new();
        let mut modules = Vec::with_capacity(packages.len());
        let mut feature_sets = Vec::with_capacity(packages.len());
        for package in packages {
            let ast =
                self.parse_module(&mut diag, &mut strings, &mut arena, package.module, None)?;
            modules.push(ast);
            feature_sets.push(package.feature_set);
        }

        Ok(Compiler { state: Parsed { modules, diag, feature_sets, conf, strings, arena } })
    }

    pub fn parse(self, path: &Path, input: Input) -> Result<Compiler<Parsed>> {
        self.parse_project(package::Project::load(path, input)?)
    }

    fn parse_module(
        &mut self,
        diag: &mut Diagnostics,
        strings: &mut Strings,
        arena: &mut PExprArena,
        module: Module,
        attrs: Option<ModuleAttributes>,
    ) -> Result<Stmt> {
        let name = strings.get_or_intern(module.name);
        let mut parsed = self.state.0.get_source(&module.path, |src| {
            let file_id = diag.add_file(module.path.clone());
            Parser::parse(src, name, diag, strings, arena, file_id, attrs.unwrap_or_default())
        })?;

        let StmtData::Module { body, .. } = &mut parsed.data.data else {
            unreachable!();
        };

        for module in module.mods {
            let mod_name = strings.get_or_intern(&module.name);
            let attrs = body.iter_mut().find_map(|stmt| {
                if let StmtData::ModuleOOL { vis, name, ref mut resolved } = stmt.data.data
                    && name.data == mod_name
                {
                    *resolved = true;
                    Some(ModuleAttributes { attrs: stmt.attrs.clone(), vis })
                } else {
                    None
                }
            });

            body.push(self.parse_module(diag, strings, arena, module, attrs)?);
        }

        Ok(parsed)
    }
}

impl Default for Compiler<Source<FileSourceProvider>> {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler<Parsed> {
    pub fn modify_conf(mut self, f: impl FnOnce(&mut Configuration)) -> Self {
        f(&mut self.state.conf);
        self
    }

    pub fn dump(&self, mods: &[String]) {
        if mods.is_empty()
            && let Some(ast) = self.state.modules.last()
        {
            pretty::print_stmt(ast, &self.state.strings, &self.state.arena, 0);
        }

        for module in mods {
            if let Some(stmt) = self.find_module(module) {
                eprintln!("Module {module}:");
                pretty::print_stmt(stmt, &self.state.strings, &self.state.arena, 0);
            } else {
                eprintln!("Couldn't find module: '{module}'");
            }
        }
    }

    pub fn find_module(&self, module: &str) -> Option<&Stmt> {
        // std::alloc::vec
        let (mut stmt, mut body) = (None, &self.state.modules);
        for submod in module.split("::") {
            let res = body.iter().find_map(|stmt| {
                if let StmtData::Module { name, body, .. } = &stmt.data.data
                    && self.state.strings.resolve(&name.data) == submod
                {
                    Some((stmt, body))
                } else {
                    None
                }
            })?;
            stmt = Some(res.0);
            body = res.1;
        }

        stmt
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.state.diag
    }

    pub fn typecheck(self, lsp: Option<LspInput>) -> Compiler<Checked> {
        let (proj, arena) = TypeChecker::check(
            self.state.modules,
            self.state.feature_sets,
            Project::new(self.state.conf, self.state.diag, self.state.strings, lsp.is_some()),
            lsp,
            self.state.arena,
        );
        Compiler { state: Checked(proj, arena) }
    }
}

impl Compiler<Checked> {
    pub fn build(self) -> (Option<String>, Configuration, Diagnostics) {
        let proj = self.state.0;
        if proj.diag.has_errors() {
            return (None, proj.conf, proj.diag);
        }

        let code = Codegen::build(&proj, self.state.1);
        (Some(code), proj.conf, proj.diag)
    }

    pub fn project(self) -> Project {
        self.state.0
    }
}

pub type FeatureSet = HashMap<String, bool>;

#[derive(Debug, Clone)]
struct Module {
    name: String,
    path: PathBuf,
    mods: Vec<Module>,
}

#[derive(Debug, Clone)]
struct Package {
    module: Module,
    feature_set: FeatureSet,
}

fn load_packages(proj: package::Project) -> Result<(Vec<Package>, Configuration)> {
    let mut packages = Vec::new();
    let main_module = proj.mods.last().unwrap();
    let is_library = main_module.lib;
    let name = safe_name(&main_module.name);
    for package in proj.mods {
        match load_module(&package.name, &package.path).with_context(|| {
            format!("Loading module '{}' failed (root '{}')", package.name, package.path.display())
        })? {
            Some(module) => packages.push(Package {
                module,
                feature_set: package
                    .features
                    .into_iter()
                    .map(|(name, feat)| (name, feat.enabled))
                    .collect(),
            }),
            None => anyhow::bail!("Couldn't load package: '{}'", package.path.display()),
        }
    }

    Ok((
        packages,
        Configuration {
            build: proj.build,
            libs: proj.libs,
            args: proj.args,
            name,
            is_library,
            test_args: None,
        },
    ))
}

fn load_module(name: &str, path: &Path) -> Result<Option<Module>> {
    let name = safe_name(name);
    if path.is_file() && path.extension().is_some_and(|ext| ext == "ctl") {
        return Ok(Some(Module { path: path.to_path_buf(), name, mods: Vec::new() }));
    } else if !path.is_dir() {
        return Ok(None);
    }

    let main = path.join("main.ctl");
    if !main.exists() {
        return Ok(None);
    }

    let mut mods = Vec::new();
    for entry in path.read_dir().with_context(|| format!("loading path {}", path.display()))? {
        let entry = entry?;
        if entry.file_name() == "main.ctl" {
            continue;
        }

        let path = entry.path();
        let mod_name = package::Project::default_package_name(&path)?;
        if let Some(module) = load_module(&safe_name(&mod_name), &path)
            .with_context(|| format!("loading path {}", path.display()))?
        {
            mods.push(module);
        }
    }

    Ok(Some(Module { path: main, name, mods }))
}

fn safe_name(s: &str) -> String {
    let mut r = String::new();
    for (i, ch) in s.chars().enumerate() {
        if i == 0 && !Lexer::is_identifier_first_char(ch) {
            r.push('_');
        }

        r.push(if Lexer::is_identifier_char(ch) { ch } else { '_' });
    }

    if Lexer::make_reserved_ident(&r).is_some() {
        r.insert(0, '_');
    }

    r
}
