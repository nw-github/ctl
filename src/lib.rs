mod ast;
mod codegen;
mod ds;
mod error;
pub mod intern;
mod lexer;
mod lsp;
mod parser;
mod pretty;
mod project;
mod source;
mod sym;
mod typecheck;
mod typeid;
mod format;

use std::path::{Path, PathBuf};

pub use project::Configuration;

use crate::{
    ast::checked::ExprArena as CExprArena,
    ast::parsed::{ExprArena as PExprArena, Stmt, StmtData},
    codegen::Codegen,
    intern::Strings,
    parser::{ModuleAttributes, Parser},
    project::Project,
    typecheck::{LspInput, TypeChecker},
};
use anyhow::{Context, Result};
use indexmap::IndexMap;

pub use error::*;
pub use lexer::*;
pub use lsp::LspBackend;
pub use source::*;

pub trait CompileState {}

pub struct Source<T>(T);
pub struct Parsed {
    modules: Vec<Stmt>,
    diag: Diagnostics,
    conf: Configuration,
    strings: Strings,
    arena: PExprArena,
}
pub struct Checked(Project, CExprArena);

impl<T> CompileState for Source<T> {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

#[derive(serde::Deserialize)]
pub struct ProjectConfig {
    pub root: Option<String>,
    pub name: Option<String>,
    pub build: Option<PathBuf>,
    pub libs: Option<Vec<String>>,

    #[serde(default)]
    pub no_std: bool,
    #[serde(default)]
    pub lib: bool,
}

pub struct Compiler<S: CompileState> {
    state: S,
}

impl Compiler<Source<FileSourceProvider>> {
    pub fn new() -> Compiler<Source<FileSourceProvider>> {
        Self::with_provider(FileSourceProvider)
    }
}

impl<T: SourceProvider> Compiler<Source<T>> {
    pub fn with_provider(provider: T) -> Self {
        Self { state: Source(provider) }
    }

    pub fn parse(mut self, project: UnloadedProject) -> Result<Compiler<Parsed>> {
        let mut diag = Diagnostics::default();
        let mut strings = Strings::new();
        let mut arena = PExprArena::new();
        let mut mods = Vec::with_capacity(project.mods.len());
        for (path, module) in project.mods {
            mods.push(self.load_module(&mut diag, &mut strings, &mut arena, path, module, None)?);
        }

        Ok(Compiler { state: Parsed { modules: mods, diag, conf: project.conf, strings, arena } })
    }

    fn load_module(
        &mut self,
        diag: &mut Diagnostics,
        strings: &mut Strings,
        arena: &mut PExprArena,
        path: PathBuf,
        module: Module,
        attrs: Option<ModuleAttributes>,
    ) -> Result<Stmt> {
        let name = strings.get_or_intern(module.name);
        let mut parsed = self.state.0.get_source(&path, |src| {
            let file_id = diag.add_file(path.clone());
            Parser::parse(src, name, diag, strings, arena, file_id, attrs.unwrap_or_default())
        })?;

        let StmtData::Module { body, .. } = &mut parsed.data.data else {
            unreachable!();
        };

        for (path, module) in module.mods {
            let mod_name = strings.get_or_intern(&module.name);
            let attrs = body.iter_mut().find_map(|stmt| {
                if let StmtData::ModuleOOL { public, name, resolved } = &mut stmt.data.data
                    && name.data == mod_name
                {
                    *resolved = true;
                    Some(ModuleAttributes { attrs: stmt.attrs.clone(), public: *public })
                } else {
                    None
                }
            });

            body.push(self.load_module(diag, strings, arena, path, module, attrs)?);
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
            self.state.diag,
            lsp,
            self.state.conf,
            &self.state.arena,
            self.state.strings,
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

#[derive(Debug, Default, Clone, Copy)]
pub struct CodegenFlags {
    pub no_bit_int: bool,
    pub lib: bool,
    pub minify: bool,
}

/*
    std/
        alloc/
            main.ctl
            vec.ctl
        span.ctl
        main.ctl

    "~/std/main.ctl": Module {
        name: "std",
        mods: {
            "~/std/span.ctl": Module {
                name: "span",
                mods: {}
            },
            "~/std/alloc/main.ctl": Module {
                name: "alloc",
                mods: {
                    "~/std/alloc/vec.ctl": Module {
                        name: "vec",
                        mods: []
                    }
                }
            }
        }
    }
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: String,
    pub mods: IndexMap<PathBuf, Module>,
}

#[derive(Debug, Clone)]
pub struct UnloadedProject {
    pub mods: IndexMap<PathBuf, Module>,
    pub conf: Configuration,
}

impl UnloadedProject {
    pub fn new(path: &Path) -> Result<Self> {
        let mut mods = IndexMap::new();
        let mut conf = Configuration::default();
        let path = path.canonicalize()?;

        let mut modpaths = vec![path];
        let mut main = true;
        while let Some(path) = modpaths.pop() {
            if mods.contains_key(&path) {
                continue;
            }

            match Self::load_module(path.clone(), Some(&mut modpaths), main.then_some(&mut conf))
                .with_context(|| format!("Loading module '{}' failed", path.display()))?
            {
                Some((path, module)) => _ = mods.insert(path, module),
                None => anyhow::bail!("Couldn't find module: '{}'", path.display()),
            }
            main = false;
        }

        // TODO: actual dependency ordering
        mods.reverse();
        Ok(Self { mods, conf })
    }

    fn load_module(
        mut path: PathBuf,
        mods: Option<&mut Vec<PathBuf>>,
        conf: Option<&mut Configuration>,
    ) -> Result<Option<(PathBuf, Module)>> {
        let mut name = Self::derive_module_name(&path);
        if let Some(mods) = mods {
            Self::handle_config(&mut path, &mut name, mods, conf)?;
        }

        if path.is_file() && path.extension().is_some_and(|ext| ext == "ctl") {
            return Ok(Some((path, Module { name, mods: IndexMap::new() })));
        } else if !path.is_dir() {
            return Ok(None);
        }

        let main = path.join("main.ctl");
        if !main.exists() {
            return Ok(None);
        }

        let mut mods = IndexMap::new();
        for entry in path.read_dir().with_context(|| format!("loading path {}", path.display()))? {
            let entry = entry?;
            if entry.file_name() == "main.ctl" {
                continue;
            }

            if let Some((path, module)) = Self::load_module(entry.path(), None, None)
                .with_context(|| format!("loading path {}", path.display()))?
            {
                mods.insert(path, module);
            }
        }

        Ok(Some((main, Module { name, mods })))
    }

    fn handle_config(
        path: &mut PathBuf,
        name: &mut String,
        mods: &mut Vec<PathBuf>,
        conf: Option<&mut Configuration>,
    ) -> Result<()> {
        let mut needs_stdlib = true;
        if path.is_dir() {
            match std::fs::read_to_string(path.join("ctl.toml")) {
                Ok(val) => {
                    let mut config = toml::from_str::<ProjectConfig>(&val)?;
                    if let Some(root) = config.root.take() {
                        *path = Path::new(&root).canonicalize()?;
                    }

                    if let Some(rename) = config.name {
                        // TODO: prevent duplicate names, naming module std, etc.
                        *name = Self::safe_name(&rename);
                    }

                    if config.no_std {
                        needs_stdlib = false;
                    }

                    if let Some(conf) = conf {
                        conf.libs = config.libs;
                        conf.build = config.build;
                        conf.name = Some(name.clone());
                        if config.lib {
                            conf.flags.lib = true;
                        }
                    }
                }
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => return Err(err.into()),
            }
        }

        if needs_stdlib {
            mods.push(Path::new(env!("CARGO_MANIFEST_DIR")).join("std").canonicalize()?);
        }
        Ok(())
    }

    fn derive_module_name(path: &Path) -> String {
        let base = if path.is_file() { path.file_stem() } else { path.file_name() };

        Self::safe_name(base.unwrap().to_string_lossy().as_ref())
    }

    fn safe_name(s: &str) -> String {
        let mut r = String::new();
        for (i, ch) in s.chars().enumerate() {
            if i == 0 && !Lexer::is_identifier_first_char(ch) {
                r.push('_');
            }

            if Lexer::is_identifier_char(ch) {
                r.push(ch);
            } else {
                r.push('_');
            }
        }
        r
    }
}

fn nearest_pow_of_two(bits: u32) -> usize {
    2usize.pow((bits as f64).log2().ceil() as u32).max(8)
}
