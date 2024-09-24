mod ast;
mod codegen;
mod comptime_int;
mod dgraph;
mod error;
mod lexer;
mod lsp;
mod parser;
mod pretty;
mod project;
mod source;
mod sym;
mod typecheck;
mod typeid;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use ast::parsed::{Stmt, StmtData};
use codegen::Codegen;
pub use error::*;
pub use lexer::*;
use project::Project;
pub use source::*;
use typecheck::LspInput;

use crate::{parser::Parser, typecheck::TypeChecker};

pub use lsp::LspBackend;

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source<T>(T);
pub struct Parsed(Vec<Stmt>, Diagnostics);
pub struct Checked(Project);

impl<T> CompileState for Source<T> {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

#[derive(serde::Deserialize)]
pub struct ProjectConfig {
    pub root: Option<String>,
    pub name: Option<String>,
    pub build: Option<String>,
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
        Self {
            state: Source(provider),
        }
    }

    pub fn parse(mut self, project: Vec<PathBuf>) -> Result<Compiler<Parsed>> {
        let mut diag = Diagnostics::default();
        let project = project
            .into_iter()
            .map(|path| {
                self.load_module(&mut diag, path).and_then(|ast| {
                    ast.context("compile target must be directory or have extension '.ctl'")
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Compiler {
            state: Parsed(project, diag),
        })
    }

    fn load_module(&mut self, diag: &mut Diagnostics, mut path: PathBuf) -> Result<Option<Stmt>> {
        let mut name = Self::derive_module_name(&path);
        if path.is_dir() {
            match std::fs::read_to_string(path.join("ctl.toml")) {
                Ok(val) => {
                    let mut config = toml::from_str::<ProjectConfig>(&val)?;
                    if let Some(root) = config.root.take() {
                        path = PathBuf::from(root);
                    }
                    if let Some(rename) = config.name {
                        // TODO: prevent duplicate names, naming module core/std, etc.
                        name = Self::safe_name(&rename);
                    }
                }
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => return Err(err.into()),
            }

            let mut body = Vec::new();
            let mut main = None;
            for entry in path
                .read_dir()
                .with_context(|| format!("loading path {}", path.display()))?
            {
                let path = entry?.path();
                if let Some(stmt) = self.load_module(diag, path.canonicalize().unwrap_or(path))? {
                    match &stmt.data {
                        StmtData::Module { name, .. } if name.data == "main" => {
                            main = Some(name.span);
                        }
                        _ => {}
                    }

                    body.push(stmt);
                }
            }
            Ok(Some(Stmt {
                data: ast::parsed::StmtData::Module {
                    public: true,
                    file: false,
                    name: lexer::Located::new(main.unwrap_or_default(), name),
                    body,
                },
                attrs: Default::default(),
            }))
        } else {
            self.state.0.get_source(&path, |src| {
                let file_id = diag.add_file(path.clone());
                Parser::parse(src, name, diag, file_id)
            })
        }
    }

    fn derive_module_name(path: &Path) -> String {
        let base = if path.is_file() {
            path.file_stem()
        } else {
            path.file_name()
        };

        Self::safe_name(base.unwrap().to_string_lossy().as_ref())
    }

    fn safe_name(s: &str) -> String {
        s.chars()
            .enumerate()
            .map(|(i, ch)| match (i, ch) {
                (0, ch) if Lexer::is_identifier_first_char(ch) => ch,
                (_, ch) if Lexer::is_identifier_char(ch) => ch,
                _ => '_',
            })
            .collect()
    }
}

impl Default for Compiler<Source<FileSourceProvider>> {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler<Parsed> {
    pub fn dump(&self) {
        if let Some(ast) = self.state.0.last() {
            pretty::print_stmt(ast, 0);
        }
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.state.1
    }

    pub fn typecheck(self, lsp: LspInput) -> Compiler<Checked> {
        Compiler {
            state: Checked(TypeChecker::check(self.state.0, self.state.1, lsp)),
        }
    }
}

impl Compiler<Checked> {
    pub fn build(mut self, flags: CodegenFlags) -> (Option<String>, Diagnostics) {
        if !flags.lib && self.state.0.main.is_none() {
            self.state
                .0
                .diag
                .error(Error::new("no main function found", Span::default()));
        }
        if self.state.0.diag.has_errors() {
            return (None, self.state.0.diag);
        }

        let (code, diag) = Codegen::build(self.state.0, flags);
        (Some(code), diag)
    }

    pub fn project(self) -> Project {
        self.state.0
    }
}

impl<T: CompileState> Compiler<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }
}

#[derive(Default, Clone, Copy)]
pub struct CodegenFlags {
    pub leak: bool,
    pub no_bit_int: bool,
    pub lib: bool,
    pub minify: bool,
}

pub fn project_from_file(
    path: &Path,
    mut libs: Vec<PathBuf>,
    mut no_core: bool,
    mut no_std: bool,
) -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let core_path = root.join("ctl/core");
    let std_path = root.join("ctl/std");
    let path = path.canonicalize().unwrap_or_else(|_| path.into());
    if path == core_path {
        no_core = true;
        no_std = true;
    } else if path == std_path {
        no_std = true;
    }

    if !no_std {
        libs.insert(0, root.join(std_path));
    }

    if !no_core {
        libs.insert(0, root.join(core_path));
    }

    libs.push(path);
    libs
}

fn nearest_pow_of_two(bits: u32) -> usize {
    2usize.pow((bits as f64).log2().ceil() as u32).max(8)
}
