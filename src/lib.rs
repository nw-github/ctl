mod ast;
mod codegen;
mod error;
mod lexer;
mod lsp;
mod parser;
mod pretty;
mod source;
mod sym;
mod typecheck;
mod typeid;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use ast::parsed::Stmt;
use codegen::Codegen;
pub use error::*;
use lexer::{Lexer, Span};
pub use source::*;
use typecheck::{LspInput, Project};

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
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Compiler {
            state: Parsed(project, diag),
        })
    }

    fn load_module(&mut self, diag: &mut Diagnostics, path: PathBuf) -> Result<Option<Stmt>> {
        if path.is_dir() {
            let mut body = Vec::new();
            for entry in path
                .read_dir()
                .with_context(|| format!("loading path {}", path.display()))?
            {
                let path = entry?.path();
                if let Some(stmt) =
                    self.load_module(diag, path.canonicalize().unwrap_or(path))?
                {
                    body.push(stmt);
                }
            }
            Ok(Some(Stmt {
                data: ast::parsed::StmtData::Module {
                    public: true,
                    file: false,
                    name: lexer::Located::new(Span::default(), Self::derive_module_name(&path)),
                    body,
                },
                attrs: Default::default(),
            }))
        } else {
            let name = Self::derive_module_name(&path);
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

        base.unwrap()
            .to_string_lossy()
            .chars()
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
    pub fn build(mut self, flags: CodegenFlags) -> Result<(Diagnostics, String), Diagnostics> {
        if let Ok(code) = Codegen::build(&mut self.state.0, flags) {
            Ok((self.state.0.diag, code))
        } else {
            Err(self.state.0.diag)
        }
    }

    pub fn project(&self) -> &Project {
        &self.state.0
    }
}

impl<T: CompileState> Compiler<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }
}

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
