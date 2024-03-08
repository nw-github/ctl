mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod pretty;
mod sym;
mod typecheck;
mod typeid;

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use anyhow::Context;
use ast::parsed::Stmt;
use codegen::Codegen;
use error::{Diagnostics, FileId};
use lexer::{Lexer, Span};
use typecheck::Module;

use crate::{parser::Parser, typecheck::TypeChecker};

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source;
pub struct Parsed(Stmt);
pub struct Checked(Module);
pub struct Built(String);

impl CompileState for Source {}
impl CompileState for Parsed {}
impl CompileState for Checked {}
impl CompileState for Built {}

pub struct Compiler<S: CompileState> {
    diag: Diagnostics,
    state: S,
    lsp_file: Option<(FileId, String)>,
}

impl Compiler<Source> {
    pub fn new(lsp_file: Option<(PathBuf, String)>) -> Self {
        let mut diag = Diagnostics::default();
        let lsp_file = lsp_file.map(|(path, data)| (diag.add_file(path), data));
        Self {
            state: Source,
            diag,
            lsp_file,
        }
    }

    pub fn with_diagnostics(diag: Diagnostics) -> Self {
        Compiler {
            diag,
            state: Source,
            lsp_file: None,
        }
    }

    pub fn parse(mut self, path: PathBuf) -> anyhow::Result<Compiler<Parsed>> {
        if let Some(stmt) = self.gather_sources(path.canonicalize().unwrap_or(path))? {
            Ok(Compiler {
                state: Parsed(stmt),
                diag: self.diag,
                lsp_file: self.lsp_file,
            })
        } else {
            Err(anyhow::anyhow!(
                "compile target must be directory or have extension '.ctl'",
            ))
        }
    }

    fn gather_sources(&mut self, path: PathBuf) -> anyhow::Result<Option<Stmt>> {
        if let Some((file_id, data)) = self
            .lsp_file
            .as_ref()
            .filter(|(rhs, _)| path == self.diag.file_path(*rhs))
        {
            Ok(Some(Parser::parse(data, &mut self.diag, *file_id)))
        } else if path.is_dir() {
            let mut body = Vec::new();
            for entry in path
                .read_dir()
                .with_context(|| format!("loading path {}", path.display()))?
            {
                let path = entry?.path();
                if let Some(stmt) = self.gather_sources(path.canonicalize().unwrap_or(path))? {
                    body.push(stmt);
                }
            }
            Ok(Some(Stmt {
                data: ast::parsed::StmtData::Module {
                    public: true,
                    file: false,
                    name: lexer::Located::new(Span::default(), derive_module_name(&path)),
                    body,
                },
                attrs: Default::default(),
            }))
        } else if path.extension() == Some(OsStr::new("ctl")) {
            let buffer = std::fs::read_to_string(&path)
                .with_context(|| format!("loading path {}", path.display()))?;
            let file_id = self.diag.add_file(path);
            Ok(Some(Parser::parse(&buffer, &mut self.diag, file_id)))
        } else {
            Ok(None)
        }
    }
}

impl Compiler<Parsed> {
    pub fn dump(&self) {
        pretty::print_stmt(&self.state.0, 0);
    }

    pub fn typecheck(self, libs: Vec<PathBuf>) -> anyhow::Result<Compiler<Checked>> {
        let (module, diag) = TypeChecker::check(self.state.0, libs, self.diag)?;
        Ok(Compiler {
            state: Checked(module),
            diag,
            lsp_file: self.lsp_file,
        })
    }
}

impl Compiler<Checked> {
    pub fn build(mut self, flags: CodegenFlags) -> Result<Compiler<Built>, Self> {
        if let Ok(code) = Codegen::build(
            &mut self.diag,
            self.state.0.scope,
            &self.state.0.scopes,
            flags,
        ) {
            Ok(Compiler {
                state: Built(code),
                diag: self.diag,
                lsp_file: self.lsp_file,
            })
        } else {
            Err(self)
        }
    }
}

impl Compiler<Built> {
    pub fn code(self) -> String {
        self.state.0
    }
}

impl<T: CompileState> Compiler<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diag
    }

    pub fn lsp_file(&self) -> Option<&(FileId, String)> {
        self.lsp_file.as_ref()
    }
}

pub struct CodegenFlags {
    pub leak: bool,
    pub no_bit_int: bool,
    pub lib: bool,
    pub minify: bool,
}

pub(crate) fn derive_module_name(path: &Path) -> String {
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

fn nearest_pow_of_two(bits: u32) -> usize {
    2usize.pow((bits as f64).log2().ceil() as u32).max(8)
}
