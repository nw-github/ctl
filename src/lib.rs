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
use error::Diagnostics;
use lexer::{Lexer, Span};
use typecheck::Module;

use crate::{parser::Parser, typecheck::TypeChecker};

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source;
pub struct Parsed(Stmt);
pub struct Checked(Module);

impl CompileState for Source {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

pub struct Compiler<S: CompileState> {
    diag: Diagnostics,
    state: S,
}

impl Compiler<Source> {
    pub fn new(diag: Diagnostics) -> Self {
        Compiler {
            diag,
            state: Source,
        }
    }

    pub fn parse(mut self, path: PathBuf) -> anyhow::Result<Compiler<Parsed>> {
        fn gather_sources(path: PathBuf, diag: &mut Diagnostics) -> anyhow::Result<Option<Stmt>> {
            if path.is_dir() {
                let mut body = Vec::new();
                for entry in path
                    .read_dir()
                    .with_context(|| format!("loading path {}", path.display()))?
                {
                    if let Some(stmt) = gather_sources(entry?.path(), diag)? {
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
                let file_id = diag.add_file(path);
                Ok(Some(Parser::parse(&buffer, diag, file_id)))
            } else {
                Ok(None)
            }
        }

        if let Some(stmt) = gather_sources(path, &mut self.diag)? {
            Ok(Compiler {
                state: Parsed(stmt),
                diag: self.diag,
            })
        } else {
            Err(anyhow::anyhow!(
                "compile target must be directory or have extension '.ctl'",
            ))
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
        })
    }
}

impl Compiler<Checked> {
    pub fn build(self, flags: CodegenFlags) -> Result<(Diagnostics, String), Diagnostics> {
        Codegen::build(self.diag, self.state.0.scope, &self.state.0.scopes, flags)
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

pub(crate) fn derive_module_name(path: &Path) -> String {
    let path = path.canonicalize().unwrap();
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
