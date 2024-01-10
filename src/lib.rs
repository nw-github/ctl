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
use lexer::Lexer;
use typecheck::Module;

use crate::{parser::Parser, typecheck::TypeChecker};

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source;
pub struct Parsed(Vec<Stmt>);
pub struct Checked(Module);

impl CompileState for Source {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

pub struct Pipeline<S: CompileState> {
    path: PathBuf,
    diag: Diagnostics,
    state: S,
}

impl Pipeline<Source> {
    pub fn new(path: PathBuf, diag: Diagnostics) -> Self {
        Pipeline {
            path,
            diag,
            state: Source,
        }
    }

    pub fn parse(mut self) -> anyhow::Result<Pipeline<Parsed>> {
        let mut sources = Vec::new();
        if self.path.is_dir() {
            for file in self
                .path
                .read_dir()
                .with_context(|| format!("loading path {}", self.path.display()))?
            {
                let file = file?;
                let path = file.path();
                if file.file_type()?.is_file() && path.extension() == Some(OsStr::new("ctl")) {
                    let buffer = std::fs::read_to_string(&path)?;
                    sources.push(Parser::parse(&buffer, &mut self.diag, path));
                }
            }
        } else {
            let buffer = std::fs::read_to_string(&self.path)
                .with_context(|| format!("loading path {}", self.path.display()))?;
            sources.push(Parser::parse(&buffer, &mut self.diag, self.path.clone()));
        }

        Ok(Pipeline {
            path: self.path,
            state: Parsed(sources),
            diag: self.diag,
        })
    }
}

impl Pipeline<Parsed> {
    pub fn dump(&self) {
        for source in self.state.0.iter() {
            pretty::print_stmt(source, 0);
        }
    }

    pub fn typecheck(self, libs: Vec<PathBuf>) -> anyhow::Result<Pipeline<Checked>> {
        let (module, diag) = TypeChecker::check(&self.path, self.state.0, libs, self.diag)?;
        Ok(Pipeline {
            state: Checked(module),
            path: self.path,
            diag,
        })
    }
}

impl Pipeline<Checked> {
    pub fn codegen(mut self) -> Result<String, Diagnostics> {
        let module = self.state.0;
        if !self.diag.has_errors() {
            match Codegen::build(module.scope, &module.scopes) {
                Ok(str) => return Ok(str),
                Err(err) => self.diag.error(err),
            }
        }

        Err(self.diag)
    }
}

impl<T: CompileState> Pipeline<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }
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
