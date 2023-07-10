mod ast;
mod checked_ast;
mod compiler;
mod lexer;
mod parser;
mod pretty;
mod typecheck;

use std::{path::{Path, PathBuf}, ffi::OsStr};

use compiler::Compiler;
use lexer::{Lexer, Span};
use parser::ParsedFile;
use typecheck::Module;

use crate::{parser::Parser, typecheck::TypeChecker};

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source;
pub struct Parsed(Vec<ParsedFile>);
pub struct Checked(Module);

impl CompileState for Source {}
impl CompileState for Parsed {}
impl CompileState for Checked {}

#[derive(Debug)]
pub struct Error {
    diagnostic: String,
    span: Span,
}

impl Error {
    pub fn new(diagnostic: impl Into<String>, span: impl Into<Span>) -> Self {
        Self {
            diagnostic: diagnostic.into(),
            span: span.into(),
        }
    }

    pub fn display(&self, file: &str) {
        eprintln!(
            "{file}:{}:{}: {}",
            self.span.loc.row, self.span.loc.col, self.diagnostic
        )
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Pipeline<S: CompileState> {
    path: PathBuf,
    state: S,
}

impl Pipeline<Source> {
    pub fn new(path: PathBuf) -> Self {
        Pipeline {
            path,
            state: Source,
        }
    }

    pub fn parse(self) -> std::io::Result<Pipeline<Parsed>> {
        let mut sources = Vec::new();
        if self.path.is_dir() {
            for file in self.path.read_dir()? {
                let file = file?;
                let path = file.path();
                if file.file_type()?.is_file() && path.extension() == Some(OsStr::new("ctl")) {
                    let buffer = std::fs::read_to_string(&path)?;
                    sources.push(Parser::parse(&buffer, path)?);
                }
            }
        } else {
            let buffer = std::fs::read_to_string(&self.path)?;
            sources.push(Parser::parse(&buffer, self.path.clone())?);
        }

        Ok(Pipeline {
            path: self.path,
            state: Parsed(sources),
        })
    }
}

impl Pipeline<Parsed> {
    pub fn dump(&self) {
        for source in self.state.0.iter() {
            let buffer = std::fs::read_to_string(&source.path).unwrap();
            pretty::print_stmt(&source.ast, &buffer, 0);
        }
    }

    pub fn typecheck(self) -> Pipeline<Checked> {
        Pipeline {
            state: Checked(TypeChecker::check(&self.path, self.state.0)),
            path: self.path,
        }
    }
}

impl Pipeline<Checked> {
    pub fn codegen(self) -> std::result::Result<String, Vec<(PathBuf, Vec<Error>)>> {
        let module = self.state.0;
        if module.errors.is_empty() {
            match Compiler::compile(module.scope, &module.scopes) {
                Ok(str) => Ok(str),
                Err(_) => todo!(),
            }
        } else {
            Err(module.errors)
        }
    }
}

impl<T: CompileState> Pipeline<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }
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
