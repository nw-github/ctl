mod ast;
mod codegen;
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
use codegen::Codegen;
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

    pub fn display(&self, paths: &[PathBuf]) {
        eprintln!(
            "{}:{}:{}: {}",
            paths[self.span.file].display(),
            self.span.loc.row,
            self.span.loc.col,
            self.diagnostic
        )
    }

    pub fn type_mismatch(expected: &str, received: &str, span: Span) -> Self {
        Self::new(
            format!("type mismatch: expected type '{expected}', got '{received}'"),
            span,
        )
    }

    pub fn private_member(ty: &str, member: &str, span: Span) -> Self {
        Self::new(
            format!("cannot access private member '{member}' of type '{ty}'"),
            span,
        )
    }

    pub fn no_member(ty: &str, member: &str, span: Span) -> Self {
        Self::new(format!("type '{ty}' has no member '{member}'"), span)
    }

    pub fn no_symbol(symbol: &str, span: Span) -> Self {
        Self::new(format!("no symbol '{symbol}' found in this module"), span)
    }

    pub fn no_lang_item(name: &str, span: Span) -> Self {
        Self::new(format!("missing language item: '{name}'"), span)
    }

    pub fn doesnt_implement(ty: &str, trait_name: &str, span: Span) -> Self {
        Self::new(
            format!("type '{ty}' does not implement '{trait_name}'"),
            span,
        )
    }

    pub fn is_unsafe(span: Span) -> Self {
        Self::new("this operation is unsafe", span)
    }
}

pub struct Pipeline<S: CompileState> {
    path: PathBuf,
    state: S,
    files: usize,
}

impl Pipeline<Source> {
    pub fn new(path: PathBuf, files: usize) -> Self {
        Pipeline {
            path,
            state: Source,
            files,
        }
    }

    pub fn parse(self) -> anyhow::Result<Pipeline<Parsed>> {
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
                    sources.push(Parser::parse(&buffer, self.files + sources.len(), path)?);
                }
            }
        } else {
            let buffer = std::fs::read_to_string(&self.path)
                .with_context(|| format!("loading path {}", self.path.display()))?;
            sources.push(Parser::parse(&buffer, self.files, self.path.clone())?);
        }

        Ok(Pipeline {
            path: self.path,
            state: Parsed(sources),
            files: self.files,
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

    pub fn typecheck(self, libs: Vec<PathBuf>) -> anyhow::Result<Pipeline<Checked>> {
        Ok(Pipeline {
            state: Checked(TypeChecker::check(&self.path, self.state.0, libs)?),
            path: self.path,
            files: self.files,
        })
    }
}

impl Pipeline<Checked> {
    pub fn codegen(self) -> std::result::Result<String, (Vec<PathBuf>, Vec<Error>)> {
        let module = self.state.0;
        if module.errors.is_empty() {
            match Codegen::build(module.scope, &module.scopes) {
                Ok(str) => Ok(str),
                Err(err) => Err((module.files, vec![err])),
            }
        } else {
            Err((module.files, module.errors))
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
