mod ast;
mod checked_ast;
mod compiler;
mod lexer;
mod parser;
mod pretty;
mod typecheck;
mod scope;

use std::path::PathBuf;

use compiler::Compiler;
use lexer::Span;
use typecheck::CheckedAst;

use crate::{ast::Stmt, parser::Parser, typecheck::TypeChecker};

pub trait CompileState {}

pub struct Source<'a>(&'a str);
pub struct Ast(Stmt);
pub struct Checked(CheckedAst);

impl CompileState for Source<'_> {}
impl CompileState for Ast {}
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
    errors: Vec<Error>,
    file: PathBuf,
    state: S,
}

impl<'a> Pipeline<Source<'a>> {
    pub fn new(source: &'a str, file: PathBuf) -> Self {
        Pipeline {
            errors: Vec::new(),
            file,
            state: Source(source),
        }
    }

    pub fn parse(self) -> Pipeline<Ast> {
        let (ast, errors) = Parser::parse(self.state.0);
        Pipeline {
            errors,
            file: self.file,
            state: Ast(ast),
        }
    }
}

impl Pipeline<Ast> {
    pub fn dump(&self) {
        pretty::print_stmt(&self.state.0, 0);
    }

    pub fn typecheck(mut self) -> Pipeline<Checked> {
        let (checked, errors) = TypeChecker::check(self.state.0);
        self.errors.extend(errors);
        Pipeline {
            errors: self.errors,
            file: self.file,
            state: Checked(checked),
        }
    }
}

impl Pipeline<Checked> {
    pub fn codegen(self) -> std::result::Result<String, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(Compiler::compile(self.state.0))
        } else {
            Err(self.errors)
        }
    }
}

impl<T: CompileState> Pipeline<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }
}
