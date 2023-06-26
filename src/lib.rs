mod ast;
mod checked_ast;
mod compiler;
mod lexer;
mod parser;
mod pretty;
mod typecheck;

use std::{error::Error, path::PathBuf};

use compiler::Compiler;
use typecheck::CheckedAst;

use crate::{
    ast::Stmt, lexer::Located, parser::Parser,
    typecheck::TypeChecker,
};

pub trait CompileState {}

pub struct Source<'a>(&'a str);
pub struct Ast(Stmt);
pub struct Checked(CheckedAst);

impl CompileState for Source<'_> {}
impl CompileState for Ast {}
impl CompileState for Checked {}

pub struct Pipeline<S: CompileState> {
    errors: Vec<Box<dyn Error>>,
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
            errors: errors
                .into_iter()
                .map(|err| self.box_parse_error(err))
                .collect(),
            file: self.file,
            state: Ast(ast),
        }
    }

    fn box_parse_error(&self, err: Located<crate::parser::Error>) -> Box<dyn Error> {
        todo!("{err:?}")
    }
}

impl Pipeline<Ast> {
    pub fn dump(&self) {
        pretty::print_stmt(&self.state.0, 0);
    }

    pub fn typecheck(mut self) -> Pipeline<Checked> {
        let (checked, errors) = TypeChecker::check(self.state.0);
        self.errors.extend(errors.into_iter().map(|err| todo!()));
        Pipeline {
            errors: self.errors,
            file: self.file,
            state: Checked(checked),
        }
    }
}

impl Pipeline<Checked> {
    pub fn codegen(self) -> Result<String, Vec<Box<dyn Error>>> {
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
