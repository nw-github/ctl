use std::path::{Path, PathBuf};

use crate::lexer::Span;

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub struct FileId(usize);

#[derive(Default)]
pub struct Diagnostics {
    errors: Vec<Error>,
    paths: Vec<PathBuf>,
}

impl Diagnostics {
    pub fn error(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn add_file(&mut self, path: PathBuf) -> FileId {
        self.paths.push(path);
        FileId(self.paths.len() - 1)
    }

    pub fn file_path(&mut self, file: FileId) -> &Path {
        &self.paths[file.0]
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn display(&self) {
        for error in self.errors.iter() {
            error.display(&self.paths[error.span.file.0]);
        }
    }
}

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

    pub fn display(&self, path: &Path) {
        eprintln!(
            "{}:{}:{}: {}",
            path.display(),
            self.span.loc.row,
            self.span.loc.col,
            self.diagnostic
        )
    }

    pub fn not_valid_here(token: &str, span: Span) -> Self {
        Self::new(format!("'{token}' is not valid here"), span)
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

    pub fn wildcard_import(span: Span) -> Self {
        Self::new("wildcard import is only valid with modules", span)
    }

    pub fn is_unsafe(span: Span) -> Self {
        Self::new("this operation is unsafe", span)
    }

    pub fn name_redef(name: &str, span: Span) -> Self {
        Self::new(format!("redefinition of name {name}"), span)
    }
}
