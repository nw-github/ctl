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
        for (i, path) in self.paths.iter().enumerate() {
            let mut file = None;
            for Error { diagnostic, span } in
                self.errors.iter().filter(|err| err.span.file == FileId(i))
            {
                // TODO: report error instead of unwrapping
                let data = file.get_or_insert_with(|| std::fs::read_to_string(path).unwrap());
                let (mut row, mut col) = (1, 1);
                // maybe do this first and keep a vector of positions?
                for ch in data.chars().take(span.pos) {
                    if ch == '\n' {
                        row += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                }

                eprintln!("{}:{row}:{col}: {diagnostic}", path.display())
            }
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

    pub fn redefinition(kind: &str, name: &str, span: Span) -> Self {
        Self::new(format!("redefinition of {kind} '{name}'"), span)
    }

    pub fn must_be_irrefutable(ty: &str, span: Span) -> Self {
        Self::new(format!("{ty} must be irrefuable"), span)
    }
}
