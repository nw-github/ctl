use std::io::{stdout, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use crate::{
    lexer::{Located, Span, Token},
    sym::Scopes,
    typeid::Type,
};

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub struct FileId(usize);

#[derive(Default)]
pub struct Diagnostics {
    errors: Vec<Error>,
    warnings: Vec<Error>,
    paths: Vec<PathBuf>,
}

impl Diagnostics {
    pub fn error(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn warn(&mut self, err: Error) {
        self.warnings.push(err);
    }

    pub fn add_file(&mut self, path: PathBuf) -> FileId {
        self.paths.push(path);
        FileId(self.paths.len() - 1)
    }

    pub fn file_path(&self, file: FileId) -> &Path {
        &self.paths[file.0]
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn display(&self, lsp_file: Option<&(FileId, String)>) {
        for (i, path) in self.paths.iter().enumerate() {
            self.display_diagnostics(FileId(i), &self.errors, lsp_file, |msg, row, col| {
                eprintln!("error: {}:{row}:{col}: {msg}", path.display());
            });
        }

        for (i, path) in self.paths.iter().enumerate() {
            self.display_diagnostics(FileId(i), &self.warnings, lsp_file, |msg, row, col| {
                eprintln!("warning: {}:{row}:{col}: {msg}", path.display());
            });
        }
    }

    pub fn display_json(&self, lsp_file: Option<&(FileId, String)>) {
        fn format_json(
            stdout: &mut impl Write,
            written: &mut bool,
            msgs: &[Error],
            severity: &str,
        ) {
            for Error { diagnostic, span } in msgs {
                if *written {
                    _ = write!(stdout, ",");
                }
                _ = write!(
                    stdout,
                    "{{\"id\":{},\"pos\":{},\"len\":{},\"msg\":\"{}\",\"sev\":\"{}\"}}",
                    span.file.0, span.pos, span.len, diagnostic, severity,
                );
                *written = true;
            }
        }

        let mut stdout = stdout().lock();

        _ = write!(stdout, "{{\"files\":[");
        for (i, path) in self.paths.iter().enumerate() {
            if i > 0 {
                _ = write!(stdout, ",");
            }
            _ = write!(stdout, "\"");
            _ = stdout.write(path.as_os_str().as_bytes());
            _ = write!(stdout, "\"");
        }
        _ = write!(stdout, "],\"diagnostics\":[");
        let mut written = false;
        format_json(&mut stdout, &mut written, &self.errors, "error");
        format_json(&mut stdout, &mut written, &self.warnings, "warning");
        _ = write!(stdout, "]");
        if let Some((id, _)) = lsp_file {
            _ = write!(stdout, ",\"input_file\":{}}}", id.0);
        } else {
            _ = write!(stdout, "}}");
        }
    }

    fn display_diagnostics(
        &self,
        id: FileId,
        errors: &[Error],
        lsp_file: Option<&(FileId, String)>,
        mut format: impl FnMut(&str, usize, usize),
    ) {
        let mut file = None;
        for Error { diagnostic, span } in errors.iter().filter(|err| err.span.file == id) {
            // TODO: report error instead of unwrapping
            let data = lsp_file
                .filter(|(rhs, _)| id == *rhs)
                .map(|f| &f.1)
                .unwrap_or_else(|| {
                    file.get_or_insert_with(|| std::fs::read_to_string(&self.paths[id.0]).unwrap())
                });
            let (mut row, mut col) = (1usize, 1usize);
            // maybe do this first and keep a vector of positions?
            for ch in data.chars().take(span.pos) {
                if ch == '\n' {
                    row += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }

            format(diagnostic, row, col);
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

    pub fn invalid_operator(op: impl std::fmt::Display, ty: &str, span: Span) -> Self {
        Self::new(
            format!("operator '{op}' is invalid for a value of type '{ty}'"),
            span,
        )
    }

    pub fn shared_member(name: &str, span: Span) -> Self {
        Self::new(
            format!("cannot declare variant member with same name as shared member '{name}'"),
            span,
        )
    }

    pub fn unterminated_str(span: Span) -> Self {
        Self::new("unterminated string literal", span)
    }

    pub fn non_ascii_char(span: Span) -> Self {
        Self::new(
            "invalid char escape (must be within the range 0..=0x7f)",
            span,
        )
    }

    pub fn not_valid_here(token: &Located<Token>) -> Self {
        Self::new(format!("'{}' is not valid here", token.data), token.span)
    }

    pub fn type_mismatch(expected: &Type, received: &Type, scopes: &Scopes, span: Span) -> Self {
        Self::new(
            format!(
                "type mismatch: expected type '{}', found '{}'",
                expected.name(scopes),
                received.name(scopes)
            ),
            span,
        )
    }

    pub fn type_mismatch_s(expected: &str, received: &str, span: Span) -> Self {
        Self::new(
            format!("type mismatch: expected type '{expected}', found '{received}'"),
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
        Self::new(format!("no member '{member}' found on type '{ty}'"), span)
    }

    pub fn no_method(ty: &str, method: &str, span: Span) -> Self {
        Self::new(format!("no method '{method}' found on type '{ty}'"), span)
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

    pub fn redefinition(name: &str, span: Span) -> Self {
        Self::new(format!("redefinition of name '{name}'"), span)
    }

    pub fn redefinition_k(kind: &str, name: &str, span: Span) -> Self {
        Self::new(format!("redefinition of {kind} '{name}'"), span)
    }

    pub fn must_be_irrefutable(ty: &str, span: Span) -> Self {
        Self::new(format!("{ty} must be irrefuable"), span)
    }

    pub fn expected_found(expected: &str, received: &str, span: Span) -> Self {
        Self::new(format!("expected {expected}, found {received}"), span)
    }

    pub fn match_statement(why: &str, span: Span) -> Self {
        Self::new(
            format!("match statement does not cover all cases {why}"),
            span,
        )
    }

    pub fn cyclic(a: &str, b: &str, span: Span) -> Self {
        Self::new(format!("cyclic dependency between {a} and {b}"), span)
    }

    pub fn bad_destructure(ty: &str, span: Span) -> Self {
        Self::new(format!("cannot destructure value of type '{ty}'"), span)
    }
}
