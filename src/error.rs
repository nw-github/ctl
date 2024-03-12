use std::path::{Path, PathBuf};

use tower_lsp::lsp_types::{Position, Range};

use crate::{
    lexer::{Located, Span, Token},
    sym::Scopes,
    typeid::Type,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum OffsetMode {
    Utf8,
    Utf16,
    Utf32,
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub struct FileId(u32);

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct Diagnostics {
    errors: Vec<Error>,
    warnings: Vec<Error>,
    paths: Vec<PathBuf>,
    errors_disabled: bool,
}

impl Diagnostics {
    pub fn error(&mut self, err: Error) {
        if !self.errors_disabled {
            self.errors.push(err);
        }
    }

    pub fn warn(&mut self, err: Error) {
        self.warnings.push(err);
    }

    pub fn add_file(&mut self, path: PathBuf) -> FileId {
        self.paths.push(path);
        FileId(self.paths.len() as u32 - 1)
    }

    pub fn file_path(&self, file: FileId) -> &Path {
        &self.paths[file.0 as usize]
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn paths(&self) -> impl Iterator<Item = (FileId, &PathBuf)> {
        self.paths
            .iter()
            .enumerate()
            .map(|(i, path)| (FileId(i as u32), path))
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn warnings(&self) -> &[Error] {
        &self.warnings
    }

    pub fn get_span_range(data: &str, span: Span, mode: OffsetMode) -> Range {
        // maybe do this first and keep a vector of positions?
        let mut start = (0u32, 0u32);
        let mut chars = data.char_indices();
        for (i, ch) in &mut chars {
            if i as u32 >= span.pos {
                break;
            }

            if ch == '\n' {
                start.0 += 1;
                start.1 = 0;
            } else {
                start.1 += match mode {
                    OffsetMode::Utf8 => ch.len_utf8(),
                    OffsetMode::Utf16 => ch.len_utf16(),
                    OffsetMode::Utf32 => 1,
                } as u32;
            }
        }

        let mut end = start;
        for (i, ch) in chars {
            if i as u32 > span.pos + span.len {
                break;
            }

            if ch == '\n' {
                end.0 += 1;
                end.1 = 0;
            } else {
                end.1 += match mode {
                    OffsetMode::Utf8 => ch.len_utf8(),
                    OffsetMode::Utf16 => ch.len_utf16(),
                    OffsetMode::Utf32 => 1,
                } as u32;
            }
        }

        Range::new(Position::new(start.0, start.1), Position::new(end.0, end.1))
    }

    pub fn set_errors_enabled(&mut self, enabled: bool) {
        self.errors_disabled = !enabled;
    }
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub span: Span,
}

impl Error {
    pub fn new(message: impl Into<String>, span: impl Into<Span>) -> Self {
        Self {
            message: message.into(),
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

    pub fn private(item: &str, span: Span) -> Self {
        Self::new(format!("'{item}' is private"), span)
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
