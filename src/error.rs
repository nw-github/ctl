use std::{fmt::Display, path::{Path, PathBuf}};

use tower_lsp::lsp_types::{Position, Range};

use crate::lexer::{Located, Span, Token};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum OffsetMode {
    Utf8,
    Utf16,
    Utf32,
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct Diagnostics {
    diagnostics: Vec<Error>,
    paths: Vec<PathBuf>,
    inactive: Vec<Span>,
    errors_disabled: bool,
}

impl Diagnostics {
    pub fn report(&mut self, err: Error) {
        if !err.severity.is_error() || !self.errors_disabled {
            self.diagnostics.push(err);
        }
    }

    pub fn add_file(&mut self, path: PathBuf) -> FileId {
        self.paths.push(path);
        FileId(self.paths.len() as u32 - 1)
    }

    pub fn add_inactive(&mut self, span: Span) {
        self.inactive.push(span);
    }

    pub fn file_path(&self, file: FileId) -> &Path {
        &self.paths[file.0 as usize]
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|e| e.severity.is_error())
    }

    pub fn paths(&self) -> impl Iterator<Item = (FileId, &PathBuf)> {
        self.paths.iter().enumerate().map(|(i, path)| (FileId(i as u32), path))
    }

    pub fn get_file_id(&self, path: &Path) -> Option<FileId> {
        self.paths().find(|p| p.1 == path).map(|v| v.0)
    }

    pub fn diagnostics(&self) -> &[Error] {
        &self.diagnostics
    }

    pub fn inactive(&self) -> &[Span] {
        &self.inactive
    }

    pub fn get_span_range(data: &str, span: Span, mode: OffsetMode) -> Range {
        // maybe do this first and keep a vector of positions?
        let mut start = Position::new(0, 0);
        let mut chars = data.char_indices();
        for (i, ch) in &mut chars {
            if i as u32 >= span.pos {
                break;
            }

            if ch == '\n' {
                start.line += 1;
                start.character = 0;
            } else {
                start.character += match mode {
                    OffsetMode::Utf8 => ch.len_utf8(),
                    OffsetMode::Utf16 => ch.len_utf16(),
                    OffsetMode::Utf32 => 1,
                } as u32;
            }
        }

        let mut prev = start;
        let mut end = start;
        for (i, ch) in chars {
            if i as u32 > span.pos + span.len {
                break;
            }

            if ch == '\n' {
                prev = end;
                end.line += 1;
                end.character = 0;
            } else {
                end.character += match mode {
                    OffsetMode::Utf8 => ch.len_utf8(),
                    OffsetMode::Utf16 => ch.len_utf16(),
                    OffsetMode::Utf32 => 1,
                } as u32;
            }
        }

        if end.character == 0 {
            end.line = prev.line;
            end.character = prev.character + 1;
        }

        Range::new(start, end)
    }

    pub fn set_errors_enabled(&mut self, enabled: bool) -> bool {
        let prev = !self.errors_disabled;
        self.errors_disabled = !enabled;
        prev
    }

    pub fn capture_errors(&self) -> usize {
        self.diagnostics.len()
    }

    pub fn truncate_errors(&mut self, idx: usize) {
        self.diagnostics.truncate(idx);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, enum_as_inner::EnumAsInner)]
pub enum ErrorSeverity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub span: Span,
    pub severity: ErrorSeverity,
}

impl Error {
    pub fn new(message: impl Into<String>, span: impl Into<Span>) -> Self {
        Self { message: message.into(), span: span.into(), severity: ErrorSeverity::Error }
    }

    pub fn invalid_operator(op: impl Display, ty: impl Display, span: Span) -> Self {
        Self::new(format!("operator '{op}' is invalid for a value of type '{ty}'"), span)
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
        Self::new("invalid char escape (must be within the range 0..=0x7f)", span)
    }

    pub fn not_valid_here(token: &Located<Token>) -> Self {
        Self::new(format!("'{}' is not valid here", token.data), token.span)
    }

    pub fn type_mismatch(expected: impl Display, received: impl Display, span: Span) -> Self {
        Self::new(format!("type mismatch: expected type '{expected}', found '{received}'"), span)
    }

    pub fn private(item: &str, span: Span) -> Self {
        Self::new(format!("'{item}' is private"), span)
    }

    pub fn private_member(ty: impl Display, member: &str, span: Span) -> Self {
        Self::new(format!("cannot access private member '{member}' of type '{ty}'"), span)
    }

    pub fn no_member(ty: impl Display, member: &str, span: Span) -> Self {
        Self::new(format!("no member '{member}' found on type '{ty}'"), span)
    }

    pub fn no_method(ty: impl Display, method: &str, span: Span) -> Self {
        Self::new(format!("no method '{method}' found on type '{ty}'"), span)
    }

    pub fn no_symbol(symbol: &str, span: Span) -> Self {
        Self::new(format!("no symbol '{symbol}' found in this module"), span)
    }

    pub fn no_lang_item(name: &str, span: Span) -> Self {
        Self::new(format!("missing language item: '{name}'"), span)
    }

    pub fn doesnt_implement(ty: impl Display, tr: impl Display, span: Span) -> Self {
        Self::new(format!("type '{ty}' does not implement '{tr}'"), span)
    }

    pub fn wildcard_import(span: Span) -> Self {
        Self::new("wildcard import is only valid with modules", span)
    }

    pub fn is_unsafe(span: Span) -> Self {
        Self::new("this operation is unsafe", span)
    }

    pub fn redefinition(name: &str, span: Span) -> Self {
        Self::redefinition_k("name", name, span)
    }

    pub fn redefinition_k(kind: &str, name: &str, span: Span) -> Self {
        Self::new(format!("redefinition of {kind} '{name}'"), span)
    }

    pub fn must_be_irrefutable(ty: &str, span: Span) -> Self {
        Self::new(format!("{ty} must be irrefuable"), span)
    }

    pub fn expected_found(expected: impl Display, received: impl Display, span: Span) -> Self {
        Self::new(format!("expected {expected}, found {received}"), span)
    }

    pub fn match_statement(why: impl Display, span: Span) -> Self {
        Self::new(format!("match statement does not cover all cases {why}"), span)
    }

    pub fn bad_destructure(ty: impl Display, span: Span) -> Self {
        Self::new(format!("cannot destructure value of type '{ty}'"), span)
    }

    pub fn recursive_type(member: &str, span: Span, variant: bool) -> Self {
        Self::new(
            format!(
                "{} '{member}' gives this type infinite size",
                if variant { "variant" } else { "member" }
            ),
            span,
        )
    }

    pub fn no_consteval(span: Span) -> Self {
        Self::new("expression is not compile time evaluatable", span)
    }

    pub fn not_assignable(span: Span) -> Self {
        Self::new("expression is not assignable", span)
    }

    pub fn consteval_overflow(span: Span) -> Self {
        Self::new("expression overflows during constant evaluation", span)
    }

    pub fn recursive_trait(span: Span) -> Self {
        Self::new("trait recursively requires implementation of itself", span)
    }

    pub fn bitfield_member(name: &str, span: Span) -> Self {
        Self::new(
            format!(
                "member '{name}' of packed struct must have integer or enum union type (union with all empty variants)"
            ),
            span,
        )
    }

    pub fn invalid_impl(func: &str, why: &str, span: Span) -> Self {
        Self::new(format!("invalid implementation of function '{func}': {why}"), span)
    }

    pub fn invalid_attr(name: impl Display, span: Span) -> Self {
        Self::new(format!("invalid attribute '{name}'"), span)
    }
}

pub struct Warning;

impl Warning {
    #[allow(clippy::new_ret_no_self)]
    fn new(message: impl Into<String>, span: impl Into<Span>) -> Error {
        Error { message: message.into(), span: span.into(), severity: ErrorSeverity::Warning }
    }

    pub fn redundant_token(token: &Located<Token>) -> Error {
        Self::new(format!("redundant '{}'", token.data), token.span)
    }

    pub fn redundant_unsafe(span: Span) -> Error {
        Self::new("unsafe expression in unsafe context", span)
    }

    pub fn useless_unsafe(span: Span) -> Error {
        Self::new("unsafe expression contains no unsafe operations", span)
    }

    pub fn decimal_leading_zero(span: Span) -> Error {
        Self::new("leading zero in decimal literal (use 0o to create an octal literal)", span)
    }

    pub fn subscript_addr(span: Span) -> Error {
        Self::new("taking address of subscript that returns a value creates a temporary", span)
    }

    pub fn bitfield_addr(span: Span) -> Error {
        Self::new("taking address of bitfield creates a temporary", span)
    }

    pub fn unused_variable(name: &str, span: Span) -> Error {
        Self::new(format!("unused variable: '{name}'"), span)
    }

    pub fn unnecessary_fallible_cast(src: impl Display, dst: impl Display, span: Span) -> Error {
        Self::new(
            format!("cast from type '{src}' to '{dst}' is infallible and may use an `as` cast",),
            span,
        )
    }

    pub fn call_mutating_on_bitfield(span: Span) -> Error {
        Self::new("call to mutating method with bitfield receiver operates on a copy", span)
    }

    pub fn mut_ptr_to_const(span: Span) -> Error {
        Self::new("taking a mutable pointer to a constant operates on a copy", span)
    }
}
