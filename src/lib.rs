mod ast;
mod codegen;
mod error;
mod lexer;
pub mod lsp;
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
use error::{Diagnostics, FileId};
use lexer::{Lexer, Span};
use sym::Scopes;
use typecheck::Module;

use crate::{parser::Parser, typecheck::TypeChecker};

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

pub trait CompileState {}

pub struct Source;
pub struct Parsed(Stmt, Vec<PathBuf>);
pub struct Checked(Module);
pub struct Built(String);

impl CompileState for Source {}
impl CompileState for Parsed {}
impl CompileState for Checked {}
impl CompileState for Built {}

pub struct Compiler<S: CompileState> {
    diag: Diagnostics,
    state: S,
    lsp_file: Option<(FileId, String)>,
}

impl Compiler<Source> {
    pub fn new() -> Self {
        Self {
            state: Source,
            diag: Diagnostics::default(),
            lsp_file: None,
        }
    }

    pub fn new_lsp(path: PathBuf, buf: String) -> (Self, FileId) {
        let mut diag = Diagnostics::default();
        let fileid = diag.add_file(path);
        (
            Self {
                state: Source,
                diag,
                lsp_file: Some((fileid, buf)),
            },
            fileid,
        )
    }

    pub fn with_diagnostics(diag: Diagnostics) -> Self {
        Compiler {
            diag,
            state: Source,
            lsp_file: None,
        }
    }

    pub fn parse(mut self, path: PathBuf, libs: Vec<PathBuf>) -> anyhow::Result<Compiler<Parsed>> {
        if let Some(stmt) = self.gather_sources(path)? {
            Ok(Compiler {
                state: Parsed(stmt, libs),
                diag: self.diag,
                lsp_file: self.lsp_file,
            })
        } else {
            Err(anyhow::anyhow!(
                "compile target must be directory or have extension '.ctl'",
            ))
        }
    }

    fn gather_sources(&mut self, path: PathBuf) -> anyhow::Result<Option<Stmt>> {
        if let Some((file_id, data)) = self
            .lsp_file
            .as_ref()
            .filter(|(rhs, _)| path == self.diag.file_path(*rhs))
        {
            Ok(Some(Parser::parse(data, &mut self.diag, *file_id)))
        } else if path.is_dir() {
            let mut body = Vec::new();
            for entry in path
                .read_dir()
                .with_context(|| format!("loading path {}", path.display()))?
            {
                let path = entry?.path();
                if let Some(stmt) = self.gather_sources(path.canonicalize().unwrap_or(path))? {
                    body.push(stmt);
                }
            }
            Ok(Some(Stmt {
                data: ast::parsed::StmtData::Module {
                    public: true,
                    file: false,
                    name: lexer::Located::new(Span::default(), derive_module_name(&path)),
                    body,
                },
                attrs: Default::default(),
            }))
        } else if path.extension() == Some(OsStr::new("ctl")) {
            let buffer = std::fs::read_to_string(&path)
                .with_context(|| format!("loading path {}", path.display()))?;
            let file_id = self.diag.add_file(path);
            Ok(Some(Parser::parse(&buffer, &mut self.diag, file_id)))
        } else {
            Ok(None)
        }
    }
}

impl Default for Compiler<Source> {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler<Parsed> {
    pub fn dump(&self) {
        pretty::print_stmt(&self.state.0, 0);
    }

    pub fn typecheck(self, hover_span: Option<Span>) -> anyhow::Result<Compiler<Checked>> {
        let (module, diag) = TypeChecker::check(self.state.0, self.state.1, self.diag, hover_span)?;
        Ok(Compiler {
            state: Checked(module),
            diag,
            lsp_file: self.lsp_file,
        })
    }
}

impl Compiler<Checked> {
    pub fn build(mut self, flags: CodegenFlags) -> Result<Compiler<Built>, Self> {
        if let Ok(code) = Codegen::build(
            &mut self.diag,
            self.state.0.scope,
            &self.state.0.scopes,
            flags,
        ) {
            Ok(Compiler {
                state: Built(code),
                diag: self.diag,
                lsp_file: self.lsp_file,
            })
        } else {
            Err(self)
        }
    }

    pub fn scopes(&self) -> &Scopes {
        &self.state.0.scopes
    }

    pub fn module(&self) -> &Module {
        &self.state.0
    }
}

impl Compiler<Built> {
    pub fn code(self) -> String {
        self.state.0
    }
}

impl<T: CompileState> Compiler<T> {
    pub fn inspect(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diag
    }

    pub fn lsp_file(&self) -> Option<&(FileId, String)> {
        self.lsp_file.as_ref()
    }
}

pub struct CodegenFlags {
    pub leak: bool,
    pub no_bit_int: bool,
    pub lib: bool,
    pub minify: bool,
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

pub fn get_default_libs(
    path: &Path,
    mut libs: Vec<PathBuf>,
    mut no_core: bool,
    mut no_std: bool,
) -> (PathBuf, Vec<PathBuf>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let core_path = root.join("ctl/core");
    let std_path = root.join("ctl/std");
    let path = path.canonicalize().unwrap_or_else(|_| path.into());
    if path == core_path {
        no_core = true;
        no_std = true;
    } else if path == std_path {
        no_std = true;
    }

    if !no_std {
        libs.insert(0, root.join(std_path));
    }

    if !no_core {
        libs.insert(0, root.join(core_path));
    }
    (path, libs)
}

fn nearest_pow_of_two(bits: u32) -> usize {
    2usize.pow((bits as f64).log2().ceil() as u32).max(8)
}
