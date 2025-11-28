use std::collections::HashMap;
use std::fmt::Write;
use std::path::Path;

use dashmap::DashMap;
use tokio::sync::MutexGuard;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::error::{Diagnostics, FileId, OffsetMode};
use crate::lexer::Span;
use crate::project::Project;
use crate::sym::{FunctionId, ScopeId, Scopes, Union, UserTypeId, UserTypeKind, VariableId};
use crate::typecheck::{LspInput, LspItem};
use crate::typeid::{GenericUserType, Type, TypeId, Types};
use crate::{
    CachingSourceProvider, Compiler, FileSourceProvider, SourceProvider, UnloadedProject,
    THIS_PARAM,
};

#[macro_export]
macro_rules! write_de {
    ($dst:expr, $($arg:tt)*) => {
        _ = write!($dst, $($arg)*)
    };
}

#[macro_export]
macro_rules! writeln_de {
    ($dst:expr, $($arg:tt)*) => {
        _ = writeln!($dst, $($arg)*)
    };
}

macro_rules! info {
    ($self: expr, $($arg: tt)*) => {{
        $self.client.log_message(MessageType::INFO, format!($($arg)*)).await
    }};
}

macro_rules! debug {
    ($self: expr, $($arg: tt)*) => {{
        #[cfg(debug_assertions)]
        $self.client.log_message(MessageType::LOG, format!($($arg)*)).await
    }};
}

#[derive(serde::Deserialize, Clone, Copy, Debug)]
struct Configuration {
    #[serde(rename = "debounceMs")]
    _debounce_ms: u32,
    #[serde(rename = "maxNumberOfProblems")]
    _max_problems: usize,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            _debounce_ms: 250,
            _max_problems: 100,
        }
    }
}

#[derive(Default)]
pub enum Lazy<T> {
    Initialized(Option<T>),
    #[default]
    None,
}

impl<T> Lazy<T> {
    pub fn get_or_try(&mut self, f: impl FnOnce() -> Option<T>) -> Option<&mut T> {
        if let Self::None = self {
            *self = Self::Initialized(f());
        }

        if let Self::Initialized(value) = self {
            value.as_mut()
        } else {
            None
        }
    }
}

#[derive(Default)]
struct FileInfoCache {
    lsp_items: Vec<(LspItem, Span)>,
    inlay_hints: Lazy<Vec<InlayHint>>,
    semantic_tokens: Lazy<Vec<SemanticToken>>,
}

struct LastChecked {
    data: UnloadedProject,
    checked: Project,
}

pub struct LspBackend {
    client: Client,
    /// Files that the client has ownership of
    open_files: DashMap<Url, String>,
    documents: DashMap<Url, FileInfoCache>,
    config: tokio::sync::Mutex<Configuration>,
    project: tokio::sync::Mutex<Option<LastChecked>>,
}

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
mod token {
    use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType};

    pub const TOKEN_TYPES: [SemanticTokenType; 4] = [
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::TYPE,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::FUNCTION,
    ];

    pub const ENUM_MEMBER: u32 = 0;
    pub const TYPE: u32 = 1;
    pub const VARIABLE: u32 = 2;
    pub const FUNCTION: u32 = 3;

    pub const TOKEN_MODS: [SemanticTokenModifier; 1] = [SemanticTokenModifier::new("mutable")];

    pub const NO_MODS: u32 = 0;
    pub const MUTABLE: u32 = 1 << 0;
}

#[tower_lsp::async_trait]
impl LanguageServer for LspBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                inlay_hint_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: None,
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: Default::default(),
                            legend: SemanticTokensLegend {
                                token_types: token::TOKEN_TYPES.into(),
                                token_modifiers: token::TOKEN_MODS.into(),
                            },
                            range: None,
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                // inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                //     InlayHintOptions {
                //         resolve_provider: Some(true),
                //         work_done_progress_options: Default::default(),
                //     },
                // ))),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!(self, "Server initialized!");
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        debug!(self, "did_change_configuration");
        *self.config.lock().await = serde_json::from_value(params.settings).unwrap_or_default();
        // _ = self.check_project(None, None, false).await;
    }

    async fn did_open(&self, mut params: DidOpenTextDocumentParams) {
        debug!(self, "did_open: '{}'", params.text_document.uri);
        self.open_files
            .entry(params.text_document.uri.clone())
            .and_modify(|text| *text = std::mem::take(&mut params.text_document.text))
            .or_insert_with(|| std::mem::take(&mut params.text_document.text));
        _ = self
            .check_project(&params.text_document.uri, None, false)
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        debug!(self, "did_change: '{}'", params.text_document.uri);
        self.open_files
            .entry(params.text_document.uri.clone())
            .and_modify(|text| *text = std::mem::take(&mut params.content_changes[0].text))
            .or_insert_with(|| std::mem::take(&mut params.content_changes[0].text));
        _ = self
            .check_project(&params.text_document.uri, None, true)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        debug!(self, "did_close: '{}'", params.text_document.uri);
        self.open_files.remove(&params.text_document.uri);
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        self.with_proj_and_doc(&params.text_document.uri, async |doc, path, file, proj| {
            doc.inlay_hints
                .get_or_try(|| {
                    self.with_source(path, &mut FileSourceProvider, |src| {
                        get_inlay_hints(&proj.scopes, &mut proj.types, src, file)
                    })
                })
                .cloned()
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        self.find_lsp_item(uri, pos, async |hover, proj| {
            let scopes = &proj.scopes;
            let span = match hover {
                &LspItem::Var(id) => scopes.get(id).name.span,
                &LspItem::Type(id) => scopes.get(id).name.span,
                &LspItem::Module(id) => scopes[id].kind.name(scopes).unwrap().span,
                &LspItem::Fn(id, _) => scopes.get(id).name.span,
                LspItem::Property(_, id, member) => {
                    let ut = scopes.get(*id);
                    ut.members.get(member).map(|m| m.span).or_else(|| {
                        ut.kind
                            .as_union()
                            .and_then(|u| u.variants.get(member).map(|v| v.span))
                    })?
                }
                _ => return None,
            };

            let diag = &mut proj.diag;
            let path = diag.file_path(span.file);
            let uri = Url::from_file_path(path).unwrap();
            let range = self.with_source(path, &mut FileSourceProvider, |src| {
                Diagnostics::get_span_range(src, span, OffsetMode::Utf16)
            })?;
            Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
        })
        .await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        if let Some(_ctx) = params.context.and_then(|ctx| ctx.trigger_character) {
            debug!(
                self,
                "looking for completion at line {}, character {} due to trigger '{_ctx}'",
                pos.line,
                pos.character,
            );
        } else {
            debug!(
                self,
                "looking for completion at line {}, character {}", pos.line, pos.character,
            );
        }

        let mut guard = self.check_project(uri, Some(pos), true).await?;
        let Some(proj) = guard.as_mut() else {
            return Ok(None);
        };
        let proj = &mut proj.checked;

        let Some(completions) = proj.completions.as_ref() else {
            return Ok(None);
        };
        let scopes = &proj.scopes;
        let types = &mut proj.types;
        let completions = completions
            .items
            .iter()
            .flat_map(|completion| {
                Some(match completion {
                    LspItem::Property(_, id, name) => {
                        let ut = scopes.get(*id);
                        let member = ut.members.get(name).unwrap();
                        let detail = member.ty.name(scopes, types);
                        CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(detail.clone()),
                            }),
                            detail: Some(detail),
                            ..Default::default()
                        }
                    }
                    &LspItem::Fn(id, owner) => {
                        let f = scopes.get(id);
                        let empty_variant =
                            owner.is_some_and(|id| scopes.get(id).is_empty_variant(&f.name.data));
                        let label = f.name.data.clone();
                        let insert_text = (!empty_variant).then(|| {
                            let mut text = format!("{label}(");
                            for (i, param) in f
                                .params
                                .iter()
                                .filter(|p| !completions.method || p.label != THIS_PARAM)
                                .enumerate()
                            {
                                if i > 0 {
                                    text += ", ";
                                }
                                if param.keyword {
                                    write_de!(
                                        text,
                                        "{}: ${{{}:{}}}",
                                        param.label,
                                        i + 1,
                                        param.label
                                    );
                                } else {
                                    write_de!(text, "${{{}:{}}}", i + 1, param.label);
                                }
                            }
                            text += ")";
                            text
                        });
                        let desc = visualize_func(id, true, scopes, types);
                        CompletionItem {
                            label,
                            label_details: Some(CompletionItemLabelDetails {
                                detail: owner.and_then(|owner| match &scopes.get(owner).kind {
                                    UserTypeKind::Extension(_) => {
                                        Some(format!(" (from {})", scopes.get(owner).name.data))
                                    }
                                    UserTypeKind::Trait(_, _) => {
                                        Some(format!(" (as {})", scopes.get(owner).name.data))
                                    }
                                    _ => None,
                                }),
                                description: desc.clone().into(),
                            }),
                            kind: Some(
                                if f.constructor
                                    .is_some_and(|id| scopes.get(id).kind.is_union())
                                {
                                    CompletionItemKind::ENUM_MEMBER
                                } else if f.constructor.is_some() {
                                    CompletionItemKind::CONSTRUCTOR
                                } else if f.params.first().is_some_and(|p| p.label == THIS_PARAM) {
                                    CompletionItemKind::METHOD
                                } else {
                                    CompletionItemKind::FUNCTION
                                },
                            ),
                            detail: desc.into(),
                            insert_text,
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        }
                    }
                    &LspItem::Type(id) => {
                        let ut = scopes.get(id);
                        CompletionItem {
                            label: ut.name.data.clone(),
                            kind: Some(match ut.kind {
                                UserTypeKind::Union(_) => CompletionItemKind::ENUM,
                                UserTypeKind::Trait(_, _) => CompletionItemKind::INTERFACE,
                                UserTypeKind::Template => CompletionItemKind::TYPE_PARAMETER,
                                _ => CompletionItemKind::STRUCT,
                            }),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(ut.name.data.clone()),
                            }),
                            detail: Some(ut.name.data.clone()),
                            ..Default::default()
                        }
                    }
                    &LspItem::Module(id) => {
                        let scope = &scopes[id];
                        let name = scope.kind.name(scopes).unwrap().data.clone();
                        CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::MODULE),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(name.clone()),
                            }),
                            detail: Some(name),
                            ..Default::default()
                        }
                    }
                    &LspItem::Var(id) => {
                        let var = scopes.get(id);
                        let typ = var.ty.name(scopes, types);
                        CompletionItem {
                            label: var.name.data.clone(),
                            kind: Some(if var.is_static {
                                CompletionItemKind::CONSTANT
                            } else {
                                CompletionItemKind::VARIABLE
                            }),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(typ.clone()),
                            }),
                            detail: Some(var.name.data.clone()),
                            ..Default::default()
                        }
                    }
                    &LspItem::BuiltinType(name) => CompletionItem {
                        label: name.into(),
                        kind: Some(CompletionItemKind::STRUCT),
                        label_details: Some(CompletionItemLabelDetails {
                            detail: None,
                            description: Some(name.into()),
                        }),
                        detail: Some(name.into()),
                        ..Default::default()
                    },
                    _ => return None,
                })
            })
            .collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        self.find_lsp_item(uri, pos, async |item, proj| {
            let scopes = &proj.scopes;
            let types = &mut proj.types;
            let str = match item {
                &LspItem::Var(id) => Some(visualize_var(id, scopes, types)),
                &LspItem::Fn(id, _) => Some(visualize_func(id, false, scopes, types)),
                &LspItem::Type(id) => Some(visualize_type(id, scopes, types)),
                LspItem::Underscore(ty) => Some(ty.name(scopes, types)),
                LspItem::Property(src_ty, id, name) => {
                    let ut = scopes.get(*id);
                    let mem = ut.members.get(name);
                    let public = ["", "pub "][mem.is_some_and(|m| m.public) as usize];
                    let ty = mem.map_or(TypeId::UNKNOWN, |m| m.ty);
                    if matches!(ut.kind, UserTypeKind::Tuple | UserTypeKind::AnonStruct) {
                        let real = src_ty
                            .map(|src| ty.with_ut_templates(types, src))
                            .unwrap_or(ty);
                        Some(format!("{public}{name}: {}", real.name(scopes, types)))
                    } else {
                        let offs = if let UserTypeKind::PackedStruct(data) = &ut.kind {
                            format!("// bit offset: {}\n", data.bit_offsets[name])
                        } else {
                            // TODO: normal offset
                            "".to_string()
                        };

                        Some(format!(
                            "{}{offs}{public}{name}: {}",
                            visualize_location(ut.body_scope, scopes),
                            ty.name(scopes, types)
                        ))
                    }
                }
                &LspItem::Module(id) => {
                    let scope = &scopes[id];
                    let mut res = String::new();
                    if let Some(parent) = scope.parent.filter(|parent| *parent != ScopeId::ROOT) {
                        res += &visualize_location(parent, scopes);
                    }
                    if scope.public {
                        res += "pub ";
                    }
                    Some(format!("{res}mod {}", scope.kind.name(scopes).unwrap()))
                }
                _ => None,
            };

            str.map(|value| Hover {
                range: None,
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "ctl".into(),
                    value,
                })),
            })
        })
        .await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.with_proj_and_doc(&params.text_document.uri, async |doc, path, file, proj| {
            let tokens = doc.semantic_tokens.get_or_try(|| {
                self.with_source(path, &mut FileSourceProvider, |src| {
                    get_semantic_tokens(&proj.scopes, &doc.lsp_items, src, file)
                })
            });

            Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens.cloned().unwrap_or_default(),
            }))
        })
        .await
    }

    async fn shutdown(&self) -> Result<()> {
        info!(self, "shutdown");
        self.documents.clear();
        Ok(())
    }
}

impl LspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            open_files: Default::default(),
            config: Default::default(),
            documents: Default::default(),
            project: Default::default(),
        }
    }

    fn make_diagnostic(
        &self,
        diag: &Diagnostics,
        provider: &mut impl SourceProvider,
        span: Span,
        msg: &str,
        severity: DiagnosticSeverity,
        all: &mut HashMap<Url, Vec<Diagnostic>>,
    ) {
        let path = diag.file_path(span.file);
        let Some(range) = self.with_source(path, provider, |src| {
            Diagnostics::get_span_range(src, span, OffsetMode::Utf16)
        }) else {
            return;
        };

        let entry = all.entry(Url::from_file_path(path).unwrap()).or_default();
        entry.push(Diagnostic {
            range,
            severity: Some(severity),
            source: Some("ctlsp".into()),
            message: msg.into(),
            tags: (severity == DiagnosticSeverity::HINT).then(|| vec![DiagnosticTag::UNNECESSARY]),
            ..Diagnostic::default()
        });
    }

    async fn check_project(
        &self,
        uri: &Url,
        completion: Option<Position>,
        change: bool,
    ) -> Result<MutexGuard<'_, Option<LastChecked>>> {
        debug!(self, "Checking project: '{}'", uri);
        let path = Self::uri_to_path(uri);
        let unloaded = match UnloadedProject::new(get_file_project(path)) {
            Ok(proj) => proj,
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("{err}"))
                    .await;
                return Err(Error::internal_error());
            }
        };

        let mut proj_lock_guard = self.project.lock().await;
        if !change {
            if let Some(prev) = proj_lock_guard.as_ref() {
                if unloaded
                    .mods
                    .iter()
                    .all(|(path, module)| prev.data.mods.get(path).is_some_and(|m| m == module))
                {
                    debug!(
                        self,
                        " -> Checking file in submodule of previous project, skip"
                    );
                    return Ok(proj_lock_guard);
                }
            }

            self.documents.clear();
        }

        let parsed = match Compiler::with_provider(LspFileProvider::new(&self.open_files))
            .parse(unloaded.clone())
        {
            Ok(parsed) => parsed,
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("{err}"))
                    .await;
                return Err(Error::internal_error());
            }
        };

        let file = parsed.diagnostics().get_file_id(path);
        let checked = parsed.typecheck(LspInput {
            completion: completion.and_then(|p| {
                file.map(|file| {
                    position_to_span(
                        self.open_files.get(uri).unwrap().as_ref(),
                        file,
                        p.line,
                        p.character,
                    )
                })
            }),
        });

        let mut proj = checked.project();
        let mut all = HashMap::<Url, Vec<Diagnostic>>::new();
        let mut cache = CachingSourceProvider::new();
        for err in proj.diag.errors() {
            self.make_diagnostic(
                &proj.diag,
                &mut cache,
                err.span,
                &err.message,
                DiagnosticSeverity::ERROR,
                &mut all,
            );
        }

        for err in proj.diag.warnings() {
            self.make_diagnostic(
                &proj.diag,
                &mut cache,
                err.span,
                &err.message,
                DiagnosticSeverity::WARNING,
                &mut all,
            );
        }

        for &span in proj.diag.inactive() {
            self.make_diagnostic(
                &proj.diag,
                &mut cache,
                span,
                "this region is disabled",
                DiagnosticSeverity::HINT,
                &mut all,
            );
        }

        proj.lsp_items.sort_by_key(|(_, span)| span.pos);
        for (id, path) in proj.diag.paths() {
            let Ok(uri) = Url::from_file_path(path) else {
                continue;
            };

            self.client
                .publish_diagnostics(uri.clone(), all.remove(&uri).unwrap_or_default(), None)
                .await;

            let mut doc = self.documents.entry(uri).or_default();
            doc.lsp_items = proj
                .lsp_items
                .iter()
                .filter(|(_, span)| span.file == id)
                .cloned()
                .collect();
            doc.inlay_hints = Lazy::None;
            doc.semantic_tokens = Lazy::None;
        }

        *proj_lock_guard = Some(LastChecked {
            data: unloaded,
            checked: proj,
        });

        debug!(self, " -> done check_project");
        Ok(proj_lock_guard)
    }

    async fn with_proj_and_doc<T>(
        &self,
        uri: &Url,
        func: impl AsyncFnOnce(&mut FileInfoCache, &Path, FileId, &mut Project) -> Option<T>,
    ) -> Result<Option<T>> {
        let mut lock = if !self.documents.contains_key(uri) {
            let Ok(v) = self.check_project(uri, None, false).await else {
                return Ok(None);
            };
            v
        } else {
            self.project.lock().await
        };

        let Some(proj) = lock.as_mut() else {
            return Ok(None);
        };

        let path = Self::uri_to_path(uri);
        let Some(file) = proj.checked.diag.get_file_id(path) else {
            return Ok(None);
        };

        let Some(mut doc) = self.documents.get_mut(uri) else {
            return Ok(None);
        };

        Ok(func(&mut doc, path, file, &mut proj.checked).await)
    }

    async fn find_lsp_item<T>(
        &self,
        uri: &Url,
        pos: Position,
        func: impl AsyncFnOnce(&LspItem, &mut Project) -> Option<T>,
    ) -> Result<Option<T>> {
        self.with_proj_and_doc(uri, async |doc, path, file, proj| {
            let user = self.with_source(path, &mut FileSourceProvider, |src| {
                position_to_span(src, file, pos.line, pos.character)
            })?;
            let (item, _) = doc
                .lsp_items
                .iter()
                .find(|(_, span)| LspInput::matches(Some(user), *span))?;
            func(item, proj).await
        })
        .await
    }

    fn with_source<T>(
        &self,
        path: &Path,
        fallback: &mut impl SourceProvider,
        get: impl FnOnce(&str) -> T,
    ) -> Option<T> {
        if let Some(text) = Url::from_file_path(path)
            .ok()
            .and_then(|uri| self.open_files.get(&uri))
        {
            return Some(get(&text));
        }

        fallback.get_source(path, get).ok()
    }

    fn uri_to_path(uri: &Url) -> &Path {
        if !uri.scheme().is_empty() {
            Path::new(&uri.as_str()[uri.scheme().len() + 3..])
        } else {
            Path::new(uri.as_str())
        }
    }
}

#[derive(derive_more::Constructor)]
pub struct LspFileProvider<'a> {
    open_files: &'a DashMap<Url, String>,
}

impl SourceProvider for LspFileProvider<'_> {
    fn get_source<T>(&mut self, path: &Path, get: impl FnOnce(&str) -> T) -> anyhow::Result<T> {
        if let Some(text) = Url::from_file_path(path)
            .ok()
            .and_then(|uri| self.open_files.get(&uri))
        {
            return Ok(get(&text));
        }

        FileSourceProvider.get_source(path, get)
    }
}

fn get_file_project(input: &Path) -> &Path {
    let mut prev = input;
    while let Some(parent) = prev.parent() {
        if parent.join("ctl.toml").exists() {
            return parent;
        }
        prev = parent;
    }
    input
}

fn position_to_span(text: &str, file: FileId, line: u32, character: u32) -> Span {
    let (mut pos, mut row, mut col) = (0, 0, 0);
    for ch in text.chars() {
        if row == line && col == character {
            return Span { file, pos, len: 1 };
        }

        if ch == '\n' {
            row += 1;
            col = 0;
            if row > line {
                break;
            }
        } else {
            col += ch.len_utf16() as u32;
        }
        pos += ch.len_utf8() as u32;
    }
    Span::default()
}

fn get_inlay_hints(scopes: &Scopes, types: &mut Types, src: &str, file: FileId) -> Vec<InlayHint> {
    let mut hints = vec![];
    for (_, var) in scopes.vars() {
        if var.name.span.file != file || var.has_hint || var.name.data.starts_with('$') {
            continue;
        }

        let r = Diagnostics::get_span_range(src, var.name.span, OffsetMode::Utf16);
        hints.push(InlayHint {
            position: r.end,
            label: InlayHintLabel::String(format!(": {}", var.ty.name(scopes, types))),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Default::default(),
            tooltip: Default::default(),
            padding_left: Default::default(),
            padding_right: Default::default(),
            data: Default::default(),
        });
    }
    hints
}

/// Assumes items is filtered by FileId and sorted by Span position
fn get_semantic_tokens(
    scopes: &Scopes,
    items: &[(LspItem, Span)],
    src: &str,
    file: FileId,
) -> Vec<SemanticToken> {
    let mut tokens = vec![];
    let [mut prev_line, mut prev_start] = [0; 2];
    for (item, span) in items.iter().filter(|(_, span)| span.file == file) {
        let (token_type, token_modifiers_bitset) = match item {
            // LspItem::Module(scope_id) => todo!(),
            // LspItem::Underscore(type_id) => todo!(),
            LspItem::Property(_, _, _) => (token::VARIABLE, token::NO_MODS),
            &LspItem::Var(id) => {
                let mods = if scopes.get(id).mutable {
                    token::MUTABLE
                } else {
                    token::NO_MODS
                };
                (token::VARIABLE, mods)
            }
            LspItem::Type(_) => (token::TYPE, token::NO_MODS),
            LspItem::BuiltinType(_) => (token::TYPE, token::NO_MODS),
            &LspItem::Fn(id, _) => {
                if let Some(id) = scopes.get(id).constructor {
                    if scopes.get(id).kind.is_union() {
                        (token::ENUM_MEMBER, token::NO_MODS)
                    } else {
                        (token::TYPE, token::NO_MODS)
                    }
                } else {
                    (token::FUNCTION, token::NO_MODS)
                }
            }
            _ => continue,
        };

        let r = Diagnostics::get_span_range(src, *span, OffsetMode::Utf16);
        let line = r.start.line;
        let start = r.start.character;

        // TODO: save the other semantic tokens so they don't have to be recalculated
        tokens.push(SemanticToken {
            delta_line: line - prev_line,
            delta_start: if line == prev_line {
                start - prev_start
            } else {
                start
            },
            length: span.len, // TODO: use UTF-16 length
            token_type,
            token_modifiers_bitset,
        });

        prev_line = line;
        prev_start = start;
    }

    tokens
}

fn visualize_location(scope: ScopeId, scopes: &Scopes) -> String {
    let mut backward = vec![];
    for (_, scope) in scopes.walk(scope) {
        if let Some(name) = scope.kind.name(scopes) {
            backward.push(name.data.to_owned());
        }
    }

    backward.reverse();
    backward.join("::") + "\n"
}

fn visualize_type_params(
    res: &mut String,
    params: &[UserTypeId],
    scopes: &Scopes,
    types: &mut Types,
) {
    if !params.is_empty() {
        *res += "<";
        for (i, id) in params.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }

            *res += &visualize_type(*id, scopes, types);
        }
        *res += ">";
    }
}

fn visualize_func(id: FunctionId, small: bool, scopes: &Scopes, types: &mut Types) -> String {
    let func = scopes.get(id);
    if let Some((union, id)) = func
        .constructor
        .and_then(|id| scopes.get(id).kind.as_union().zip(Some(id)))
    {
        let mut res = if small {
            String::new()
        } else {
            visualize_location(scopes.get(id).body_scope, scopes)
        };
        let variant = &func.name.data;
        visualize_variant_body(
            &mut res,
            union,
            variant,
            union.variants.get(variant).and_then(|inner| inner.ty),
            scopes,
            types,
            small,
        );
        return res;
    }

    let mut res = if small {
        String::new()
    } else {
        visualize_location(func.scope, scopes)
    };

    if !small {
        if func.public {
            res += "pub ";
        }

        if func.is_extern {
            res += "extern ";
        }

        if func.is_unsafe {
            write_de!(res, "unsafe fn {}", func.name.data)
        } else {
            write_de!(res, "fn {}", func.name.data)
        }
    } else if func.is_unsafe {
        write_de!(res, "unsafe fn")
    } else {
        write_de!(res, "fn")
    }

    visualize_type_params(&mut res, &func.type_params, scopes, types);

    res += "(";
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            res += ", ";
        }

        if param.keyword {
            res += "kw ";
        }

        if param.label == THIS_PARAM {
            match types[param.ty] {
                Type::MutPtr(_) => res += "mut ",
                Type::Ptr(_) => {}
                _ => res += "my ",
            }
        }

        if param
            .label
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_digit() || ch == '$')
        {
            res += "_";
        } else {
            res += &param.label;
        }

        if param.label != THIS_PARAM {
            if param.default.is_some() {
                res += "?";
            }
            res += ": ";
            res += &param.ty.name(scopes, types);
        }
    }
    if func.variadic {
        if func.params.is_empty() {
            res += "...";
        } else {
            res += ", ...";
        }
    }

    res += ")";
    if func.ret != TypeId::VOID {
        res += ": ";
        res += &func.ret.name(scopes, types);
    }

    res
}

fn visualize_var(id: VariableId, scopes: &Scopes, types: &mut Types) -> String {
    let var = scopes.get(id);
    let mut res = String::new();
    if var.is_static {
        res += &visualize_location(var.scope, scopes);
    }
    if var.public {
        res += "pub ";
    }
    res += match (var.is_static, var.mutable) {
        (true, true) => "static mut",
        (true, false) => "static",
        (false, true) => "mut",
        (false, false) => "let",
    };
    write_de!(res, " {}: {}", var.name.data, var.ty.name(scopes, types));
    res
}

fn visualize_type(id: UserTypeId, scopes: &Scopes, types: &mut Types) -> String {
    let ut = scopes.get(id);
    let mut res = String::new();
    let print_body = |types: &mut Types, res: &mut String, mut wrote: bool| {
        if wrote && !ut.members.is_empty() {
            *res += "\n";
        }
        for (name, member) in ut.members.iter() {
            let header = if ut.kind.is_union() {
                "shared "
            } else if member.public {
                "pub "
            } else {
                ""
            };

            write_de!(
                res,
                "\n\t{header}{name}: {},",
                member.ty.name(scopes, types)
            );
            wrote = true;
        }

        if wrote {
            *res += "\n}";
        } else {
            *res += "}";
        }
    };

    if !ut.kind.is_template() {
        res += &visualize_location(ut.scope, scopes);
    }

    if ut.type_params.is_empty()
        && matches!(
            ut.kind,
            UserTypeKind::Struct
                | UserTypeKind::PackedStruct(_)
                | UserTypeKind::Union(_)
                | UserTypeKind::UnsafeUnion
        )
        && !ut.recursive
    {
        let ut = GenericUserType::new(id, Default::default());
        let (sz, align) = types.insert(Type::User(ut)).size_and_align(scopes, types);
        writeln_de!(res, "// size = {sz} ({sz:#x}), align = {align:#x}");
    }

    if ut.public {
        res += "pub ";
    }
    match &ut.kind {
        UserTypeKind::PackedStruct(_) | UserTypeKind::Struct => {
            if ut.kind.is_packed_struct() {
                write_de!(res, "packed ");
            }

            write_de!(res, "struct {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes, types);
            res += " {";
            print_body(types, &mut res, false);
        }
        UserTypeKind::UnsafeUnion => {
            write_de!(res, "unsafe union {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes, types);
            res += " {";
            print_body(types, &mut res, false);
        }
        UserTypeKind::Union(union) => {
            write_de!(res, "union {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes, types);
            write_de!(res, ": {} {{", union.tag.name(scopes, types));
            for (name, variant) in union.variants.iter() {
                res += "\n\t";
                visualize_variant_body(&mut res, union, name, variant.ty, scopes, types, false);
                res += ",";
            }
            print_body(types, &mut res, !union.variants.is_empty());
        }
        UserTypeKind::Template => {
            res += &ut.name.data;
            for (i, tr) in ut.impls.iter().flat_map(|imp| imp.as_checked()).enumerate() {
                if i > 0 {
                    res += " + ";
                } else {
                    res += ": ";
                }

                res += &scopes.get(tr.id).name.data;
                if !tr.ty_args.is_empty() {
                    res += "<";
                    for (i, id) in tr.ty_args.iter().enumerate() {
                        if i > 0 {
                            res.push_str(", ");
                        }

                        res += &id.1.name(scopes, types);
                    }
                    res += ">";
                }
            }
        }
        UserTypeKind::Trait(_, _) => {
            write_de!(res, "trait {}", ut.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes, types);
        }
        UserTypeKind::Extension(ty) => {
            write_de!(res, "extension {}", ut.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes, types);
            write_de!(res, " for {}", ty.name(scopes, types));
        }
        UserTypeKind::AnonStruct => {}
        UserTypeKind::Tuple => {}
    }

    res
}

fn visualize_variant_body(
    res: &mut String,
    union: &Union,
    name: &str,
    ty: Option<TypeId>,
    scopes: &Scopes,
    types: &mut Types,
    small: bool,
) {
    *res += name;
    match ty.map(|id| (id, &types[id])) {
        Some((_, Type::User(ut))) => {
            let ut = ut.clone();
            let inner = scopes.get(ut.id);
            if inner.kind.is_anon_struct() {
                *res += " {";
                for (i, (name, _)) in inner.members.iter().enumerate() {
                    write_de!(
                        res,
                        "{}{name}: {}",
                        if i > 0 { ", " } else { " " },
                        ut.ty_args
                            .get_index(i)
                            .map(|v| *v.1)
                            .unwrap_or(TypeId::UNKNOWN)
                            .name(scopes, types)
                    )
                }
                *res += " }";
            } else if inner.kind.is_tuple() {
                *res += "(";
                for i in 0..inner.members.len() {
                    if i > 0 {
                        *res += ", ";
                    }
                    *res += &ut
                        .ty_args
                        .get_index(i)
                        .map(|v| *v.1)
                        .unwrap_or(TypeId::UNKNOWN)
                        .name(scopes, types)
                }
                *res += ")";
            }
        }
        Some((id, _)) => write_de!(res, "({})", id.name(scopes, types)),
        None => {}
    }
    if !small {
        write_de!(res, " = {}", union.discriminant(name).unwrap());
    }
}
