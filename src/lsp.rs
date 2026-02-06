use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};

use dashmap::DashMap;
use tokio::sync::MutexGuard;
use tower_lsp::{
    Client, LanguageServer,
    jsonrpc::{Error, Result},
    lsp_types::*,
};

use crate::{
    CachingSourceProvider, Compiler, FileSourceProvider, Located, SourceProvider,
    error::{Diagnostics, FileId, OffsetMode},
    intern::{StrId, Strings},
    lexer::Span,
    project::Project,
    sym::{
        Function, FunctionId, ScopeId, Scoped, Scopes, Union, UserTypeId, UserTypeKind, VariableId,
        VariableKind,
    },
    typecheck::{LspInput, LspItem},
    typeid::{BitSizeResult, GenericUserType, Type, TypeId, Types},
    write_if,
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

macro_rules! error {
    ($self: expr, $($arg: tt)*) => {{
        $self.client.log_message(MessageType::ERROR, format!($($arg)*)).await
    }};
}

macro_rules! debug {
    ($self: expr, $($arg: tt)*) => {{
        #[cfg(debug_assertions)]
        $self.client.log_message(MessageType::LOG, format!($($arg)*)).await
    }};
}

#[derive(serde::Deserialize, Clone, Copy, Debug)]
struct LspConfiguration {
    #[serde(rename = "debounceMs")]
    _debounce_ms: u32,
    #[serde(rename = "maxNumberOfProblems")]
    _max_problems: usize,
}

impl Default for LspConfiguration {
    fn default() -> Self {
        Self { _debounce_ms: 250, _max_problems: 100 }
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

        if let Self::Initialized(value) = self { value.as_mut() } else { None }
    }
}

#[derive(Default)]
struct FileInfoCache {
    lsp_items: Vec<(LspItem, Span)>,
    inlay_hints: Lazy<Vec<InlayHint>>,
    semantic_tokens: Lazy<Vec<SemanticToken>>,
    document_symbols: Lazy<Vec<DocumentSymbol>>,
}

struct LastChecked {
    root: PathBuf,
    proj: Project,
    diagnostics: HashSet<Url>,
}

impl LastChecked {
    pub async fn clear_diagnostics(&mut self, client: &Client) {
        for doc in std::mem::take(&mut self.diagnostics) {
            client.publish_diagnostics(doc, vec![], None).await;
        }
    }
}

pub struct LspBackend {
    client: Client,
    /// Files that the client has ownership of
    open_files: DashMap<Url, String>,
    documents: DashMap<Url, FileInfoCache>,
    config: tokio::sync::Mutex<LspConfiguration>,
    project: tokio::sync::Mutex<Option<LastChecked>>,
}

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
mod token {
    use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType};

    pub const TOKEN_TYPES: [SemanticTokenType; 7] = [
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::TYPE,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PARAMETER,
    ];

    pub const ENUM_MEMBER: u32 = 0;
    pub const TYPE: u32 = 1;
    pub const VARIABLE: u32 = 2;
    pub const FUNCTION: u32 = 3;
    pub const NAMESPACE: u32 = 4;
    pub const KEYWORD: u32 = 5;
    pub const PARAMETER: u32 = 6;

    pub const TOKEN_MODS: [SemanticTokenModifier; 4] = [
        SemanticTokenModifier::new("mutable"),
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::STATIC,
        SemanticTokenModifier::new("constant"),
    ];

    pub mod mods {
        pub const NONE: u32 = 0;
        pub const MUTABLE: u32 = 1 << 0;
        pub const READONLY: u32 = 1 << 1;
        pub const STATIC: u32 = 1 << 2;
        pub const CONSTANT: u32 = 1 << 3;
    }
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
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
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
    }

    async fn did_open(&self, mut params: DidOpenTextDocumentParams) {
        debug!(self, "did_open: '{}'", params.text_document.uri);
        self.open_files
            .entry(params.text_document.uri.clone())
            .and_modify(|text| *text = std::mem::take(&mut params.text_document.text))
            .or_insert_with(|| std::mem::take(&mut params.text_document.text));
        _ = self.check_project(&params.text_document.uri, None, false).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        debug!(self, "did_change: '{}'", params.text_document.uri);
        self.open_files
            .entry(params.text_document.uri.clone())
            .and_modify(|text| *text = std::mem::take(&mut params.content_changes[0].text))
            .or_insert_with(|| std::mem::take(&mut params.content_changes[0].text));
        _ = self.check_project(&params.text_document.uri, None, true).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        debug!(self, "did_close: '{}'", params.text_document.uri);
        self.open_files.remove(&params.text_document.uri);
        if self.open_files.is_empty()
            && let Some(mut prev) = self.project.lock().await.take()
        {
            prev.clear_diagnostics(&self.client).await;
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        self.with_proj_and_doc(&params.text_document.uri, async |doc, path, file, proj| {
            doc.inlay_hints
                .get_or_try(|| {
                    self.with_source(path, &mut FileSourceProvider, |src| {
                        get_inlay_hints(proj, src, file)
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
            let span = match *hover {
                LspItem::Var(id) => scopes.get(id).name.span,
                LspItem::Type(id) => scopes.get(id).name.span,
                LspItem::Trait(id) => scopes.get(id).name.span,
                LspItem::Module(id, _) => scopes[id].kind.name(scopes).unwrap().span,
                LspItem::Fn(id) => scopes.get(id).name.span,
                LspItem::Alias(id) => scopes.get(id).name.span,
                LspItem::Property(_, id, ref member) => {
                    let ut = scopes.get(id);
                    ut.members.get(member).map(|m| m.span).or_else(|| {
                        ut.kind.as_union().and_then(|u| u.variants.get(member).map(|v| v.span))
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
        let Some(checked) = guard.as_mut() else {
            return Ok(None);
        };
        let proj = &mut checked.proj;

        let Some(completions) = proj.completions.as_ref() else {
            return Ok(None);
        };
        let completions = completions
            .items
            .iter()
            .flat_map(|item| get_completion(proj, item, completions.method))
            .collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        self.find_lsp_item(uri, pos, async |item, proj| {
            let str = match *item {
                LspItem::Var(id) | LspItem::FnParamLabel(id, _) => Some(visualize_var(id, proj)),
                LspItem::Fn(id) => Some(visualize_func(id, false, proj)),
                LspItem::Type(id) => Some(visualize_type(id, proj)),
                LspItem::Trait(id) => {
                    let tr_data = proj.scopes.get(id);
                    let mut res = String::new();
                    res += &visualize_location(tr_data.scope, proj);
                    if tr_data.public {
                        res += "pub ";
                    }

                    write_de!(res, "trait {}", proj.strings.resolve(&tr_data.name.data));
                    visualize_type_params(&mut res, &tr_data.type_params, proj);

                    Some(res)
                }
                LspItem::Alias(id) => {
                    let alias = proj.scopes.get(id);
                    let mut str = String::new();
                    write_if!(alias.public, str, "pub ");
                    write_de!(str, "type {}", proj.strings.resolve(&alias.name.data));
                    visualize_type_params(&mut str, &alias.type_params, proj);
                    write_de!(str, " = {}", proj.fmt_ty(alias.ty.unwrap_or_default()));
                    Some(str)
                }
                LspItem::Property(src_ty, id, name) => {
                    let ut = proj.scopes.get(id);
                    let mem = ut.members.get(&name);
                    let public = if ut.kind.is_union() {
                        "shared "
                    } else if mem.is_some_and(|m| m.public) {
                        "pub "
                    } else {
                        ""
                    };
                    let ty = mem.map_or(TypeId::UNKNOWN, |m| m.ty);
                    if matches!(ut.kind, UserTypeKind::Tuple) {
                        let real =
                            src_ty.map(|src| ty.with_ut_templates(&proj.types, src)).unwrap_or(ty);
                        Some(format!("{public}{}: {}", proj.str(name), proj.fmt_ty(real)))
                    } else {
                        let offs = hover_property_text(proj, id, src_ty, ty, name);
                        Some(format!(
                            "{}{offs}{public}{}: {}",
                            visualize_location(ut.body_scope, proj),
                            proj.str(name),
                            proj.fmt_ty(ty),
                        ))
                    }
                }
                LspItem::Module(id, _) => {
                    let scope = &proj.scopes[id];
                    let mut res = String::new();
                    if let Some(parent) = scope.parent.filter(|parent| *parent != ScopeId::ROOT) {
                        res += &visualize_location(parent, proj);
                    }
                    if scope.public {
                        res += "pub ";
                    }
                    Some(format!(
                        "{res}mod {}",
                        proj.strings.resolve(&scope.kind.name(&proj.scopes).unwrap().data)
                    ))
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
        self.with_proj_and_doc(&params.text_document.uri, async |doc, path, _, proj| {
            let tokens = doc.semantic_tokens.get_or_try(|| {
                self.with_source(path, &mut FileSourceProvider, |src| {
                    let [mut prev_line, mut prev_start] = [0; 2];
                    doc.lsp_items
                        .iter()
                        .flat_map(|item| {
                            get_semantic_token(
                                &proj.scopes,
                                &proj.types,
                                item,
                                src,
                                &mut prev_line,
                                &mut prev_start,
                            )
                        })
                        .collect()
                })
            });

            Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens.cloned().unwrap_or_default(),
            }))
        })
        .await
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let pos = params.text_document_position.position;
        let uri = params.text_document_position.text_document.uri;
        debug!(self, "references: {uri}");
        self.find_lsp_item(&uri, pos, async |src, proj| {
            let mut locations = vec![];
            self.find_references(proj, src, false, |_, _, loc| locations.push(loc));
            Some(locations)
        })
        .await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let pos = params.text_document_position.position;
        let uri = params.text_document_position.text_document.uri;
        debug!(self, "rename: {uri}");
        self.find_lsp_item(&uri, pos, async |item, proj| {
            if matches!(item, LspItem::BuiltinType(_)) {
                return Some(WorkspaceEdit {
                    changes: None,
                    document_changes: None,
                    change_annotations: None,
                });
            }

            let mut changes = HashMap::<Url, Vec<TextEdit>>::new();
            self.find_references(proj, item, true, |src, span, loc| {
                if span.len as usize == crate::intern::THIS_TYPE.len()
                    && get_span_text(src, span).is_some_and(|text| text == crate::intern::THIS_TYPE)
                {
                    return;
                }

                let map = changes.entry(loc.uri).or_default();
                map.push(TextEdit { range: loc.range, new_text: params.new_name.clone() });
            });

            debug!(self, "  -> {changes:?}");
            Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            })
        })
        .await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        debug!(self, "document_symbol: {}", params.text_document.uri);
        self.with_proj_and_doc(&params.text_document.uri, async |doc, path, file, proj| {
            let symbols = doc.document_symbols.get_or_try(|| {
                self.with_source(path, &mut FileSourceProvider, |src| {
                    get_document_symbols(proj, src, file)
                })
            });

            Some(DocumentSymbolResponse::Nested(symbols?.clone()))
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

    async fn make_diagnostic(
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

        let Ok(path) = Url::from_file_path(path) else {
            error!(self, "Couldn't turn path '{}' into a Url", path.display());
            return;
        };

        let entry = all.entry(path).or_default();
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
        let mut root = get_file_project(path);
        let mut proj_lock_guard = self.project.lock().await;
        if let Some(prev) = proj_lock_guard.as_ref()
            && prev.proj.diag.paths().any(|(_, other)| path == *other)
        {
            if !change {
                debug!(self, " -> Checking file in submodule of previous project, skip");
                return Ok(proj_lock_guard);
            }

            root = &prev.root;
            debug!(self, " -> Checking file in submodule of previous project, using prev data");
        }

        self.documents.clear();

        let root = root.to_owned();
        let parsed = match Compiler::with_provider(LspFileProvider::new(&self.open_files))
            .parse(&root, Default::default())
        {
            Ok(parsed) => parsed.modify_conf(|conf| {
                // Always show tests in the LSP
                conf.test_args = Some(crate::TestArgs { test: None, modules: None })
            }),
            Err(err) => {
                error!(self, "{err}");
                return Err(Error::internal_error());
            }
        };

        let file = parsed.diagnostics().get_file_id(path);
        let checked = parsed.typecheck(Some(LspInput {
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
        }));

        let mut proj = checked.project();
        let mut all = HashMap::<Url, Vec<Diagnostic>>::new();
        let mut cache = CachingSourceProvider::new();
        for err in proj.diag.diagnostics() {
            self.make_diagnostic(
                &proj.diag,
                &mut cache,
                err.span,
                &err.message,
                match err.severity {
                    crate::ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                    crate::ErrorSeverity::Error => DiagnosticSeverity::ERROR,
                },
                &mut all,
            )
            .await;
        }

        for &span in proj.diag.inactive() {
            self.make_diagnostic(
                &proj.diag,
                &mut cache,
                span,
                "this region is disabled",
                DiagnosticSeverity::HINT,
                &mut all,
            )
            .await;
        }

        if let Some(items) = &mut proj.lsp_items {
            items.sort_by_key(|(_, span)| span.pos);
            // Reverse to make sure the LAST duplicate is preserved.
            // Duplicates are occasionally produced when checking patterns, but the last is the most
            // relevant one.
            // Can't just reverse sort because we only care that the equal elements are reversed,
            // which sort_by_key will not do.
            items.reverse();
            items.dedup_by_key(|(_, span)| (span.file, span.pos));
            items.reverse();
        }

        let mut sent = HashSet::new();
        for (id, path) in proj.diag.paths() {
            let Ok(uri) = Url::from_file_path(path) else {
                continue;
            };

            if let Some(diagnostics) = all.remove(&uri) {
                self.client.publish_diagnostics(uri.clone(), diagnostics, None).await;
                sent.insert(uri.clone());
                if let Some(prev) = proj_lock_guard.as_mut() {
                    prev.diagnostics.remove(&uri);
                }
            }

            let mut doc = self.documents.entry(uri).or_default();
            doc.lsp_items = proj
                .lsp_items
                .as_ref()
                .map(|items| items.iter().filter(|(_, span)| span.file == id).cloned().collect())
                .unwrap_or_default();
            doc.inlay_hints = Lazy::None;
            doc.semantic_tokens = Lazy::None;
            doc.document_symbols = Lazy::None;
        }

        if let Some(prev) = proj_lock_guard.as_mut() {
            prev.clear_diagnostics(&self.client).await;
        }

        *proj_lock_guard = Some(LastChecked { root, proj, diagnostics: sent });

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

        let Some(checked) = lock.as_mut() else {
            return Ok(None);
        };

        let path = Self::uri_to_path(uri);
        let Some(file) = checked.proj.diag.get_file_id(path) else {
            return Ok(None);
        };

        let Some(mut doc) = self.documents.get_mut(uri) else {
            return Ok(None);
        };

        Ok(func(&mut doc, path, file, &mut checked.proj).await)
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
            let (item, _) =
                doc.lsp_items.iter().find(|(_, span)| LspInput::matches(Some(user), *span))?;
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
        if let Some(text) = Url::from_file_path(path).ok().and_then(|uri| self.open_files.get(&uri))
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

    fn find_references(
        &self,
        proj: &Project,
        src: &LspItem,
        rename: bool,
        mut next_range: impl FnMut(&str, Span, Location),
    ) {
        let is_constructor_for = |lhs: FunctionId, rhs: UserTypeId| {
            !proj.scopes.get(rhs).kind.is_union()
                && proj.scopes.get(lhs).constructor.is_some_and(|lhs| lhs == rhs)
        };

        let mut cache = CachingSourceProvider::new();
        let mut spans = HashSet::new();
        let Some(lsp_items) = &proj.lsp_items else {
            return;
        };

        for (item, span) in lsp_items.iter() {
            match (*src, *item) {
                (LspItem::Module(lhs, _), LspItem::Module(rhs, is_super))
                    if lhs == rhs && (!is_super || !rename) => {}

                (LspItem::Type(lhs), LspItem::Type(rhs)) if lhs == rhs => {}
                (LspItem::Trait(lhs), LspItem::Trait(rhs)) if lhs == rhs => {}
                (LspItem::Fn(lhs), LspItem::Fn(rhs)) if lhs == rhs => {}
                (LspItem::Type(lhs), LspItem::Fn(rhs)) if is_constructor_for(rhs, lhs) => {}
                (LspItem::Fn(lhs), LspItem::Type(rhs)) if is_constructor_for(lhs, rhs) => {}

                (LspItem::Alias(lhs), LspItem::Alias(rhs)) if lhs == rhs => {}
                (LspItem::Var(lhs), LspItem::Var(rhs)) if lhs == rhs => {}
                (LspItem::Var(lhs), LspItem::FnParamLabel(rhs, _)) if lhs == rhs => {}
                (LspItem::FnParamLabel(lhs, _), LspItem::Var(rhs)) if lhs == rhs => {}
                (LspItem::FnParamLabel(lhs, _), LspItem::FnParamLabel(rhs, _)) if lhs == rhs => {}
                (
                    LspItem::Property(_, lhs_ut, lhs_name),
                    LspItem::Property(_, rhs_ut, rhs_name),
                ) if lhs_ut == rhs_ut && lhs_name == rhs_name => {}
                // TODO: variant properties & FnParamLabel on constructor
                _ => continue,
            }

            if !spans.insert((span.pos, span.len)) {
                continue;
            }

            let path = proj.diag.file_path(span.file);
            let Ok(uri) = Url::from_file_path(path) else {
                continue;
            };

            _ = self.with_source(path, &mut cache, |src| {
                let range = Diagnostics::get_span_range(src, *span, OffsetMode::Utf16);
                next_range(src, *span, Location { uri, range });
            });
        }
    }
}

#[derive(derive_more::Constructor)]
pub struct LspFileProvider<'a> {
    open_files: &'a DashMap<Url, String>,
}

impl SourceProvider for LspFileProvider<'_> {
    fn get_source<T>(&mut self, path: &Path, get: impl FnOnce(&str) -> T) -> anyhow::Result<T> {
        if let Some(text) = Url::from_file_path(path).ok().and_then(|uri| self.open_files.get(&uri))
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

fn get_inlay_hints(proj: &Project, src: &str, file: FileId) -> Vec<InlayHint> {
    let mut hints = vec![];
    for (_, var) in proj.scopes.vars() {
        if var.name.span.file != file
            || var.has_hint
            || proj.strings.resolve(&var.name.data).starts_with('$')
        {
            continue;
        }

        let r = Diagnostics::get_span_range(src, var.name.span, OffsetMode::Utf16);
        hints.push(InlayHint {
            position: r.end,
            label: InlayHintLabel::String(format!(": {}", proj.fmt_ty(var.ty))),
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
fn get_semantic_token(
    scopes: &Scopes,
    types: &Types,
    (item, span): &(LspItem, Span),
    src: &str,
    prev_line: &mut u32,
    prev_start: &mut u32,
) -> Option<SemanticToken> {
    let mut mods = token::mods::NONE;
    let token_type = match *item {
        LspItem::Module(_, true) => token::KEYWORD,
        LspItem::Module(_, false) => token::NAMESPACE,
        LspItem::Property(_, _, _) => token::VARIABLE,
        LspItem::FnParamLabel(_, _) => token::VARIABLE,
        LspItem::Var(id) => {
            let var = scopes.get(id);
            if var.mutable || types[var.ty].is_mut_ptr() {
                mods |= token::mods::MUTABLE;
            } else if matches!(var.kind, VariableKind::Const | VariableKind::Static) {
                mods |= token::mods::READONLY | token::mods::CONSTANT;
            }

            if var.kind.is_static() {
                mods |= token::mods::STATIC;
            }

            if var.name.data == Strings::THIS_PARAM {
                token::KEYWORD
            } else if var.param {
                token::PARAMETER
            } else {
                token::VARIABLE
            }
        }
        LspItem::Type(id) => {
            if scopes.get(id).kind.is_extension() {
                return None;
            }
            token::TYPE
        },
        LspItem::Trait(_) => token::TYPE,
        LspItem::Alias(_) => token::TYPE,
        LspItem::BuiltinType(_) => token::TYPE,
        LspItem::Fn(id) => {
            if let Some(id) = scopes.get(id).constructor {
                if scopes.get(id).kind.is_union() { token::ENUM_MEMBER } else { token::TYPE }
            } else if scopes
                .get(id)
                .params
                .first()
                .is_some_and(|p| p.label == Strings::THIS_PARAM && types[p.ty].is_mut_ptr())
            {
                mods |= token::mods::MUTABLE;
                token::FUNCTION
            } else {
                token::FUNCTION
            }
        }
        LspItem::Literal(_) | LspItem::Attribute(_) => return None,
    };

    let r = Diagnostics::get_span_range(src, *span, OffsetMode::Utf16);
    let line = r.start.line;
    let start = r.start.character;

    // TODO: save the other semantic tokens so they don't have to be recalculated
    let token = SemanticToken {
        delta_line: line - *prev_line,
        delta_start: if line == *prev_line { start - *prev_start } else { start },
        length: span.len, // TODO: use UTF-16 length
        token_type,
        token_modifiers_bitset: mods,
    };

    *prev_line = line;
    *prev_start = start;
    Some(token)
}

fn get_document_symbols(proj: &Project, src: &str, file: FileId) -> Vec<DocumentSymbol> {
    let valid_name = |name: Located<StrId>| {
        name.span.file == file
            && !matches!(name.data, Strings::EMPTY | Strings::UNDERSCORE)
            && !proj.strings.resolve(&name.data).starts_with('$')
    };

    let get_range = |sel: Span, range: Span| {
        let r = range.pos..range.pos + range.len;
        if r.contains(&sel.pos) && r.contains(&(sel.pos + sel.len)) {
            Diagnostics::get_span_range(src, range, OffsetMode::Utf16)
        } else {
            Diagnostics::get_span_range(src, sel, OffsetMode::Utf16)
        }
    };

    let func_symbol = |func: &Scoped<Function>| {
        let mut kind = SymbolKind::FUNCTION;
        if let Some(ut) = func.constructor
            && proj.scopes.get(ut).kind.is_union()
        {
            kind = SymbolKind::ENUM_MEMBER;
        }

        let selection_range = Diagnostics::get_span_range(src, func.name.span, OffsetMode::Utf16);
        let range = get_range(func.name.span, func.full_span);
        DocumentSymbol {
            name: proj.strings.resolve(&func.name.data).into(),
            kind,
            range,
            selection_range,
            children: None,
            detail: None,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
        }
    };

    let dump_functions = |scope: ScopeId, children: &mut Vec<DocumentSymbol>| {
        for id in proj.scopes[scope].iter_fns() {
            if !valid_name(proj.scopes.get(id).name) {
                continue;
            }

            children.push(func_symbol(proj.scopes.get(id)));
        }
    };

    let mut result = vec![];
    for (_, tr) in proj.scopes.traits() {
        if !valid_name(tr.name) {
            continue;
        }

        let mut children = vec![];
        dump_functions(tr.body_scope, &mut children);

        let range = get_range(tr.name.span, tr.full_span);
        let selection_range = Diagnostics::get_span_range(src, tr.name.span, OffsetMode::Utf16);
        result.push(DocumentSymbol {
            name: proj.strings.resolve(&tr.name.data).into(),
            kind: SymbolKind::INTERFACE,
            range,
            selection_range,
            children: (!children.is_empty()).then_some(children),
            detail: None,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
        });
    }

    for (_, ut) in proj.scopes.types() {
        if !valid_name(ut.name) {
            continue;
        }

        let kind = match &ut.kind {
            UserTypeKind::Template => continue,
            UserTypeKind::Union(_) => SymbolKind::ENUM,
            _ => SymbolKind::STRUCT,
        };

        let mut children = vec![];
        dump_functions(ut.body_scope, &mut children);

        for imp in ut.iter_impls(proj, false) {
            if let Some(scope) = imp.scope {
                dump_functions(scope, &mut children);
            }
        }

        for (name, member) in ut.members.iter() {
            if !valid_name(Located::new(member.span, *name)) {
                continue;
            }

            let range = Diagnostics::get_span_range(src, member.span, OffsetMode::Utf16);
            children.push(DocumentSymbol {
                name: proj.strings.resolve(name).into(),
                kind: SymbolKind::FIELD,
                range,
                selection_range: range,
                children: None,
                detail: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }

        let range = get_range(ut.name.span, ut.full_span);
        let selection_range = Diagnostics::get_span_range(src, ut.name.span, OffsetMode::Utf16);
        result.push(DocumentSymbol {
            name: proj.strings.resolve(&ut.name.data).into(),
            kind,
            range,
            selection_range,
            children: (!children.is_empty()).then_some(children),
            detail: None,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
        });
    }

    for (_, func) in proj.scopes.functions() {
        if !valid_name(func.name)
            || proj.scopes[func.scope].kind.is_user_type()
            || proj.scopes[func.scope].kind.is_trait()
            || proj.scopes[func.scope].kind.is_impl()
            || func.constructor.is_some()
        {
            continue;
        }

        result.push(func_symbol(func));
    }

    for (_, var) in proj.scopes.vars() {
        if var.kind.is_local() || !valid_name(var.name) {
            continue;
        }

        let range = Diagnostics::get_span_range(src, var.name.span, OffsetMode::Utf16);
        result.push(DocumentSymbol {
            name: proj.strings.resolve(&var.name.data).into(),
            kind: if var.mutable { SymbolKind::VARIABLE } else { SymbolKind::CONSTANT },
            range,
            selection_range: range,
            children: None,
            detail: None,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
        });
    }

    result
}

fn get_completion(proj: &Project, item: &LspItem, method: bool) -> Option<CompletionItem> {
    let scopes = &proj.scopes;
    let strings = &proj.strings;
    Some(match *item {
        LspItem::Property(_, id, ref name) => {
            let ut = scopes.get(id);
            let member = ut.members.get(name).unwrap();
            let detail = proj.fmt_ty(member.ty).to_string();
            CompletionItem {
                label: strings.resolve(name).into(),
                kind: Some(CompletionItemKind::FIELD),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(detail.clone()),
                }),
                detail: Some(detail),
                ..Default::default()
            }
        }
        LspItem::Fn(id) => {
            let f = scopes.get(id);
            let mut owner = &scopes[f.scope];
            let mut detail = None;
            if let Some(imp_id) = owner.kind.as_impl() {
                owner = &scopes[owner.parent.unwrap()];

                if let Some(imp) = proj.impls.get(*imp_id).as_checked() {
                    detail = Some(format!(" (as {})", proj.fmt_tr(&imp.tr)));
                }
            }

            let mut empty_variant = false;
            if let Some(&id) = owner.kind.as_user_type() {
                if scopes.get(id).kind.is_extension() && detail.is_none() {
                    let module = scopes
                        .walk(scopes.get(id).scope)
                        .fold(None, |prev, scope| scope.1.kind.as_module().or(prev));
                    detail = Some(format!(" (from {})", strings.resolve(&module.unwrap().data)));
                }

                empty_variant = scopes.get(id).is_empty_variant(f.name.data);
            } else if let Some(&id) = owner.kind.as_trait() {
                detail = Some(format!(" (as {})", proj.str(scopes.get(id).name.data)));
            }

            let name = strings.resolve(&f.name.data);
            let insert_text = (!empty_variant).then(|| {
                let mut text = format!("{name}(");
                for (i, param) in f
                    .params
                    .iter()
                    .filter(|p| (!method || p.label != Strings::THIS_PARAM) && p.default.is_none())
                    .enumerate()
                {
                    if i > 0 {
                        text += ", ";
                    }
                    let label = strings.resolve(&param.label);
                    if param.keyword {
                        write_de!(text, "{label}: ${{{}:{label}}}", i + 1);
                    } else {
                        write_de!(text, "${{{}:{label}}}", i + 1);
                    }
                }
                text += ")";
                text
            });
            let desc = visualize_func(id, true, proj);
            CompletionItem {
                label: name.into(),
                label_details: Some(CompletionItemLabelDetails {
                    detail,
                    description: desc.clone().into(),
                }),
                kind: Some(if f.constructor.is_some_and(|id| scopes.get(id).kind.is_union()) {
                    CompletionItemKind::ENUM_MEMBER
                } else if f.constructor.is_some() {
                    CompletionItemKind::CONSTRUCTOR
                } else if f.params.first().is_some_and(|p| p.label == Strings::THIS_PARAM) {
                    CompletionItemKind::METHOD
                } else {
                    CompletionItemKind::FUNCTION
                }),
                detail: desc.into(),
                insert_text,
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        }
        LspItem::Type(id) => {
            let ut = scopes.get(id);
            let name = strings.resolve(&ut.name.data).to_string();
            CompletionItem {
                label: name.clone(),
                kind: Some(match ut.kind {
                    UserTypeKind::Union(_) => CompletionItemKind::ENUM,
                    UserTypeKind::Template => CompletionItemKind::TYPE_PARAMETER,
                    _ => CompletionItemKind::STRUCT,
                }),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(name.clone()),
                }),
                detail: Some(name),
                ..Default::default()
            }
        }
        LspItem::Trait(id) => {
            let ut = scopes.get(id);
            let name = strings.resolve(&ut.name.data).to_string();
            CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::INTERFACE),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(name.clone()),
                }),
                detail: Some(name),
                ..Default::default()
            }
        }
        LspItem::Alias(id) => {
            let alias = scopes.get(id);
            let name = strings.resolve(&alias.name.data).to_string();
            // TODO: pick CompletionItemKind based on alias.ty
            CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::STRUCT),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(name.clone()),
                }),
                detail: Some(name),
                ..Default::default()
            }
        }
        LspItem::Module(id, _) => {
            let name = strings.resolve(&scopes[id].kind.name(scopes).unwrap().data).to_string();
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
        LspItem::Var(id) | LspItem::FnParamLabel(id, _) => {
            let var = scopes.get(id);
            let name = strings.resolve(&var.name.data).to_string();
            CompletionItem {
                label: name.clone(),
                kind: Some(if var.kind.is_local() {
                    CompletionItemKind::VARIABLE
                } else {
                    CompletionItemKind::CONSTANT
                }),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(proj.fmt_ty(var.ty).to_string()),
                }),
                detail: Some(name),
                ..Default::default()
            }
        }
        LspItem::BuiltinType(name) => CompletionItem {
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
}

fn visualize_location(scope: ScopeId, proj: &Project) -> String {
    let mut backward = vec![];
    for (_, scope) in proj.scopes.walk(scope) {
        if let Some(name) = scope.kind.name(&proj.scopes) {
            backward.push(proj.strings.resolve(&name.data));
        }
    }

    backward.reverse();
    backward.join("::") + "\n"
}

fn visualize_type_params(res: &mut String, params: &[UserTypeId], proj: &Project) {
    if !params.is_empty() {
        *res += "<";
        for (i, id) in params.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }

            *res += &visualize_type(*id, proj);
        }
        *res += ">";
    }
}

fn visualize_func(id: FunctionId, small: bool, proj: &Project) -> String {
    let func = proj.scopes.get(id);
    if let Some((union, id)) =
        func.constructor.and_then(|id| proj.scopes.get(id).kind.as_union().zip(Some(id)))
    {
        let mut res = if small {
            String::new()
        } else {
            visualize_location(proj.scopes.get(id).body_scope, proj)
        };
        let variant = func.name.data;
        visualize_variant_body(
            &mut res,
            union,
            variant,
            union.variants.get(&variant).and_then(|inner| inner.ty),
            proj,
            small,
        );
        return res;
    } else if let Some(id) = func.constructor.filter(|id| proj.scopes.get(*id).kind.is_struct())
        && !small
    {
        return visualize_type(id, proj);
    } else if func.typ.is_test() {
        return format!("unittest \"{}\"", proj.str(func.name.data));
    }

    let mut res = if small { String::new() } else { visualize_location(func.scope, proj) };

    if !small {
        if func.public {
            res += "pub ";
        }

        if func.is_extern {
            res += "extern ";
        }

        if func.is_unsafe {
            write_de!(res, "unsafe fn {}", proj.strings.resolve(&func.name.data))
        } else {
            write_de!(res, "fn {}", proj.strings.resolve(&func.name.data))
        }
    } else if func.is_unsafe {
        write_de!(res, "unsafe fn")
    } else {
        write_de!(res, "fn")
    }

    visualize_type_params(&mut res, &func.type_params, proj);

    res += "(";
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            res += ", ";
        }

        if param.keyword {
            res += "kw ";
        }

        if param.label == Strings::THIS_PARAM {
            match proj.types[param.ty] {
                Type::MutPtr(_) => res += "mut ",
                Type::Ptr(_) => {}
                _ => res += "my ",
            }
        }

        let label = proj.strings.resolve(&param.label);
        if label.starts_with(|ch: char| ch.is_ascii_digit() || ch == '$') {
            res += "_";
        } else {
            res += label;
        }

        if param.label != Strings::THIS_PARAM {
            if param.default.is_some() {
                res += "?";
            }
            write_de!(res, ": {}", proj.fmt_ty(param.ty));
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
        write_de!(res, ": {}", proj.fmt_ty(func.ret));
    }

    res
}

fn visualize_var(id: VariableId, proj: &Project) -> String {
    let var = proj.scopes.get(id);
    let mut res = String::new();
    if !var.kind.is_local() {
        res += &visualize_location(var.scope, proj);
    }
    if var.public {
        res += "pub ";
    }
    res += match (var.kind, var.mutable) {
        (VariableKind::Const, _) => "const",
        (VariableKind::Static, true) => "static mut",
        (VariableKind::Static, false) => "static",
        (VariableKind::Normal | VariableKind::Capture, true) => "mut",
        (VariableKind::Normal | VariableKind::Capture, false) => "let",
    };
    let name =
        if var.name.data == Strings::EMPTY { "_" } else { proj.strings.resolve(&var.name.data) };
    write_de!(res, " {name}: {}", proj.fmt_ty(var.ty));
    res
}

fn visualize_type(id: UserTypeId, proj: &Project) -> String {
    let ut = proj.scopes.get(id);
    let mut res = String::new();
    let print_body = |res: &mut String, mut wrote: bool| {
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
                "\n\t{header}{}: {},",
                proj.strings.resolve(name),
                proj.fmt_ty(member.ty)
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
        res += &visualize_location(ut.scope, proj);
    }

    if ut.type_params.is_empty()
        && matches!(
            ut.kind,
            UserTypeKind::Struct(_, _) | UserTypeKind::Union(_) | UserTypeKind::UnsafeUnion
        )
        && !ut.recursive
    {
        let ut = GenericUserType::new(id, Default::default());
        let (sz, align) =
            proj.types.insert(Type::User(ut)).size_and_align(&proj.scopes, &proj.types);
        writeln_de!(res, "// size = {sz} ({sz:#x}), align = {align:#x}");
    }

    if ut.public {
        res += "pub ";
    }
    match &ut.kind {
        &UserTypeKind::Struct(_, packed) => {
            if packed {
                write_de!(res, "packed ");
            }

            write_de!(res, "struct {}", proj.strings.resolve(&ut.item.name.data));
            visualize_type_params(&mut res, &ut.type_params, proj);
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeKind::UnsafeUnion => {
            write_de!(res, "unsafe union {}", proj.strings.resolve(&ut.item.name.data));
            visualize_type_params(&mut res, &ut.type_params, proj);
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeKind::Union(union) => {
            write_de!(res, "union {}", proj.strings.resolve(&ut.item.name.data));
            visualize_type_params(&mut res, &ut.type_params, proj);
            write_de!(res, ": {} {{", proj.fmt_ty(union.tag));
            for (name, variant) in union.variants.iter() {
                res += "\n\t";
                visualize_variant_body(&mut res, union, *name, variant.ty, proj, false);
                res += ",";
            }
            print_body(&mut res, !union.variants.is_empty());
        }
        UserTypeKind::Template => {
            res += proj.strings.resolve(&ut.name.data);
            for (i, imp) in ut.iter_impls(proj, false).enumerate() {
                if i > 0 {
                    res += " + ";
                } else {
                    res += ": ";
                }

                write_de!(res, "{}", proj.fmt_tr(&imp.tr));
            }
        }
        &UserTypeKind::Extension(ty) => {
            write_de!(res, "extension");
            visualize_type_params(&mut res, &ut.type_params, proj);
            write_de!(res, " {}", proj.fmt_ty(ty.unwrap_or_default()));
        }
        UserTypeKind::Closure | UserTypeKind::Tuple => {}
    }

    res
}

#[allow(clippy::too_many_arguments)]
fn visualize_variant_body(
    res: &mut String,
    union: &Union,
    name: StrId,
    ty: Option<TypeId>,
    proj: &Project,
    small: bool,
) {
    *res += proj.strings.resolve(&name);
    match ty.map(|id| (id, &proj.types[id])) {
        Some((_, Type::User(ut))) => {
            let inner = proj.scopes.get(ut.id);
            if inner.kind.is_tuple() {
                *res += "(";
                for (i, (name, member)) in inner.members.iter().enumerate() {
                    if i > 0 {
                        *res += ", ";
                    }

                    let name = proj.strings.resolve(name);
                    if !name.starts_with(|ch: char| ch.is_ascii_digit()) {
                        write_de!(res, "{name}: ");
                    }

                    let ty = member.ty.with_templates(&proj.types, &ut.ty_args);
                    write_de!(res, "{}", proj.fmt_ty(ty));
                }
                *res += ")";
            }
        }
        Some((id, _)) => write_de!(res, "({})", proj.fmt_ty(id)),
        None => {}
    }
    if !small {
        write_de!(res, " = {}", union.discriminant(name).unwrap());
    }
}

fn get_span_text(s: &str, span: Span) -> Option<&str> {
    s.get(span.pos as usize..span.pos as usize + span.len as usize)
}

fn hover_property_text(
    proj: &Project,
    id: UserTypeId,
    _inst: Option<TypeId>,
    ty: TypeId,
    name: StrId,
) -> String {
    let ut_data = proj.scopes.get(id);
    if !ut_data.type_params.is_empty() {
        return "".into();
    }

    let ut = GenericUserType::from_id(&proj.scopes, &proj.types, id);
    if ut_data.kind.is_packed_struct() {
        let s = match ty.bit_size(&proj.scopes, &proj.types) {
            BitSizeResult::Size(n) => n,
            BitSizeResult::Tag(_, n) => n,
            _ => unreachable!(),
        };
        if let Some(o) = ut.bit_offset_of(&proj.scopes, &proj.types, name) {
            return format!("// bit size = {s} ({s:#x}), bit offset = {o} ({o:#x})\n");
        }
    } else {
        let (s, a) = ty.size_and_align(&proj.scopes, &proj.types);
        if let Some(o) = ut.offset_of(&proj.scopes, &proj.types, name) {
            return format!("// size = {s} ({s:#x}), align = {a:#x}, offset = {o} ({o:#x})\n");
        }
    }

    "".into()
}
