use std::collections::HashMap;
use std::fmt::Write;
use std::path::Path;

use dashmap::DashMap;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::ast::parsed::Linkage;
use crate::error::{Diagnostics, FileId, OffsetMode};
use crate::lexer::Span;
use crate::project::Project;
use crate::sym::{FunctionId, ScopeId, Scopes, Union, UserTypeId, UserTypeKind, VariableId};
use crate::typecheck::{LspInput, LspItem};
use crate::typeid::{GenericUserType, Type, TypeId, Types};
use crate::{
    project_from_file, CachingSourceProvider, Compiler, FileSourceProvider, SourceProvider,
    THIS_PARAM,
};

macro_rules! write_de {
    ($dst:expr, $($arg:tt)*) => {
        _ = write!($dst, $($arg)*)
    };
}

macro_rules! writeln_de {
    ($dst:expr, $($arg:tt)*) => {
        _ = writeln!($dst, $($arg)*)
    };
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

#[derive(Debug)]
struct Document {
    text: String,
    inlay_hints: Vec<InlayHint>,
}

#[derive(Debug)]
pub struct LspBackend {
    client: Client,
    documents: DashMap<Url, Document>,
    config: tokio::sync::Mutex<Configuration>,
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
        self.client
            .log_message(MessageType::INFO, "Server initialized!")
            .await;
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        *self.config.lock().await = serde_json::from_value(params.settings).unwrap_or_default();
        if let Some(entry) = self.documents.iter().next() {
            _ = self.check_project(entry.key(), None, None).await;
        }
    }

    async fn did_open(&self, mut params: DidOpenTextDocumentParams) {
        self.documents
            .entry(params.text_document.uri.clone())
            .and_modify(|doc| doc.text = std::mem::take(&mut params.text_document.text))
            .or_insert(Document {
                text: std::mem::take(&mut params.text_document.text),
                inlay_hints: vec![],
            });
        _ = self
            .check_project(&params.text_document.uri, None, None)
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.documents
            .entry(params.text_document.uri.clone())
            .and_modify(|doc| doc.text = std::mem::take(&mut params.content_changes[0].text))
            .or_insert(Document {
                text: std::mem::take(&mut params.content_changes[0].text),
                inlay_hints: vec![],
            });
        _ = self
            .check_project(&params.text_document.uri, None, None)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        Ok(self
            .documents
            .get(&params.text_document.uri)
            .map(|c| c.inlay_hints.clone()))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let (file, mut proj) = self.check_project(uri, Some(pos), None).await?;
        let scopes = &proj.scopes;
        let Some(span) = proj.hover.as_ref().and_then(|hover| {
            Some(match hover {
                &LspItem::Var(id) => scopes.get(id).name.span,
                &LspItem::Type(id) => scopes.get(id).name.span,
                &LspItem::Module(id) => scopes[id].kind.name(scopes).unwrap().span,
                &LspItem::Fn(id, _) => scopes.get(id).name.span,
                LspItem::Property(id, member) => {
                    let ut = scopes.get(*id);
                    return ut.members.get(member).map(|m| m.span).or_else(|| {
                        ut.kind
                            .as_union()
                            .and_then(|u| u.variants.get(member).map(|v| v.span))
                    });
                }
                _ => return None,
            })
        }) else {
            return Ok(None);
        };

        let diag = &mut proj.diag;
        let path = diag.file_path(span.file);
        let uri = get_uri((file, uri), (span.file, path));
        let range = if let Some(doc) = self.documents.get(&uri) {
            Diagnostics::get_span_range(&doc.text, span, OffsetMode::Utf16)
        } else {
            let Some(file) = std::fs::read_to_string(path).ok() else {
                return Ok(None);
            };
            Diagnostics::get_span_range(&file, span, OffsetMode::Utf16)
        };

        Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
            uri, range,
        ))))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        if let Some(ctx) = params.context.and_then(|ctx| ctx.trigger_character) {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "looking for completion at line {}, character {} due to trigger '{}'",
                        pos.line, pos.character, ctx
                    ),
                )
                .await;
        } else {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "looking for completion at line {}, character {}",
                        pos.line, pos.character
                    ),
                )
                .await;
        }

        let (_, mut proj) = self.check_project(uri, None, Some(pos)).await?;
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
                    LspItem::Property(id, name) => {
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
                                    UserTypeKind::Trait(_) => {
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
                                UserTypeKind::Trait(_) => CompletionItemKind::INTERFACE,
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
                        CompletionItem {
                            label: var.name.data.clone(),
                            kind: Some(if var.is_static {
                                CompletionItemKind::CONSTANT
                            } else {
                                CompletionItemKind::VARIABLE
                            }),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(var.name.data.clone()),
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
        let (_, mut proj) = self.check_project(uri, Some(pos), None).await?;
        let Some(item) = proj.hover.as_ref() else {
            return Ok(None);
        };
        let scopes = &proj.scopes;
        let types = &mut proj.types;
        let str = match item {
            &LspItem::Var(id) => Some(visualize_var(id, scopes, types)),
            &LspItem::Fn(id, _) => Some(visualize_func(id, false, scopes, types)),
            &LspItem::Type(id) => Some(visualize_type(id, scopes, types)),
            LspItem::Property(id, name) => {
                let ut = scopes.get(*id);
                let ty = ut
                    .members
                    .get(name)
                    .map(|m| m.ty)
                    .unwrap_or(TypeId::UNKNOWN);
                Some(format!(
                    "{}{name}: {}",
                    visualize_location(ut.body_scope, scopes),
                    ty.name(scopes, types)
                ))
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

        Ok(str.map(|value| Hover {
            range: None,
            contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                language: "ctl".into(),
                value,
            })),
        }))
    }

    async fn shutdown(&self) -> Result<()> {
        self.documents.clear();
        Ok(())
    }
}

impl LspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            config: Default::default(),
            documents: Default::default(),
        }
    }

    async fn check_project(
        &self,
        uri: &Url,
        hover: Option<Position>,
        completion: Option<Position>,
    ) -> Result<(FileId, Project)> {
        let path = if !uri.scheme().is_empty() {
            Path::new(&uri.as_str()[uri.scheme().len() + 3..])
        } else {
            Path::new(uri.as_str())
        };
        let project = project_from_file(get_file_project(path), vec![], false, false);
        let parsed = Compiler::with_provider(LspFileProvider::new(&self.documents)).parse(project);
        let parsed = match parsed {
            Ok(parsed) => parsed,
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("{err}"))
                    .await;
                return Err(Error::internal_error());
            }
        };
        let file = parsed
            .diagnostics()
            .paths()
            .find(|p| p.1 == path)
            .unwrap()
            .0;
        let checked = parsed.typecheck(LspInput {
            hover: hover.map(|p| {
                position_to_span(
                    &self.documents.get(uri).unwrap().text,
                    file,
                    p.line,
                    p.character,
                )
            }),
            completion: completion.map(|p| {
                position_to_span(
                    &self.documents.get(uri).unwrap().text,
                    file,
                    p.line,
                    p.character,
                )
            }),
        });

        let mut proj = checked.project();
        let diag = &mut proj.diag;
        let mut all = HashMap::<Url, Vec<Diagnostic>>::new();
        let mut cache = CachingSourceProvider::new();
        for (severity, err) in diag
            .errors()
            .iter()
            .map(|err| (DiagnosticSeverity::ERROR, err))
            .chain(
                diag.warnings()
                    .iter()
                    .map(|err| (DiagnosticSeverity::WARNING, err)),
            )
        {
            let path = diag.file_path(err.span.file);
            let range = if let Some(doc) = Url::from_file_path(path)
                .ok()
                .and_then(|uri| self.documents.get(&uri))
            {
                Diagnostics::get_span_range(&doc.text, err.span, OffsetMode::Utf16)
            } else if let Ok(Some(range)) = cache.get_source(path, |data| {
                Diagnostics::get_span_range(data, err.span, OffsetMode::Utf16)
            }) {
                range
            } else {
                continue;
            };

            let entry = all
                .entry(get_uri((file, uri), (err.span.file, path)))
                .or_default();
            entry.push(Diagnostic {
                range,
                severity: Some(severity),
                source: Some("ctlsp".into()),
                message: err.message.clone(),
                ..Diagnostic::default()
            });
        }
        for (_, path) in diag.paths() {
            if let Ok(uri) = Url::from_file_path(path) {
                all.entry(uri).or_default();
            }
        }

        for (uri, diags) in all.into_iter() {
            self.client.publish_diagnostics(uri, diags, None).await;
        }

        if let Some(mut doc) = self.documents.get_mut(uri) {
            doc.inlay_hints.clear();
            for (_, var) in proj.scopes.vars() {
                if var.name.span.file != file || var.has_hint || var.name.data.starts_with('$') {
                    continue;
                }

                let r = Diagnostics::get_span_range(&doc.text, var.name.span, OffsetMode::Utf16);
                doc.inlay_hints.push(InlayHint {
                    position: r.end,
                    label: InlayHintLabel::String(format!(
                        ": {}",
                        var.ty.name(&proj.scopes, &mut proj.types)
                    )),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: Default::default(),
                    tooltip: Default::default(),
                    padding_left: Default::default(),
                    padding_right: Default::default(),
                    data: Default::default(),
                });
            }
        }

        Ok((file, proj))
    }
}

#[derive(derive_more::Constructor)]
pub struct LspFileProvider<'a> {
    documents: &'a DashMap<Url, Document>,
}

impl SourceProvider for LspFileProvider<'_> {
    fn get_source<T>(
        &mut self,
        path: &Path,
        get: impl FnOnce(&str) -> T,
    ) -> anyhow::Result<Option<T>> {
        if let Some(doc) = Url::from_file_path(path)
            .ok()
            .and_then(|uri| self.documents.get(&uri))
        {
            Ok(Some(get(&doc.text)))
        } else {
            FileSourceProvider.get_source(path, get)
        }
    }
}

fn get_uri((file, furi): (FileId, &Url), (spanid, path): (FileId, &Path)) -> Url {
    if spanid == file {
        furi.clone()
    } else {
        Url::from_file_path(path).unwrap()
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

        match func.linkage {
            Linkage::Import => res += "import ",
            Linkage::Export => res += "export ",
            Linkage::Internal => {}
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

        if param.label == THIS_PARAM && types.get(param.ty).is_mut_ptr() {
            res += "mut ";
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
            UserTypeKind::Struct | UserTypeKind::Union(_) | UserTypeKind::UnsafeUnion
        )
    {
        let ut = GenericUserType::new(id, Default::default());
        let (sz, align) = types.insert(Type::User(ut)).size_and_align(scopes, types);
        writeln_de!(res, "// size = {sz}, align = {align}");
    }

    if ut.public {
        res += "pub ";
    }
    match &ut.kind {
        UserTypeKind::Struct => {
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
        UserTypeKind::Trait(_) => {
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
    match ty.map(|id| (id, types.get(id))) {
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
        write_de!(res, " = {}", union.variant_tag(name).unwrap());
    }
}
