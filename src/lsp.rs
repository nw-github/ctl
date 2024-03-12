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
use crate::sym::{FunctionId, ScopeId, Scopes, Union, UserTypeData, UserTypeId, VariableId};
use crate::typecheck::{Completion, HoverItem, LspInput};
use crate::typeid::{GenericUserType, Type};
use crate::{
    project_from_file, CachingSourceProvider, Checked, Compiler, FileSourceProvider,
    SourceProvider, THIS_PARAM,
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
                    trigger_characters: Some(vec![".".into()]),
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
        let (file, checked) = self.check_project(uri, Some(pos), None).await?;
        let proj = checked.project();
        let Some(span) = proj.lsp.hover.as_ref().and_then(|hover| {
            Some(match hover {
                &HoverItem::Var(id) => proj.scopes.get(id).name.span,
                &HoverItem::Type(id) => proj.scopes.get(id).name.span,
                &HoverItem::Trait(id) => proj.scopes.get(id).name.span,
                &HoverItem::Extension(id) => proj.scopes.get(id).name.span,
                &HoverItem::Module(id) => proj.scopes[id].kind.name(&proj.scopes).unwrap().span,
                &HoverItem::Fn(id) => proj.scopes.get(id).name.span,
                HoverItem::Member(id, member) => {
                    let ut = proj.scopes.get(*id);
                    return ut.members.get(member).map(|m| m.span).or_else(|| {
                        ut.data
                            .as_union()
                            .and_then(|u| u.variants.get(member).map(|v| v.1))
                    });
                }
                _ => return None,
            })
        }) else {
            return Ok(None);
        };

        let diag = &checked.project().diag;
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
        if !params
            .context
            .and_then(|ctx| ctx.trigger_character)
            .is_some_and(|c| c == ".")
        {
            return Ok(None);
        }

        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let (_, checked) = self.check_project(uri, None, Some(pos)).await?;
        let project = checked.project();
        let scopes = &project.scopes;
        let Some(completions) = project.lsp.completions.as_ref() else {
            return Ok(None);
        };
        let completions = completions
            .iter()
            .map(|completion| match completion {
                Completion::Property(id, name) => {
                    let ut = scopes.get(*id);
                    let member = ut.members.get(name).unwrap();
                    CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(member.ty.name(scopes)),
                        ..Default::default()
                    }
                }
                &Completion::Method(id, ext) => {
                    let f = scopes.get(id);
                    let label = f.name.data.clone();
                    let mut insertion = format!("{label}(");
                    for (i, param) in f.params.iter().skip(1).enumerate() {
                        if i > 0 {
                            insertion += ", ";
                        }
                        if param.keyword {
                            write_de!(insertion, "{}: ${{{}:{}}}", param.label, i + 1, param.label);
                        } else {
                            write_de!(insertion, "${{{}:{}}}", i + 1, param.label);
                        }
                    }
                    insertion += ")";
                    let detail = Some(visualize_func(id, true, scopes));
                    CompletionItem {
                        label,
                        label_details: Some(CompletionItemLabelDetails {
                            detail: ext.map(|ext| format!(" (from {})", scopes.get(ext).name.data)),
                            description: detail.clone(),
                        }),
                        kind: Some(CompletionItemKind::METHOD),
                        detail,
                        insert_text: Some(insertion),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    }
                }
            })
            .collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let (_, checked) = self.check_project(uri, Some(pos), None).await?;
        let proj = checked.project();
        let Some(item) = proj.lsp.hover.as_ref() else {
            return Ok(None);
        };
        let scopes = &proj.scopes;
        let str = match item {
            &HoverItem::Var(id) => Some(visualize_var(id, scopes)),
            &HoverItem::Fn(id) => Some(visualize_func(id, false, scopes)),
            &HoverItem::Type(id) => Some(visualize_type(id, scopes)),
            HoverItem::Member(id, name) => {
                let ut = scopes.get(*id);
                let unk = Type::Unknown;
                let ty = ut.members.get(name).map(|m| &m.ty).unwrap_or(&unk);
                Some(format!(
                    "{}{name}: {}",
                    visualize_location(ut.body_scope, scopes),
                    ty.name(scopes)
                ))
            }
            &HoverItem::Module(id) => {
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
            &HoverItem::Trait(id) => {
                let tr = scopes.get(id);
                let mut res = format!(
                    "{}{}trait {}",
                    visualize_location(tr.scope, scopes),
                    if tr.public { "pub " } else { "" },
                    tr.name.data
                );
                visualize_type_params(&mut res, &tr.type_params, scopes);
                Some(res)
            }
            &HoverItem::Extension(id) => {
                let ext = scopes.get(id);
                let mut res = format!(
                    "{}{}extension {}",
                    visualize_location(ext.scope, scopes),
                    if ext.public { "pub " } else { "" },
                    ext.name.data
                );
                visualize_type_params(&mut res, &ext.type_params, scopes);
                write_de!(res, " for {}", ext.ty.name(scopes));

                Some(res)
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
    ) -> Result<(FileId, Compiler<Checked>)> {
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

        let diag = &checked.project().diag;
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

        let scopes = &checked.project().scopes;
        if let Some(mut doc) = self.documents.get_mut(uri) {
            doc.inlay_hints.clear();
            for (_, var) in scopes.vars() {
                if var.name.span.file != file || var.has_hint || var.name.data.starts_with('$') {
                    continue;
                }

                let r = Diagnostics::get_span_range(&doc.text, var.name.span, OffsetMode::Utf16);
                doc.inlay_hints.push(InlayHint {
                    position: r.end,
                    label: InlayHintLabel::String(format!(": {}", var.ty.name(scopes))),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: Default::default(),
                    tooltip: Default::default(),
                    padding_left: Default::default(),
                    padding_right: Default::default(),
                    data: Default::default(),
                });
            }
        }

        Ok((file, checked))
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

fn visualize_type_params(res: &mut String, params: &[UserTypeId], scopes: &Scopes) {
    if !params.is_empty() {
        *res += "<";
        for (i, id) in params.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }

            *res += &visualize_type(*id, scopes);
        }
        *res += ">";
    }
}

fn visualize_func(id: FunctionId, small: bool, scopes: &Scopes) -> String {
    let func = scopes.get(id);
    if let Some((union, id)) = func
        .constructor
        .and_then(|id| scopes.get(id).data.as_union().zip(Some(id)))
    {
        let mut res = visualize_location(scopes.get(id).body_scope, scopes);
        let variant = &func.name.data;
        visualize_variant_body(
            &mut res,
            union,
            variant,
            union
                .variants
                .get(variant)
                .and_then(|inner| inner.0.as_ref()),
            scopes,
        );
        return res;
    }

    let mut res = if small {
        String::new()
    } else {
        visualize_location(func.scope, scopes)
    };
    if !small && func.public {
        res += "pub ";
    }

    match func.linkage {
        Linkage::Import => res += "import ",
        Linkage::Export => res += "export ",
        Linkage::Internal => {}
    }

    match (small, func.is_unsafe) {
        (false, false) => write_de!(res, "fn {}", func.name.data),
        (false, true) => write_de!(res, "unsafe fn {}", func.name.data),
        (true, false) => write_de!(res, "fn"),
        (true, true) => write_de!(res, "unsafe fn"),
    }

    visualize_type_params(&mut res, &func.type_params, scopes);

    res += "(";
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            res += ", ";
        }

        if param.keyword {
            res += "kw ";
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
            res += &param.ty.name(scopes);
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
    if !func.ret.is_void() {
        res += ": ";
        res += &func.ret.name(scopes);
    }

    res
}

fn visualize_var(id: VariableId, scopes: &Scopes) -> String {
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
    write_de!(res, " {}: {}", var.name.data, var.ty.name(scopes));
    res
}

fn visualize_type(id: UserTypeId, scopes: &Scopes) -> String {
    let ut = scopes.get(id);
    let mut res = String::new();
    let print_body = |res: &mut String, mut wrote: bool| {
        if wrote && !ut.members.is_empty() {
            *res += "\n";
        }
        for (name, member) in ut.members.iter() {
            let header = if ut.data.is_union() {
                "shared "
            } else if member.public {
                "pub "
            } else {
                ""
            };

            write_de!(res, "\n\t{header}{name}: {},", &member.ty.name(scopes));
            wrote = true;
        }

        if wrote {
            *res += "\n}";
        } else {
            *res += "}";
        }
    };

    if !ut.data.is_template() {
        res += &visualize_location(ut.scope, scopes);
    }

    if ut.type_params.is_empty() && !ut.data.is_template() {
        let (sz, align) = GenericUserType::new(id, Default::default()).size_and_align(scopes);
        writeln_de!(res, "// size = {sz}, align = {align}");
    }

    if ut.public {
        res += "pub ";
    }
    match &ut.data {
        UserTypeData::Struct => {
            write_de!(res, "struct {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes);
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeData::UnsafeUnion => {
            write_de!(res, "unsafe union {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes);
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeData::Union(union) => {
            write_de!(res, "union {}", &ut.item.name.data);
            visualize_type_params(&mut res, &ut.type_params, scopes);
            write_de!(res, ": {} {{", union.tag.name(scopes));
            for (name, (ty, _)) in union.variants.iter() {
                res += "\n\t";
                visualize_variant_body(&mut res, union, name, ty.as_ref(), scopes);
                res += ",";
            }
            print_body(&mut res, !union.variants.is_empty());
        }
        UserTypeData::Template => {
            res += &ut.name.data;
            for (i, (tr, _)) in ut.impls.iter().flat_map(|imp| imp.as_checked()).enumerate() {
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

                        res += &id.1.name(scopes);
                    }
                    res += ">";
                }
            }
        }
        UserTypeData::AnonStruct => todo!(),
        UserTypeData::Tuple => todo!(),
    }

    res
}

fn visualize_variant_body(
    res: &mut String,
    union: &Union,
    name: &str,
    ty: Option<&Type>,
    scopes: &Scopes,
) {
    *res += name;
    match ty {
        Some(Type::User(ut)) => {
            let inner = scopes.get(ut.id);
            if inner.data.is_anon_struct() {
                *res += " {";
                for (i, (name, _)) in inner.members.iter().enumerate() {
                    write_de!(
                        res,
                        "{}{name}: {}",
                        if i > 0 { ", " } else { " " },
                        ut.ty_args
                            .get_index(i)
                            .map(|v| v.1)
                            .unwrap_or(&Type::Unknown)
                            .name(scopes)
                    )
                }
                *res += " }";
            } else if inner.data.is_tuple() {
                *res += "(";
                for i in 0..inner.members.len() {
                    if i > 0 {
                        *res += ", ";
                    }
                    *res += &ut
                        .ty_args
                        .get_index(i)
                        .map(|v| v.1)
                        .unwrap_or(&Type::Unknown)
                        .name(scopes)
                }
                *res += ")";
            }
        }
        Some(ty) => write_de!(res, "({})", ty.name(scopes)),
        None => {}
    }
    write_de!(res, " = {}", union.variant_tag(name).unwrap());
}
