use std::collections::HashMap;
use std::path::Path;

use dashmap::DashMap;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::ast::parsed::Linkage;
use crate::error::{FileId, OffsetMode};
use crate::lexer::Span;
use crate::sym::{FunctionId, Scopes, UserTypeData, UserTypeId, VariableId};
use crate::typecheck::HoverItem;
use crate::typeid::Type;
use crate::{get_default_libs, Compiler};

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

#[derive(Debug, Default)]
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
            if let Err(err) = self.on_document_change(entry.key(), None).await {
                self.client
                    .log_message(MessageType::ERROR, format!("{err}"))
                    .await;
            }
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
        if let Err(err) = self
            .on_document_change(&params.text_document.uri, None)
            .await
        {
            self.client
                .log_message(MessageType::ERROR, format!("{err}"))
                .await;
        }
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.documents
            .entry(params.text_document.uri.clone())
            .and_modify(|doc| doc.text = std::mem::take(&mut params.content_changes[0].text))
            .or_insert(Document {
                text: std::mem::take(&mut params.content_changes[0].text),
                inlay_hints: vec![],
            });
        if let Err(err) = self
            .on_document_change(&params.text_document.uri, None)
            .await
        {
            self.client
                .log_message(MessageType::ERROR, format!("{err}"))
                .await;
        }
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        match self
            .on_document_change(
                &params.text_document_position_params.text_document.uri,
                Some(params.text_document_position_params.position),
            )
            .await
        {
            Ok(hover) => Ok(hover),
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("{err}"))
                    .await;
                Err(Error::internal_error())
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
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

    async fn on_document_change(
        &self,
        uri: &Url,
        hover: Option<Position>,
    ) -> anyhow::Result<Option<Hover>> {
        let path = if !uri.scheme().is_empty() {
            Path::new(&uri.as_str()[uri.scheme().len() + 3..])
        } else {
            Path::new(uri.as_str())
        };
        let (project, libs) = get_default_libs(get_file_project(path), vec![], false, false);
        let (compiler, file) =
            Compiler::new_lsp(path.into(), self.documents.get(uri).unwrap().text.clone());
        let checked = compiler.parse(project, libs)?.typecheck(hover.map(|p| {
            position_to_span(
                &self.documents.get(uri).unwrap().text,
                file,
                p.line,
                p.character,
            )
        }))?;

        let diag = checked.diagnostics();
        let mut all = HashMap::<Url, Vec<Diagnostic>>::new();
        for (id, path) in diag.paths() {
            let l = checked.lsp_file();
            let entry = all
                .entry(if id == file {
                    uri.clone()
                } else {
                    Url::from_file_path(path).unwrap()
                })
                .or_default();
            let errors = diag.errors();
            diag.format_diagnostics(id, errors, l, OffsetMode::Utf16, |msg, start, end| {
                entry.push(Diagnostic {
                    range: Range::new(
                        Position::new(start.0 as u32 - 1, start.1 as u32 - 1),
                        Position::new(end.0 as u32 - 1, end.1 as u32 - 1),
                    ),
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("ctllsp".into()),
                    message: msg.into(),
                    ..Diagnostic::default()
                });
            });
            let warnings = diag.warnings();
            diag.format_diagnostics(id, warnings, l, OffsetMode::Utf16, |msg, start, end| {
                entry.push(Diagnostic {
                    range: Range::new(
                        Position::new(start.0 as u32 - 1, start.1 as u32 - 1),
                        Position::new(end.0 as u32 - 1, end.1 as u32 - 1),
                    ),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("ctllsp".into()),
                    message: msg.into(),
                    ..Diagnostic::default()
                });
            });
        }

        for (uri, diags) in all.into_iter() {
            self.client.publish_diagnostics(uri, diags, None).await;
        }

        let scopes = checked.scopes();
        let mut doc = self.documents.entry(uri.clone()).or_default();
        doc.inlay_hints.clear();
        for (_, var) in scopes.vars() {
            if var.name.span.file != file || var.has_hint || var.name.data.starts_with('$') {
                continue;
            }

            let position = span_to_position(&doc.text, var.name.span.pos + var.name.span.len);
            doc.inlay_hints.push(InlayHint {
                position,
                label: InlayHintLabel::String(format!(": {}", var.ty.name(scopes))),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Default::default(),
                tooltip: Default::default(),
                padding_left: Default::default(),
                padding_right: Default::default(),
                data: Default::default(),
            });
        }

        Ok(checked
            .module()
            .hover_item
            .as_ref()
            .and_then(|item| match item {
                &HoverItem::Var(id) => Some(visualize_var(id, scopes)),
                &HoverItem::Fn(id) => Some(visualize_func(id, scopes)),
                &HoverItem::Type(id) => Some(visualize_type(id, scopes)),
                HoverItem::Member(id, name) => {
                    let ut = scopes.get(*id);
                    let unk = Type::Unknown;
                    let ty = ut.members.get(name).map(|m| &m.ty).unwrap_or(&unk);
                    Some(format!("{name}: {}", ty.name(scopes)))
                }
                HoverItem::Module(id) => {
                    Some(format!("mod {}", scopes[*id].kind.name(scopes).unwrap()))
                }
                HoverItem::Trait(id) => {
                    let tr = scopes.get(*id);
                    let mut res = format!("trait {}", tr.name.data);
                    visualize_type_params(&mut res, &tr.type_params, scopes);
                    Some(res)
                }
                _ => None,
            })
            .map(|value| Hover {
                range: None,
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "ctl".into(),
                    value,
                })),
            }))
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

fn span_to_position(text: &str, offset: usize) -> Position {
    let (mut row, mut col) = (1, 1);
    for ch in text.chars().take(offset) {
        if ch == '\n' {
            row += 1;
            col = 1;
        } else {
            col += ch.len_utf16() as u32;
        }
    }
    Position::new(row - 1, col - 1)
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
        pos += ch.len_utf8();
    }
    Span::default()
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

fn visualize_func(id: FunctionId, scopes: &Scopes) -> String {
    let func = scopes.get(id);

    let mut res = String::new();
    match func.linkage {
        Linkage::Import => res += "import ",
        Linkage::Export => res += "export ",
        Linkage::Internal => {}
    }

    if func.is_unsafe {
        res += "unsafe fn ";
    } else {
        res += "fn ";
    }

    res += &func.name.data;
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

        if param.default.is_some() {
            res += "?";
        }
        res += ": ";
        res += &param.ty.name(scopes);
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
    format!(
        "{} {}: {}",
        match (var.is_static, var.mutable) {
            (true, true) => "static mut",
            (true, false) => "static",
            (false, true) => "mut",
            (false, false) => "let",
        },
        var.name.data,
        var.ty.name(scopes)
    )
}

fn visualize_type(id: UserTypeId, scopes: &Scopes) -> String {
    let mut res = String::new();
    let ut = scopes.get(id);
    let print_body = |res: &mut String, mut wrote: bool| {
        if wrote && !ut.members.is_empty() {
            *res += "\n";
        }
        for (name, member) in ut.members.iter() {
            *res += "\n\t";
            if ut.data.is_union() {
                *res += "shared ";
            } else if member.public {
                *res += "pub ";
            }
            *res += &name;
            *res += ": ";
            *res += &member.ty.name(scopes);
            *res += ",";
            wrote = true;
        }

        if wrote {
            *res += "\n}";
        } else {
            *res += "}";
        }
    };

    match &ut.data {
        UserTypeData::Struct => {
            res += "struct ";
            res += &ut.item.name.data;
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeData::UnsafeUnion => {
            res += "unsafe union ";
            res += &ut.item.name.data;
            res += " {";
            print_body(&mut res, false);
        }
        UserTypeData::Union(union) => {
            res += "union ";
            res += &ut.item.name.data;
            res += " {";
            for (name, ty) in union.variants.iter() {
                res += "\n\t";
                res += &name;
                match ty {
                    Some(Type::User(ut)) => {
                        let inner = scopes.get(ut.id);
                        if inner.data.is_anon_struct() {
                            res += " {";
                            for (i, (name, _)) in inner.members.iter().enumerate() {
                                if i > 0 {
                                    res += ", ";
                                } else {
                                    res += " ";
                                }
                                res += name;
                                res += ": ";
                                res += &ut.ty_args.get_index(i).unwrap().1.name(scopes);
                            }
                            res += " }";
                        } else if inner.data.is_tuple() {
                            res += "(";
                            for i in 0..inner.members.len() {
                                if i > 0 {
                                    res += ", ";
                                }
                                res += &ut.ty_args.get_index(i).unwrap().1.name(scopes);
                            }
                            res += ")";
                        }
                    }
                    Some(ty) => {
                        res += "(";
                        res += &ty.name(scopes);
                        res += ")";
                    }
                    None => {}
                }
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
