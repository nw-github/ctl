use std::collections::HashMap;
use std::path::Path;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::error::OffsetMode;
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
            if let Err(err) = self.on_document_change(entry.key()).await {
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
        if let Err(err) = self.on_document_change(&params.text_document.uri).await {
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
        if let Err(err) = self.on_document_change(&params.text_document.uri).await {
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

    async fn on_document_change(&self, uri: &Url) -> anyhow::Result<()> {
        let path = if !uri.scheme().is_empty() {
            Path::new(&uri.as_str()[uri.scheme().len() + 3..])
        } else {
            Path::new(uri.as_str())
        };
        let (project, libs) = get_default_libs(get_file_project(path), vec![], false, false);
        let (compiler, file) =
            Compiler::new_lsp(path.into(), self.documents.get(uri).unwrap().text.clone());
        let checked = compiler.parse(project, libs)?.typecheck()?;

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

        Ok(())
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
