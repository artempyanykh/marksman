use std::{path::PathBuf, sync::Arc};

use serde::{Deserialize, Serialize};
use std::default::Default;
use tracing::info;

use crate::{diag::DiagCollection, facts, store};

use anyhow::{anyhow, Result};
use lsp_server::{Connection, IoThreads, Message, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification,
        PublishDiagnostics,
    },
    request::{
        CodeLensRequest, CodeLensResolve, Completion, DocumentSymbolRequest, GotoDefinition,
        HoverRequest, ResolveCompletionItem, SemanticTokensFullRequest, SemanticTokensRangeRequest,
        WorkspaceSymbol,
    },
    ClientCapabilities, ClientInfo, CodeLensOptions, CompletionOptions, CompletionResponse,
    GotoDefinitionResponse, HoverProviderCapability, InitializeParams, OneOf, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensRangeResult,
    SemanticTokensResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

use super::handlers;

pub struct Ctx {
    pub root: PathBuf,
    pub is_vscode: bool,
    pub experimental: ExperimentalCapabilities,
}

#[derive(Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExperimentalCapabilities {
    #[serde(default)]
    code_lens_show_references: bool,
}

pub fn init_connection() -> Result<(Connection, IoThreads, Ctx)> {
    let (connection, io_threads) = Connection::stdio();

    let (id, params) = connection.initialize_start()?;
    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let root = init_params
        .root_uri
        .ok_or(anyhow!("Expected a `root_uri` parameter"))?
        .to_file_path()
        .map_err(|_| anyhow!("`root_uri` couldn't be converted to path"))?;

    let is_vscode = init_params
        .client_info
        .as_ref()
        .map(is_vscode)
        .unwrap_or(false);

    let experimental = extract_experimental(&init_params.capabilities);

    let ctx = Ctx {
        root,
        is_vscode,
        experimental,
    };

    let server_caps = mk_server_caps(&ctx);

    let server_init_data = serde_json::json!({
        "capabilities": server_caps,
        "serverInfo": {
            "name": "zeta-note",
        }
    });

    connection.initialize_finish(id, server_init_data)?;

    Ok((connection, io_threads, ctx))
}

fn is_vscode(client_info: &ClientInfo) -> bool {
    client_info.name == "Visual Studio Code"
}

fn extract_experimental(cap: &ClientCapabilities) -> ExperimentalCapabilities {
    cap.experimental
        .as_ref()
        .map(|val| serde_json::from_value::<ExperimentalCapabilities>(val.clone()).unwrap())
        .unwrap_or_default()
}

fn mk_server_caps(ctx: &Ctx) -> ServerCapabilities {
    let mut server_capabilities = ServerCapabilities::default();

    // VSCode already has Markdown built-in language features, which can't be
    // turned off https://github.com/microsoft/vscode/issues/118817
    // To avoid conflicts don't enable symbol providers for VSCode
    if !ctx.is_vscode {
        server_capabilities.document_symbol_provider = Some(OneOf::Left(true));
        server_capabilities.workspace_symbol_provider = Some(OneOf::Left(true));
    }

    server_capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(
        TextDocumentSyncKind::Incremental,
    ));

    server_capabilities.completion_provider = Some(CompletionOptions {
        trigger_characters: Some(vec![":".to_string(), "@".to_string()]),
        resolve_provider: Some(true),
        ..CompletionOptions::default()
    });

    server_capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));

    server_capabilities.definition_provider = Some(OneOf::Left(true));

    server_capabilities.semantic_tokens_provider = Some(
        SemanticTokensOptions {
            legend: handlers::semantic_tokens_legend().clone(),
            range: Some(true),
            full: Some(SemanticTokensFullOptions::Bool(true)),
            ..SemanticTokensOptions::default()
        }
        .into(),
    );

    if ctx.experimental.code_lens_show_references {
        server_capabilities.code_lens_provider = Some(CodeLensOptions {
            resolve_provider: Some(true),
        });
    }

    server_capabilities
}

pub async fn main_loop(connection: Connection, ctx: Ctx) -> Result<()> {
    let connection = Arc::new(connection);

    let root = &ctx.root;
    info!("Starting zeta-note main loop at {}", root.display());

    let ignores = store::find_ignores(&root).await?;
    let note_files = store::find_notes(&root, &ignores).await?;
    info!("Found {} note files", note_files.len());

    let mut facts = facts::FactsDB::from_files(&root, &note_files, &ignores).await?;
    let mut diag_col = DiagCollection::default();
    let mut last_note_count = facts.note_index().size();

    let (pending_not_tx, mut pending_not_rx) = tokio::sync::mpsc::channel(100);
    pending_not_tx
        .send(handlers::status_notification(last_note_count))
        .await?;

    let not_connection = connection.clone();
    let not_handle = tokio::spawn(async move {
        while let Some(not) = pending_not_rx.recv().await {
            not_connection
                .sender
                .send(Message::Notification(not))
                .unwrap_or(());
        }
    });

    for msg in &connection.receiver {
        let current_notes_count = facts.note_index().size();
        if current_notes_count != last_note_count {
            pending_not_tx
                .send(handlers::status_notification(current_notes_count))
                .await?;
            last_note_count = current_notes_count;
        }

        if let Some((publish_params, new_col)) = handlers::diag(&facts, &diag_col) {
            diag_col = new_col;
            for param in publish_params {
                let param = serde_json::to_value(param).unwrap();
                let not = lsp_server::Notification {
                    method: PublishDiagnostics::METHOD.to_string(),
                    params: param,
                };
                pending_not_tx.send(not).await?;
            }
        }

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                if let Ok((id, params)) = cast_r::<DocumentSymbolRequest>(req.clone()) {
                    let file = params.text_document.uri.to_file_path().unwrap();
                    let result = handlers::document_symbols(&facts, &file, "");
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<WorkspaceSymbol>(req.clone()) {
                    let result = handlers::workspace_symbols(&facts, &params.query);
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<Completion>(req.clone()) {
                    let path = params
                        .text_document_position
                        .text_document
                        .uri
                        .to_file_path()
                        .unwrap();
                    let pos = params.text_document_position.position;
                    let candidates = handlers::completion_candidates(&root, &facts, &path, &pos)
                        .unwrap_or_default();
                    let result = Some(CompletionResponse::Array(candidates));
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<ResolveCompletionItem>(req.clone()) {
                    let resolved = handlers::completion_resolve(&facts, &params)
                        .unwrap_or_else(|| params.clone());
                    let result = serde_json::to_value(resolved).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<HoverRequest>(req.clone()) {
                    let path = params
                        .text_document_position_params
                        .text_document
                        .uri
                        .to_file_path()
                        .unwrap();
                    let position = params.text_document_position_params.position;
                    let hover = handlers::hover(&root, &facts, &path, &position);
                    let result = serde_json::to_value(hover).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<GotoDefinition>(req.clone()) {
                    let path = params
                        .text_document_position_params
                        .text_document
                        .uri
                        .to_file_path()
                        .unwrap();
                    let position = params.text_document_position_params.position;
                    let result = handlers::goto_definition(&root, &facts, &path, &position)
                        .map(|loc| GotoDefinitionResponse::Scalar(loc));
                    let result = serde_json::to_value(result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<SemanticTokensFullRequest>(req.clone()) {
                    let path = params.text_document.uri.to_file_path().unwrap();
                    let tokens = handlers::semantic_tokens_full(&facts, &path);
                    let result = tokens.map(|tv| {
                        SemanticTokensResult::Tokens(SemanticTokens {
                            data: tv,
                            ..SemanticTokens::default()
                        })
                    });
                    let result = serde_json::to_value(result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<SemanticTokensRangeRequest>(req.clone()) {
                    let path = params.text_document.uri.to_file_path().unwrap();
                    let range = params.range;
                    let tokens = handlers::semantic_tokens_range(&facts, &path, &range);
                    let result = tokens.map(|tv| {
                        SemanticTokensRangeResult::Tokens(SemanticTokens {
                            data: tv,
                            ..SemanticTokens::default()
                        })
                    });
                    let result = serde_json::to_value(result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<CodeLensRequest>(req.clone()) {
                    let path = params.text_document.uri.to_file_path().unwrap();
                    let lenses = handlers::code_lenses(&facts, &path);
                    let result = serde_json::to_value(lenses).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }

                if let Ok((id, params)) = cast_r::<CodeLensResolve>(req.clone()) {
                    let resolved_lens =
                        handlers::code_lens_resolve(&facts, &params).unwrap_or(params);
                    let result = serde_json::to_value(resolved_lens).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                if let Ok(params) = cast_n::<DidOpenTextDocument>(not.clone()) {
                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    handlers::note_open(&mut facts, &root, &path, &params.text_document);
                }

                if let Ok(params) = cast_n::<DidCloseTextDocument>(not.clone()) {
                    handlers::note_close(&mut facts, &root, &params.text_document, &ignores)
                        .await?;
                }

                if let Ok(params) = cast_n::<DidChangeTextDocument>(not.clone()) {
                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    handlers::note_apply_changes(&mut facts, &path, &params);
                }
            }
        }
    }

    not_handle.abort();
    not_handle.await?;

    Ok(())
}

fn cast_r<R>(req: lsp_server::Request) -> Result<(RequestId, R::Params), lsp_server::Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_n<N>(notif: lsp_server::Notification) -> Result<N::Params, lsp_server::Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}
