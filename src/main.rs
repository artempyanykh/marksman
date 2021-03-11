use std::sync::Arc;

use anyhow::{anyhow, Result};
use tracing::{info, Level};

use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{
        Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, ResolveCompletionItem,
        SemanticTokensFullRequest, SemanticTokensRangeRequest, WorkspaceSymbol,
    },
    CompletionOptions, CompletionResponse, GotoDefinitionResponse, HoverProviderCapability,
    InitializeParams, OneOf, SemanticTokens, SemanticTokensFullOptions, SemanticTokensOptions,
    SemanticTokensRangeResult, SemanticTokensResult, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};

use lsp_server::{Connection, Message, Notification, RequestId, Response};
use tracing_subscriber::EnvFilter;

use zeta_note::{facts, ls, store};

#[tokio::main]
async fn main() -> Result<()> {
    let filter = EnvFilter::default().add_directive(Level::DEBUG.into());
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .init();

    info!("Starting zeta-note LSP server");

    let (connection, io_threads) = Connection::stdio();

    let mut server_capabilities = ServerCapabilities::default();
    server_capabilities.document_symbol_provider = Some(OneOf::Left(true));
    server_capabilities.workspace_symbol_provider = Some(OneOf::Left(true));
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
            legend: ls::semantic_tokens_legend().clone(),
            range: Some(true),
            full: Some(SemanticTokensFullOptions::Bool(true)),
            ..SemanticTokensOptions::default()
        }
        .into(),
    );

    let server_capabilities = serde_json::to_value(&server_capabilities).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;

    main_loop(connection, initialization_params).await?;
    io_threads.join()?;

    info!("Shutting down zeta-note LSP server");
    Ok(())
}

async fn main_loop(connection: Connection, params: serde_json::Value) -> Result<()> {
    let connection = Arc::new(connection);

    let params: InitializeParams = serde_json::from_value(params).unwrap();
    let root_uri = params.root_uri.ok_or(anyhow!("Expected a `rootUri`"))?;
    let root_path = root_uri
        .to_file_path()
        .map_err(|_| anyhow!("`rootUri` should be a file path"))?;

    info!("Starting zeta-note main loop at {}", root_path.display());

    let ignores = store::find_ignores(&root_path).await?;
    let note_files = store::find_notes(&root_path, &ignores).await?;
    info!("Found {} note files", note_files.len());

    let mut index = facts::FactsDB::default();
    for f in note_files {
        index.with_file(&root_path, &f, &ignores).await?;
    }

    let (pending_not_tx, mut pending_not_rx) = tokio::sync::mpsc::channel(10);
    let mut last_note_count = index.note_index().ids().count();
    pending_not_tx
        .send(ls::status_notification(last_note_count))
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
        let current_notes_count = index.note_index().ids().count();
        if current_notes_count != last_note_count {
            pending_not_tx
                .send(ls::status_notification(current_notes_count))
                .await?;
            last_note_count = current_notes_count;
        }

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                if let Ok((id, params)) = cast_r::<DocumentSymbolRequest>(req.clone()) {
                    let file = params.text_document.uri.to_file_path().unwrap();
                    let result = ls::document_symbols(&index, &file, "");
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
                    let result = ls::workspace_symbols(&index, &params.query);
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
                    let candidates = ls::completion_candidates(&root_path, &index, &path, &pos)
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
                    let resolved =
                        ls::completion_resolve(&index, &params).unwrap_or_else(|| params.clone());
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
                    let hover = ls::hover(&root_path, &index, &path, &position);
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
                    let result = ls::goto_definition(&root_path, &index, &path, &position)
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
                    let tokens = ls::semantic_tokens_full(&index, &path);
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
                    let tokens = ls::semantic_tokens_range(&index, &path, &range);
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
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                if let Ok(params) = cast_n::<DidOpenTextDocument>(not.clone()) {
                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    ls::note_open(&mut index, &root_path, &path, &params.text_document);
                }

                if let Ok(params) = cast_n::<DidCloseTextDocument>(not.clone()) {
                    ls::note_close(&mut index, &root_path, &params.text_document, &ignores).await?;
                }

                if let Ok(params) = cast_n::<DidChangeTextDocument>(not.clone()) {
                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    ls::note_apply_changes(&mut index, &path, &params);
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

fn cast_n<N>(notif: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}
