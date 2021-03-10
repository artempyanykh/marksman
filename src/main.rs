use anyhow::{anyhow, Result};
use tracing::{debug, info, Level};

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

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use tracing_subscriber::EnvFilter;

use zeta_note::{db, ls, store};

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

    main_loop(&connection, initialization_params).await?;
    io_threads.join()?;

    info!("Shutting down zeta-note LSP server");
    Ok(())
}

async fn main_loop(connection: &Connection, params: serde_json::Value) -> Result<()> {
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    let root_uri = params.root_uri.ok_or(anyhow!("Expected a `rootUri`"))?;
    let root_path = root_uri
        .to_file_path()
        .map_err(|_| anyhow!("`rootUri` should be a file path"))?;

    info!("Starting zeta-note main loop at {}", root_path.display());

    let ignores = store::find_ignores(&root_path).await?;
    let note_files = store::find_notes(&root_path, &ignores).await?;
    info!("Found {} note files", note_files.len());

    let mut index = db::GlobalIndex::from_files(&note_files, &ignores).await?;

    for msg in &connection.receiver {
        debug!("Got msg: {:?}", msg);

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                debug!("Got request: {:?}", req);

                if let Ok((id, params)) = cast_r::<DocumentSymbolRequest>(req.clone()) {
                    debug!("Got documentSymbol request #{}: {:?}", id, params);
                    let file = params.text_document.uri.to_file_path().unwrap();
                    let result = Some(index.headings(file, ""));
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
                    debug!("Got workspaceSymbol request #{}: {:?}", id, params);
                    let result = Some(index.headings_all(&params.query));
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
                    let resolved = ls::completion_resolve(&root_path, &index, &params)
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
            Message::Response(resp) => {
                debug!("Got response: {:?}", resp);
            }
            Message::Notification(not) => {
                debug!("Got notification: {:?}", not);

                if let Ok(params) = cast_n::<DidOpenTextDocument>(not.clone()) {
                    debug!("Got didOpen notification: {:?}", params);

                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    let note = ls::note_open(&params.text_document);
                    index.insert(path, note);
                }

                if let Ok(params) = cast_n::<DidCloseTextDocument>(not.clone()) {
                    debug!("Got didClose notification: {:?}", params);

                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    let note = ls::note_close(&params.text_document, &ignores).await?;
                    if let Some(note) = note {
                        index.insert(path, note);
                    }
                }

                if let Ok(params) = cast_n::<DidChangeTextDocument>(not.clone()) {
                    debug!("Got didChange notification: {:?}", params);

                    let path = params
                        .text_document
                        .uri
                        .to_file_path()
                        .expect("Failed to turn uri into path");
                    let existing_note = index.require(&path);
                    let updated_note = ls::note_apply_changes(existing_note, &params);
                    index.insert(path, updated_note);
                }
            }
        }
    }
    Ok(())
}

fn cast_r<R>(req: Request) -> Result<(RequestId, R::Params), Request>
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
