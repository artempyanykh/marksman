use std::{path::PathBuf, sync::Arc};

use serde::{Deserialize, Serialize};
use std::default::Default;
use tracing::{debug, info, trace};

use crate::{
    diag::DiagCollection,
    store::{self, NoteFolder},
};

use anyhow::{anyhow, Result};
use lsp_server::{Connection, IoThreads, Message};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidChangeWorkspaceFolders, DidCloseTextDocument,
        DidOpenTextDocument, Notification, PublishDiagnostics,
    },
    request::{
        CodeLensRequest, CodeLensResolve, Completion, DocumentLinkRequest, DocumentSymbolRequest,
        GotoDefinition, HoverRequest, ResolveCompletionItem, SemanticTokensFullRequest,
        SemanticTokensRangeRequest, WorkspaceSymbol,
    },
    ClientCapabilities, CodeLensOptions, CompletionOptions, DocumentLinkOptions,
    HoverProviderCapability, InitializeParams, InitializeResult, OneOf, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensOptions, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
    WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};

use super::handlers;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ClientName {
    VSCode,
    Neovim,
    Other,
}

impl ClientName {
    pub fn parse(name: &str) -> ClientName {
        if name.contains("Visual Studio Code") {
            ClientName::VSCode
        } else if name == "Neovim" {
            ClientName::Neovim
        } else {
            ClientName::Other
        }
    }
}

pub struct Ctx {
    pub root: PathBuf,
    pub client_name: ClientName,
    pub experimental: ExperimentalCapabilities,
    pub folders: Vec<NoteFolder>,
}

#[derive(Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExperimentalCapabilities {
    #[serde(default)]
    code_lens_show_references: bool,
    #[serde(default)]
    follow_links: bool,
}

pub fn init_connection() -> Result<(Connection, IoThreads, Ctx)> {
    trace!("init_connection: start");

    let (connection, io_threads) = Connection::stdio();

    let (id, params) = connection.initialize_start()?;
    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    trace!("init_connection: received init params");

    let root = init_params
        .root_uri
        .clone()
        .ok_or_else(|| anyhow!("Expected a `root_uri` parameter"))?
        .to_file_path()
        .map_err(|_| anyhow!("`root_uri` couldn't be converted to path"))?;

    let folders = extract_workspace_folders(&init_params);

    let client_name = init_params
        .client_info
        .as_ref()
        .map(|info| ClientName::parse(info.name.as_str()))
        .unwrap_or(ClientName::Other);

    let experimental = extract_experimental(&init_params.capabilities);

    let ctx = Ctx {
        root,
        client_name,
        experimental,
        folders,
    };

    let capabilities = mk_server_caps(&ctx);
    let server_info = ServerInfo {
        name: "zeta-note".to_string(),
        ..ServerInfo::default()
    };

    let init_result = InitializeResult {
        capabilities,
        server_info: Some(server_info),
    };

    let init_result = serde_json::to_value(init_result).unwrap();

    trace!("init_connection: finalizing init");
    connection.initialize_finish(id, init_result)?;
    trace!("init_connection: finish");

    Ok((connection, io_threads, ctx))
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
    if ctx.client_name != ClientName::VSCode {
        server_capabilities.document_symbol_provider = Some(OneOf::Left(true));
        server_capabilities.workspace_symbol_provider = Some(OneOf::Left(true));
    }

    server_capabilities.workspace = Some(WorkspaceServerCapabilities {
        workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            supported: Some(true),
            change_notifications: Some(OneOf::Left(true)),
        }),
        file_operations: None,
    });

    server_capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(
        TextDocumentSyncKind::INCREMENTAL,
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

    if ctx.experimental.follow_links {
        server_capabilities.document_link_provider = Some(DocumentLinkOptions {
            resolve_provider: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        });
    }

    server_capabilities
}

pub fn extract_workspace_folders(init_params: &InitializeParams) -> Vec<NoteFolder> {
    if let Some(folders) = &init_params.workspace_folders {
        folders
            .iter()
            .map(NoteFolder::from_workspace_folder)
            .collect()
    } else {
        let root = init_params
            .root_uri
            .clone()
            .expect("Expected a `root_uri` parameter")
            .to_file_path()
            .expect("`root_uri` couldn't be converted to path");

        vec![NoteFolder::from_root_path(&root)]
    }
}

pub async fn main_loop(connection: Connection, ctx: Ctx) -> Result<()> {
    let connection = Arc::new(connection);

    info!("Starting zeta-note main loop at {}", ctx.root.display());
    debug!("Folders in the workspace: {:?}", ctx.folders);

    let mut workspace = store::Workspace::new(&ctx.folders).await?;

    let mut diag_col = DiagCollection::default();
    let mut last_note_count = workspace.note_count();

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
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                handle_request!(
                    connection,
                    req,
                    DocumentSymbolRequest => params -> {
                        let file = params.text_document.uri.to_file_path().unwrap();
                        let symbols = handlers::document_symbols(&workspace, &file, "");
                        Ok(Some(symbols.into()))
                    },
                    WorkspaceSymbol => params -> {
                        Ok(Some(handlers::workspace_symbols(&workspace, &params.query)))
                    },
                    Completion => params -> {
                        let candidates = handlers::completion_candidates(&workspace, params)
                            .unwrap_or_default();
                        Ok(Some(candidates.into()))
                    },
                    ResolveCompletionItem => params -> {
                        Ok(handlers::completion_resolve(&workspace, &params).unwrap_or(params))
                    },
                    HoverRequest => params -> {
                        Ok(handlers::hover(&workspace, params))
                    },
                    GotoDefinition => params -> {
                        Ok(handlers::goto_definition(&workspace, params).map(|loc| loc.into()))
                    },
                    SemanticTokensFullRequest => params -> {
                        let tokens = handlers::semantic_tokens_full(&workspace, params).map(|tv| {
                            SemanticTokens {
                                data: tv,
                                ..SemanticTokens::default()
                            }.into()
                        });
                        Ok(tokens)
                    },
                    SemanticTokensRangeRequest => params -> {
                        let tokens = handlers::semantic_tokens_range(&workspace, params).map(|tv| {
                            SemanticTokens {
                                data: tv,
                                ..SemanticTokens::default()
                            }.into()
                        });
                        Ok(tokens)
                    },
                    CodeLensRequest => params -> {
                        Ok(handlers::code_lenses(&workspace, params))
                    },
                    CodeLensResolve => params -> {
                        Ok(handlers::code_lens_resolve(&workspace, &params).unwrap_or(params))
                    },
                    DocumentLinkRequest => params -> {
                        Ok(handlers::document_links(&workspace, params))
                    }
                )
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                handle_notification!(
                    not,
                    DidOpenTextDocument => params -> {
                        let path = params
                            .text_document
                            .uri
                            .to_file_path()
                            .expect("Failed to turn uri into path");
                        handlers::note_open(&mut workspace, &path, &params.text_document);
                    },
                    DidCloseTextDocument => params -> {
                        handlers::note_close(&mut workspace, &params.text_document)
                            .await.unwrap();
                    },
                    DidChangeTextDocument => params -> {
                        let path = params
                            .text_document
                            .uri
                            .to_file_path()
                            .expect("Failed to turn uri into path");
                        handlers::note_apply_changes(&mut workspace, ctx.client_name, &path, &params);
                    },
                    DidChangeWorkspaceFolders => params -> {
                        handlers::note_change_workspace_folders(&mut workspace, &params.event).await.unwrap();
                    }
                )
            }
        }

        // Diagnostics and change of status should be updated after we process the message.
        // Doing otherwise may lead to stale diagnostics shown on the file:
        // 1. We receive a didChange notification.
        // 2. We produce diagnostics based on the previous state of the document (stale).
        // 3. We process the notification (diagnostics are still stale).
        // 4. We need one more message to update diagnostics based on the notification received on step 1.

        let current_notes_count = workspace.note_count();
        if current_notes_count != last_note_count {
            pending_not_tx
                .send(handlers::status_notification(current_notes_count))
                .await?;
            last_note_count = current_notes_count;
        }

        if let Some((publish_params, new_col)) = handlers::diag(&workspace, &diag_col) {
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
    }

    not_handle.abort();
    not_handle.await?;

    Ok(())
}
