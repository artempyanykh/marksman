mod note;
mod parsing;
mod store;

use std::error::Error;

use anyhow::{anyhow, Result};
use tracing::{debug, info, Level};

use lsp_types::{
    request::DocumentSymbolRequest, DocumentSymbolResponse, InitializeParams, OneOf,
    ServerCapabilities,
};

use lsp_server::{Connection, Message, Request, RequestId, Response};
use tracing_subscriber::EnvFilter;

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

    let server_capabilities = serde_json::to_value(&ServerCapabilities::default()).unwrap();
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

    let note_files = store::find_notes(&root_path).await?;
    info!("Found {} note files", note_files.len());

    for msg in &connection.receiver {
        debug!("Got msg: {:?}", msg);

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                debug!("Got request: {:?}", req);

                if let Ok((id, params)) = cast::<DocumentSymbolRequest>(req) {
                    debug!("Got documentSymbol request #{}: {:?}", id, params);
                    let result: Option<DocumentSymbolResponse> = None;
                    let result = serde_json::to_value(&result).unwrap();
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
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
