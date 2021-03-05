mod note;
mod parsing;

use std::error::Error;

use tracing::{debug, info, Level};

use lsp_types::{
    request::DocumentSymbolRequest, DocumentSymbolResponse, InitializeParams, OneOf,
    ServerCapabilities,
};

use lsp_server::{Connection, Message, Request, RequestId, Response};
use tracing_subscriber::EnvFilter;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
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

    main_loop(&connection, initialization_params)?;
    io_threads.join()?;

    info!("Shutting down zeta-note LSP server");
    Ok(())
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    info!("Starting zeta-note main loop");

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
