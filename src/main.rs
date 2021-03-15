use anyhow::Result;

use tracing::info;

use tracing_subscriber::EnvFilter;

use zeta_note::lsp;

#[tokio::main]
async fn main() -> Result<()> {
    let filter = EnvFilter::try_from_default_env().unwrap_or_default();
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .init();

    info!("Starting zeta-note LSP server");
    let (connection, io_threads, ctx) = lsp::comm::init_connection()?;
    lsp::comm::main_loop(connection, ctx).await?;
    io_threads.join()?;
    info!("Shutting down zeta-note LSP server");

    Ok(())
}
