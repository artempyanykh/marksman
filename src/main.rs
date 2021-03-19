use anyhow::Result;

use tracing::info;

use tracing_subscriber::EnvFilter;

use zeta_note::lsp;

use clap::{crate_version, Clap};
#[derive(Clap)]
#[clap(version = crate_version!())]
/// Markdown LSP server for easy note-taking with cross-references and diagnostics
pub struct Opts {
    #[clap(subcommand)]
    pub command: Option<Command>,
}

#[derive(Clap)]
pub enum Command {
    /// Start LSP server inside the current directory
    Serve(ServeCmd),
}

#[derive(Clap, Default)]
#[clap(alias = "s")]
pub struct ServeCmd {}

#[tokio::main]
async fn main() -> Result<()> {
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::default().add_directive("zeta_note=info".parse().unwrap()));

    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .init();

    let opts = Opts::parse();

    match opts.command {
        Some(Command::Serve(ServeCmd {})) | None => {
            info!("Starting zeta-note LSP server");
            let (connection, io_threads, ctx) = lsp::server::init_connection()?;
            lsp::server::main_loop(connection, ctx).await?;
            io_threads.join()?;
            info!("Shutting down zeta-note LSP server");
        }
    }

    Ok(())
}
