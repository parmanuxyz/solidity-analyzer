use std::sync::Arc;

use solidity_analyzer::backend::{Backend, BackendState};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    // Set up a filter based on the environment variable `RUST_LOG`
    let env_filter = tracing_subscriber::EnvFilter::from_default_env();

    // Create a file-based subscriber and add it to the filter
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_writer(|| {
            #[allow(deprecated)]
            let home_dir = std::env::home_dir().expect("Failed to get home dir");
            let log_file_path = home_dir.join("solidity-analyzer.log");
            std::io::BufWriter::new(
                std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(log_file_path)
                    .expect("Failed to open solidity-analyzer.log"),
            )
        })
        .with_env_filter(env_filter)
        .finish();

    // Set the global subscriber
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let (service, socket) = LspService::new(|client| {
        let client_clone = client.clone();
        Backend {
            client,
            client_capabilities: Default::default(),
            state: Arc::new(BackendState::new(client_clone)),
        }
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
