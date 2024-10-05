use solidity_analyzer::backend::Backend;
use tower_lsp::{LspService, Server};
use tracing::level_filters::LevelFilter;
use tree_sitter::Parser;

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
        .with_max_level(LevelFilter::DEBUG)
        .finish();

    // Set the global subscriber
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let (service, socket) = LspService::new(|client| {
        let mut parser = Parser::new();
        parser
            .set_language(tree_sitter_solidity::language())
            .expect("Error loading solidity grammar");
        Backend::new(client)
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
