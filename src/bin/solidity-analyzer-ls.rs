use std::sync::Arc;

use solidity_analyzer::backend::{Backend, BackendState};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

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
