use solidity_analyzer::backend::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        client_capabilities: Default::default(),
        state: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
