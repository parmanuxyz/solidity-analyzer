use solidity_analyzer::backend::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Default::default(),
        client_capabilities: Default::default(),
        document_symbols: Default::default(),
        document_diagnostics: Default::default(),
        project_compilation_output: Default::default(),
        solc_diagnostics: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
