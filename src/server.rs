use crate::backend::Backend;

use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::LanguageServer;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let file_path: Url = params.text_document.uri;
        self.client
            .log_message(
                MessageType::INFO,
                format!("formatting request for {file_path}"),
            )
            .await;

        match self.get_fmt_textedits(file_path).await {
            Ok(edits) => Ok(edits),
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("error on fmt: {:?}", err))
                    .await;
                Err(Error::new(ErrorCode::InternalError))
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
