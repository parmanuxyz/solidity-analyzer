#[allow(unused_imports)]
use crate::append_to_file;
use crate::backend::Backend;

use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::LanguageServer;
use tracing::{debug, instrument};

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client_capabilities
            .set(params.capabilities)
            .map_err(|_| Error::new(ErrorCode::InternalError))?;

        let state_clone = self.state.clone();
        tokio::spawn(async move { state_clone.process_jobs().await });

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                document_formatting_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        ..Default::default()
                    },
                )),
                document_symbol_provider: Some(OneOf::Left(true)),
                position_encoding: Some(PositionEncodingKind::UTF8),
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

    #[instrument(skip_all)]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_path: Url = params.text_document.uri;
        debug!(
            "did_open request for {file_path}, lang: {lang_id}",
            file_path = file_path.to_string(),
            lang_id = params.text_document.language_id
        );

        let text = params.text_document.text;
        self.client
            .log_message(
                MessageType::INFO,
                format!("did_open request for {file_path}"),
            )
            .await;
        self.add_file(&file_path, text).await;
    }

    #[instrument(skip_all)]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let file_path: Url = params.text_document.uri;
        debug!(
            "did_change request for {file_path}",
            file_path = file_path.to_string()
        );

        for content_change in params.content_changes {
            match content_change.range {
                Some(range) => {
                    self.update_file_range(&file_path, range, content_change.text)
                        .await;
                }
                None => {
                    self.update_file(&file_path, content_change.text).await;
                }
            }
        }

        self.client
            .log_message(
                MessageType::INFO,
                format!("did_change request for {file_path}"),
            )
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let file_path: Url = params.text_document.uri;
        self.client
            .log_message(
                MessageType::INFO,
                format!("did_close request for {file_path}"),
            )
            .await;
        self.remove_file(&file_path).await;
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
            Ok(edits) => {
                self.client
                    .log_message(MessageType::LOG, format!("edits: {:?}", edits))
                    .await;
                Ok(edits)
            }
            Err(err) => {
                self.client
                    .log_message(MessageType::ERROR, format!("error on fmt: {:?}", err))
                    .await;
                Err(Error::new(ErrorCode::InternalError))
            }
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let file_path: Url = params.text_document.uri;
        self.update_document_symbols(&file_path).await;
        self.state.document_symbols.get(file_path.as_str()).map_or(
            Err(Error::new(ErrorCode::InternalError)),
            |symbols| {
                // symbols
                //     .iter()
                //     .map(|sym| {
                //         debug!(
                //             "document_symbol request for {file_path}: {sym}",
                //             file_path = file_path.to_string(),
                //             sym = format!("{:?}", sym),
                //         );
                //     })
                //     .collect::<Vec<()>>();

                Ok(Some(DocumentSymbolResponse::Nested(symbols.clone())))
            },
        )
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
