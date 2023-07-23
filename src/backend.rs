use std::{
    fs::{metadata, File},
    io::Read,
    os::unix::prelude::MetadataExt,
    sync::OnceLock,
};

use dashmap::DashMap;
use similar::{DiffOp, TextDiff};
use tower_lsp::{
    lsp_types::{ClientCapabilities, Position, Range, TextEdit, Url},
    Client,
};

use crate::{utils};

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub documents: DashMap<String, String>,
    pub client_capabilities: OnceLock<ClientCapabilities>,
}

#[derive(thiserror::Error, Debug)]
pub enum BackendError {
    #[error("Parse error")]
    ParseError,
    #[error("Format error")]
    FormatError,
    #[error("Read error")]
    ReadError,
    #[error("Unprocessable url error")]
    UnprocessableUrlError,
}

impl Backend {
    pub async fn get_fmt_textedits(&self, file_path: Url) -> anyhow::Result<Option<Vec<TextEdit>>> {
        let config = utils::get_foundry_config(&file_path)?;
        let mut err = None;
        let file_contents = self.read_file(file_path.clone()).map_err(|_err| {
            // append_to_file!(
            //     "/Users/meet/solidity-analyzer.log",
            //     "read error on {}: {:?}",
            //     file_path,
            //     err
            // );
            BackendError::ReadError
        })?;
        let parsed_src = forge_fmt::parse(&file_contents).map_err(|err_| {
            err = Some(err_);
            BackendError::ParseError
        });
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::ERROR,
                format!("error on parse: {:?}", err),
            )
            .await;
        let parsed_src = parsed_src?;
        let mut formatted_txt = String::default();
        forge_fmt::format(&mut formatted_txt, parsed_src, config.fmt)
            .map_err(|_| BackendError::FormatError)?;
        let formatted_txt_lines = formatted_txt.lines().collect::<Vec<&str>>();

        let diff: TextDiff<'_, '_, '_, str> = TextDiff::from_lines(&file_contents, &formatted_txt);
        let text_edits = diff
            .ops()
            .iter()
            .map(|diff_op| match diff_op {
                DiffOp::Insert {
                    old_index,
                    new_index,
                    new_len,
                } => {
                    let to_add = &formatted_txt_lines[*new_index..(*new_index + *new_len)];
                    Some(TextEdit {
                        range: Range {
                            start: Position {
                                line: *old_index as u32,
                                character: 0,
                            },
                            end: Position {
                                line: *old_index as u32,
                                character: 0,
                            },
                        },
                        new_text: to_add.join("\n"),
                    })
                }
                DiffOp::Delete {
                    old_index,
                    old_len,
                    new_index: _,
                } => Some(TextEdit {
                    range: Range {
                        start: Position {
                            line: *old_index as u32,
                            character: 0,
                        },
                        end: Position {
                            line: (*old_index + *old_len) as u32,
                            character: u32::MAX,
                        },
                    },
                    new_text: "".to_string(),
                }),
                DiffOp::Replace {
                    old_index,
                    old_len,
                    new_index,
                    new_len,
                } => {
                    let to_add = &formatted_txt_lines[*new_index..(*new_index + *new_len)];
                    Some(TextEdit {
                        range: Range {
                            start: Position {
                                line: *old_index as u32,
                                character: 0,
                            },
                            end: Position {
                                line: (*old_index + *old_len - 1) as u32,
                                character: u32::MAX,
                            },
                        },
                        new_text: to_add.join("\n"),
                    })
                }
                _ => None,
            })
            .filter(|x| x.is_some())
            .collect();
        Ok(text_edits)
    }

    fn read_file(&self, file_path: Url) -> anyhow::Result<String> {
        // if the document is owned by client, use the synchronized
        // copy in server's memory
        if self.documents.contains_key(&file_path.to_string()) {
            return Ok(self.documents.get(&file_path.to_string()).unwrap().clone());
        }

        // read from disk if the document is not owned by client
        let file_path = utils::url_to_path(&file_path)?;
        let fsize = metadata(&file_path)?.size();
        let mut buf = Vec::<u8>::with_capacity(fsize as usize);
        let mut file = File::open(&file_path)?;
        file.read_to_end(&mut buf)?;
        Ok(String::from_utf8_lossy(&buf).into_owned())
    }

    pub fn add_file(&self, file_path: &Url, file_contents: String) {
        self.documents.insert(file_path.to_string(), file_contents);
    }

    pub fn remove_file(&self, file_path: &Url) {
        self.documents.remove(&file_path.to_string());
    }

    pub fn update_file(&self, file_path: &Url, file_contents: String) {
        self.documents.insert(file_path.to_string(), file_contents);
    }

    pub fn update_file_range(&self, _file_path: &Url, _range: Range, _text: String) {
        unimplemented!("update_file_range")
    }
}
