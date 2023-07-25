use std::{
    fmt::Debug,
    fs::{metadata, File},
    io::Read,
    os::unix::prelude::MetadataExt,
    sync::OnceLock,
};

use dashmap::DashMap;
use similar::{DiffOp, TextDiff};
use solang_parser::pt::SourceUnitPart;
use tower_lsp::{
    lsp_types::{ClientCapabilities, DocumentSymbol, Position, Range, TextEdit, Url},
    Client,
};

use crate::solang::document_symbol::ToDocumentSymbol;
use crate::utils;

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub documents: DashMap<String, Source>,
    pub client_capabilities: OnceLock<ClientCapabilities>,
    pub document_symbols: DashMap<String, Vec<DocumentSymbol>>,
    pub document_diagnostics: DashMap<String, Vec<solang_parser::diagnostics::Diagnostic>>,
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
    #[error("Solidity parse error")]
    SolidityParseError(Vec<solang_parser::diagnostics::Diagnostic>),
    #[error("Position not found error")]
    PositionNotFoundError,
    #[error("Invalid location error")]
    InvalidLocationError,
}

enum FileAction {
    Open,
    Update,
    Close,
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

        // crate::append_to_file!(
        //     "/Users/meet/solidity-analyzer.log",
        //     "formatted_txt: {:?}",
        //     formatted_txt
        // );

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
            return Ok(self
                .documents
                .get(&file_path.to_string())
                .unwrap()
                .text
                .clone());
        }

        // read from disk if the document is not owned by client
        let file_path = utils::url_to_path(&file_path)?;
        let fsize = metadata(&file_path)?.size();
        let mut buf = Vec::<u8>::with_capacity(fsize as usize);
        let mut file = File::open(&file_path)?;
        file.read_to_end(&mut buf)?;
        Ok(String::from_utf8_lossy(&buf).into_owned())
    }

    pub async fn add_file(&self, file_path: &Url, file_contents: String) {
        self.documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.on_file_change(FileAction::Open, file_path).await;
    }

    pub async fn remove_file(&self, file_path: &Url) {
        self.documents.remove(&file_path.to_string());
        self.on_file_change(FileAction::Close, file_path).await;
    }

    pub async fn update_file(&self, file_path: &Url, file_contents: String) {
        self.documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.on_file_change(FileAction::Update, file_path).await;
    }

    pub fn update_file_range(&self, _file_path: &Url, _range: Range, _text: String) {
        unimplemented!("update_file_range")
    }

    async fn on_file_change(&self, action: FileAction, path: &Url) {
        match action {
            FileAction::Close => {
                self.document_symbols.remove(&path.to_string());
                self.document_diagnostics.remove(&path.to_string());
            }
            _ => {
                self.update_document_symbols(path).await;
            }
        }
    }

    pub async fn update_document_symbols(&self, path: &Url) -> bool {
        if let Ok(symbols) = self.get_document_symbols(path) {
            self.document_symbols.insert(path.to_string(), symbols);
        }

        match self.get_document_symbols(path) {
            Ok(symbols) => {
                self.document_symbols.insert(path.to_string(), symbols);
                self.document_diagnostics.remove(&path.to_string());
                self.on_document_diagnostic_update(path).await;
                true
            }
            Err(error) => {
                if let BackendError::SolidityParseError(diagnostics) = error {
                    self.document_diagnostics
                        .insert(path.to_string(), diagnostics);
                    self.on_document_diagnostic_update(path).await
                }
                false
            }
        }
    }

    async fn on_document_diagnostic_update(&self, _path: &Url) {
        // if let Some(diagnostics) = self.document_diagnostics.get(&path.to_string()) {
        // let diags = diagnostics
        //     .iter()
        //     .map(|diagnostic| {
        //         // let range = diagnostic.range.clone();
        //         tower_lsp::lsp_types::Diagnostic {
        //             range: Default::default(),
        //             severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
        //             code: None,
        //             code_description: None,
        //             source: Some("solang".to_string()),
        //             message: diagnostic.message.clone(),
        //             related_information: None,
        //             tags: None,
        //             data: None,
        //         }
        //     })
        //     .collect();

        // self.client
        //     .publish_diagnostics(path.clone(), diags, None)
        //     .await;
        // }
    }

    pub fn get_document_symbols(
        &self,
        file_path: &Url,
    ) -> Result<Vec<DocumentSymbol>, BackendError> {
        let file_contents = self
            .read_file(file_path.clone())
            .map_err(|_| BackendError::ReadError)?;
        let (source_unit, _comments) =
            solang_parser::parse(&file_contents, 0).map_err(BackendError::SolidityParseError)?;
        let source = Source::new(file_contents);

        Ok(source_unit
            .0
            .iter()
            .filter_map(|part| match part {
                SourceUnitPart::ContractDefinition(contract) => {
                    Some(contract.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::EnumDefinition(enum_definition) => {
                    Some(enum_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::StructDefinition(struct_definition) => {
                    Some(struct_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::EventDefinition(event_definition) => {
                    Some(event_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::ErrorDefinition(error_definition) => {
                    Some(error_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::FunctionDefinition(func_definition) => {
                    Some(func_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::VariableDefinition(variable_definition) => {
                    Some(variable_definition.to_document_symbol_with_loc(&source))
                }
                SourceUnitPart::TypeDefinition(type_definition) => {
                    Some(type_definition.to_document_symbol_with_loc(&source))
                }
                _ => None,
            })
            .collect())
    }
}

#[derive(Debug)]
pub struct Source {
    pub text: String,
    pub line_lengths: Vec<usize>,
}

impl Source {
    fn new(source: String) -> Self {
        let line_lengths = source.as_str().lines().map(|x| x.len()).collect();

        Source {
            text: source,
            line_lengths,
        }
    }

    pub fn loc_to_range(
        &self,
        loc: &solang_parser::pt::Loc,
    ) -> Result<tower_lsp::lsp_types::Range, BackendError> {
        if let solang_parser::pt::Loc::File(_, start, end) = loc {
            let start_pos = self.byte_index_to_position(*start)?;
            let end_pos = self.byte_index_to_position(*end)?;
            Ok(tower_lsp::lsp_types::Range {
                start: start_pos,
                end: end_pos,
            })
        } else {
            Err(BackendError::InvalidLocationError)
        }
    }

    fn byte_index_to_position(
        &self,
        index: usize,
    ) -> Result<tower_lsp::lsp_types::Position, BackendError> {
        let mut chars_read = 0;
        for (i, line_length) in self.line_lengths.iter().enumerate() {
            let line_number = i;
            let last_char_pos = chars_read + line_length;
            let first_char_pos = chars_read;
            if index >= first_char_pos && index <= last_char_pos {
                return Ok(tower_lsp::lsp_types::Position {
                    line: line_number as u32,
                    character: (index - first_char_pos) as u32,
                });
            }
            chars_read += line_length + 1; // for \n
        }
        Err(BackendError::PositionNotFoundError)
    }

    #[allow(dead_code)]
    fn position_to_byte_index(
        &self,
        position: &tower_lsp::lsp_types::Position,
    ) -> Result<usize, BackendError> {
        let mut chars_read = 0;
        for (i, line_length) in self.line_lengths.iter().enumerate() {
            let line_number = i;
            let _last_char_pos = chars_read + line_length;
            let first_char_pos = chars_read;
            if position.line as usize == line_number {
                return Ok(first_char_pos + position.character as usize);
            }
            chars_read += line_length + 1; // for \n
        }
        Err(BackendError::PositionNotFoundError)
    }

    #[allow(dead_code)]
    fn get_loc_substring(&self, loc: &solang_parser::pt::Loc) -> String {
        if let solang_parser::pt::Loc::File(_, start, end) = loc {
            self.text[*start..*end].to_string()
        } else {
            "".to_string()
        }
    }

    #[allow(dead_code)]
    fn get_range_substring(&self, range: &tower_lsp::lsp_types::Range) -> String {
        let start = self.position_to_byte_index(&range.start).unwrap();
        let end = self.position_to_byte_index(&range.end).unwrap();
        self.text[start..end].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use solang_parser::pt::*;

    mod source_tests {
        use super::*;

        #[test]
        fn test_byte_index_to_position() {
            let source: &str = r#"contract flipper {
    bool private value;

    /// Constructor that initializes the `bool` value to the given `init_value`.
    constructor(bool initvalue) {
        value = initvalue;
    }

    /// A message that can be called on instantiated contracts.
    /// This one flips the value of the stored `bool` from `true`
    /// to `false` and vice versa.
    function flip() public {
        value = !value;
    }

    /// Simply returns the current value of our `bool`.
    function get() public view returns (bool) {
        return value;
    }

    function something() public
      view
        returns (uint _a) {
            return 0;
        }
}"#;
            let source_bytes = source.as_bytes();
            let src = Source::new(source.to_string());
            let lines = source.lines().collect::<Vec<&str>>();
            let (tree, _comments) = solang_parser::parse(source, 0).unwrap();

            let verify_loc_to_range = |loc: &Loc| {
                if let Loc::File(_, loc_start, loc_end) = loc {
                    let loc_string = String::from_utf8_lossy(&source_bytes[*loc_start..*loc_end]);
                    let range = src.loc_to_range(loc).unwrap();

                    let range_string = if range.start.line != range.end.line {
                        let start_line = lines[range.start.line as usize]
                            [range.start.character as usize..]
                            .to_string();
                        let end_line = lines[range.end.line as usize]
                            [..range.end.character as usize]
                            .to_string();

                        let in_between_start = range.start.line as usize + 1;
                        let in_between_end = range.end.line as usize - 1;

                        let mut in_between_lines = None;
                        if in_between_start <= in_between_end {
                            let to_join = lines[in_between_start..=in_between_end].to_vec();
                            in_between_lines = Some(to_join.join("\n"));
                        }
                        (if let Some(in_between_lines) = in_between_lines {
                            vec![start_line, in_between_lines, end_line]
                        } else {
                            vec![start_line, end_line]
                        })
                        .join("\n")
                    } else {
                        lines[range.start.line as usize]
                            [range.start.character as usize..range.end.character as usize]
                            .to_string()
                    };

                    assert_eq!(
                        loc_string, range_string,
                        "loc: {:#?}, range: {:#?}",
                        loc, range
                    );

                    println!(
                        "loc_string: {:?}, range_string: {:?}",
                        loc_string, range_string
                    );
                }
            };

            for part in &tree.0 {
                if let SourceUnitPart::ContractDefinition(def) = part {
                    for part in &def.parts {
                        match part {
                            ContractPart::VariableDefinition(def) => {
                                verify_loc_to_range(&def.loc);
                            }
                            ContractPart::FunctionDefinition(def) => {
                                verify_loc_to_range(&def.loc);
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }
}
