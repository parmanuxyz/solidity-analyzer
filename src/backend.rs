use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    fs::{metadata, File},
    io::Read,
    os::unix::prelude::MetadataExt,
    path::PathBuf,
    sync::OnceLock,
};

use dashmap::DashMap;
use ethers_solc::{artifacts::Severity, Project, ProjectCompileOutput, ProjectPathsConfig};
use similar::{DiffOp, TextDiff};
use solang_parser::pt::SourceUnitPart;
use tokio::task::JoinSet;
use tower_lsp::{
    lsp_types::{
        ClientCapabilities, Diagnostic, DiagnosticSeverity, DocumentSymbol, NumberOrString,
        Position, Range, TextEdit, Url,
    },
    Client,
};

use crate::utils;
#[allow(unused_imports)]
use crate::{append_to_file, solang::document_symbol::ToDocumentSymbol};

pub enum Job {
    ComputeSolcDiagnostics(Url),
}

#[derive(Debug)]
pub struct BackendState {
    pub documents: DashMap<String, Source>,
    pub document_symbols: DashMap<String, Vec<DocumentSymbol>>,
    pub document_diagnostics: DashMap<String, Vec<solang_parser::diagnostics::Diagnostic>>,
    pub solc_diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub project_compilation_output: DashMap<PathBuf, ProjectCompileOutput>,
    pub jobs_sender: tokio::sync::mpsc::Sender<Job>,
    pub jobs_receiver: tokio::sync::RwLock<tokio::sync::mpsc::Receiver<Job>>,
    pub diagnostics_pushed_for: tokio::sync::RwLock<HashMap<Url, bool>>,
    pub client: Client,
}

impl BackendState {
    pub fn new(client: Client) -> Self {
        let (sender, receiver) = tokio::sync::mpsc::channel::<Job>(100);

        Self {
            jobs_receiver: tokio::sync::RwLock::new(receiver),
            jobs_sender: sender,
            documents: Default::default(),
            document_symbols: Default::default(),
            document_diagnostics: Default::default(),
            solc_diagnostics: Default::default(),
            project_compilation_output: Default::default(),
            client,
            diagnostics_pushed_for: Default::default(),
        }
    }

    pub async fn process_jobs(&self) {
        let mut receiver = self.jobs_receiver.write().await;
        while let Some(job) = receiver.recv().await {
            match job {
                Job::ComputeSolcDiagnostics(path) => {
                    if self.compile_project(&path).is_ok() {
                        let root = utils::get_root_path(&path).unwrap();
                        let output = self.project_compilation_output.get(&root).unwrap();
                        let diagnostics = self.compile_errors_to_diagnostic(&root, output.clone());
                        if !diagnostics.is_empty() {
                            self.solc_diagnostics.insert(root.clone(), diagnostics);
                            self.on_solc_diagnostics_update(&root).await;
                        }
                    }
                }
            }
        }
    }

    pub async fn queue_job(&self, job: Job) {
        let sender = self.jobs_sender.clone();
        sender.send(job).await.unwrap();
    }

    fn compile_project(&self, path: &Url) -> anyhow::Result<()> {
        let root_path = utils::get_root_path(path)?;
        let project = Project::builder()
            .paths(ProjectPathsConfig::dapptools(root_path.as_os_str()).unwrap())
            .build()?;
        let output = project.compile()?;
        self.project_compilation_output.insert(root_path, output);
        Ok(())
    }

    fn compile_errors_to_diagnostic(
        &self,
        root: &std::path::Path,
        output: ProjectCompileOutput,
    ) -> Vec<Diagnostic> {
        output
            .output()
            .errors
            .iter()
            .filter(|err| {
                err.source_location.is_some()
                    && (std::path::Path::new(
                        &root.join(&err.source_location.as_ref().unwrap().file),
                    )
                    .exists())
            })
            .map(|err| {
                let severity = match err.severity {
                    Severity::Error => Some(DiagnosticSeverity::ERROR),
                    Severity::Warning => Some(DiagnosticSeverity::WARNING),
                    Severity::Info => Some(DiagnosticSeverity::INFORMATION),
                };
                let source_location = err.source_location.as_ref().unwrap();
                let path = root.join(&source_location.file);
                let url_string = format!("file://{}", path.to_str().unwrap());
                let url = Url::parse(&url_string).unwrap();
                // append_to_file!("/Users/meet/solidity-analyzer.log", "url: {}", url);
                let file_contents = self.read_file(url).unwrap();
                let src = Source::new(file_contents);
                let range = Range {
                    start: src
                        .byte_index_to_position(source_location.start as usize)
                        .unwrap(),
                    end: src
                        .byte_index_to_position(source_location.end as usize)
                        .unwrap(),
                };

                Diagnostic {
                    range,
                    severity,
                    source: Some("solc".to_string()),
                    code: err.error_code.map(|x| x as i32).map(NumberOrString::Number),
                    code_description: None,
                    message: err.message.clone(),
                    related_information: None,
                    tags: None,
                    data: Some(serde_json::json!({
                        "url": url_string
                    })),
                }
            })
            .collect()
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

    fn get_grouped_diagnostics_by_file(&self, root: &PathBuf) -> HashMap<Url, Vec<Diagnostic>> {
        let mut map = HashMap::<Url, Vec<Diagnostic>>::new();
        if let Some(diagnostics) = self.solc_diagnostics.get(root) {
            // group by file
            for diagnostic in diagnostics.iter() {
                if let Some(data) = &diagnostic.data {
                    if let Some(url) = data.get("url") {
                        if let Some(url) = url.as_str() {
                            let url = Url::parse(url).unwrap();
                            if let std::collections::hash_map::Entry::Vacant(e) =
                                map.entry(url.clone())
                            {
                                e.insert(vec![diagnostic.clone()]);
                            } else {
                                map.get_mut(&url).unwrap().push(diagnostic.clone());
                            }
                        }
                    }
                }
            }
        }
        map
    }

    async fn on_solc_diagnostics_update(&self, root: &PathBuf) {
        let grouped = self.get_grouped_diagnostics_by_file(root);
        let mut join_set = JoinSet::new();
        let grouped_keys: HashSet<Url> = grouped.keys().cloned().into_iter().collect();
        let already_pushed = self
            .diagnostics_pushed_for
            .read()
            .await
            .keys()
            .cloned()
            .collect::<HashSet<Url>>();

        let mut diagnostics_pushed_for = self.diagnostics_pushed_for.write().await;
        // clear fixed/stale diagnostics
        for url in already_pushed.difference(&grouped_keys) {
            let client_clone = self.client.clone();
            let uri = url.clone();
            join_set.spawn(async move {
                client_clone.publish_diagnostics(uri, vec![], None).await;
            });
            diagnostics_pushed_for.remove(url);
        }

        for (uri, diags) in grouped {
            assert!(!diags.is_empty());
            if !diags.is_empty() {
                let client_clone = self.client.clone();
                diagnostics_pushed_for.insert(uri.clone(), true);
                // publish diagnostics of all files async
                join_set.spawn(async move {
                    client_clone
                        .publish_diagnostics(uri.clone(), diags, None)
                        .await;
                });
            }
        }

        while let Some(_) = join_set.join_next().await {}
    }
}

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub client_capabilities: OnceLock<ClientCapabilities>,
    pub state: std::sync::Arc<BackendState>,
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
        let file_contents = self.state.read_file(file_path.clone()).map_err(|_err| {
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

    pub async fn add_file(&self, file_path: &Url, file_contents: String) {
        self.state
            .documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.on_file_change(FileAction::Open, file_path).await;
    }

    pub async fn remove_file(&self, file_path: &Url) {
        self.state.documents.remove(&file_path.to_string());
        self.on_file_change(FileAction::Close, file_path).await;
    }

    pub async fn update_file(&self, file_path: &Url, file_contents: String) {
        self.state
            .documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.on_file_change(FileAction::Update, file_path).await;
    }

    pub fn update_file_range(&self, _file_path: &Url, _range: Range, _text: String) {
        unimplemented!("update_file_range")
    }

    async fn on_file_change(&self, action: FileAction, path: &Url) {
        match action {
            FileAction::Close => {
                self.state.document_symbols.remove(&path.to_string());
                self.state.document_diagnostics.remove(&path.to_string());
            }
            _ => {
                self.update_document_symbols(path).await;
                self.state
                    .queue_job(Job::ComputeSolcDiagnostics(path.clone()))
                    .await;
            }
        }
    }

    pub async fn update_document_symbols(&self, path: &Url) -> bool {
        if let Ok(symbols) = self.get_document_symbols(path) {
            self.state
                .document_symbols
                .insert(path.to_string(), symbols);
        }

        match self.get_document_symbols(path) {
            Ok(symbols) => {
                self.state
                    .document_symbols
                    .insert(path.to_string(), symbols);
                self.state.document_diagnostics.remove(&path.to_string());
                self.on_document_diagnostic_update(path).await;
                true
            }
            Err(error) => {
                if let BackendError::SolidityParseError(diagnostics) = error {
                    self.state
                        .document_diagnostics
                        .insert(path.to_string(), diagnostics);
                    self.on_document_diagnostic_update(path).await;
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
            .state
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
    pub fn new(source: String) -> Self {
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

    pub fn byte_index_to_position(
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
