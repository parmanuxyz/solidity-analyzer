use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    fs::{metadata, File},
    io::Read,
    os::unix::prelude::MetadataExt,
    path::PathBuf,
    sync::{Arc, OnceLock},
};

use dashmap::DashMap;
use foundry_compilers::{
    artifacts::{
        error::SourceLocation, SecondarySourceLocation, Severity, Source as FoundrySource,
    },
    compilers::{multi::MultiCompilerError, CompilationError},
    project::ProjectCompiler,
    ProjectCompileOutput,
};
use ropey::Rope;
use similar::{DiffOp, TextDiff};
use tokio::task::JoinSet;
use tower_lsp::{
    lsp_types::{
        notification::Progress, request::WorkDoneProgressCreate, ClientCapabilities, Diagnostic,
        DiagnosticRelatedInformation, DiagnosticSeverity, DocumentSymbol, Location, NumberOrString,
        Position, ProgressParams, ProgressParamsValue, ProgressToken, Range, TextEdit, Url,
        WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    },
    Client,
};
use tracing::{debug, error, instrument};
use tree_sitter::{InputEdit, Language, Parser, Point};

use crate::utils;

pub enum Job {
    ComputeSolcDiagnostics(Url),
}

impl std::fmt::Debug for Job {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Job::ComputeSolcDiagnostics(path) => {
                write!(f, "ComputeSolcDiagnostics({})", path)
            }
        }
    }
}

#[derive(Debug)]
pub struct BackendState {
    pub documents: DashMap<String, Source>,
    pub trees: DashMap<String, tree_sitter::Tree>,
    pub document_symbols: DashMap<String, Vec<DocumentSymbol>>,
    pub solc_diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub project_compilation_output: DashMap<PathBuf, ProjectCompileOutput>,
    pub jobs_sender: tokio::sync::mpsc::Sender<Job>,
    pub jobs_receiver: tokio::sync::RwLock<tokio::sync::mpsc::Receiver<Job>>,
    pub diagnostics_pushed_for: tokio::sync::RwLock<HashMap<Url, bool>>,
    pub client: Client,
    pub progress_id: std::sync::atomic::AtomicU64,
}

impl BackendState {
    pub fn new(client: Client) -> Self {
        let (sender, receiver) = tokio::sync::mpsc::channel::<Job>(100);

        Self {
            jobs_receiver: tokio::sync::RwLock::new(receiver),
            jobs_sender: sender,
            documents: Default::default(),
            trees: Default::default(),
            document_symbols: Default::default(),
            solc_diagnostics: Default::default(),
            project_compilation_output: Default::default(),
            client,
            diagnostics_pushed_for: Default::default(),
            progress_id: std::sync::atomic::AtomicU64::new(0),
        }
    }

    pub async fn process_jobs(&self) {
        let mut receiver = self.jobs_receiver.write().await;
        while let Some(job) = receiver.recv().await {
            debug!("processing job: {:?}", job);
            match job {
                Job::ComputeSolcDiagnostics(path) => {
                    let req_token = ProgressToken::Number(
                        self.progress_id
                            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                            as i32,
                    );
                    let create_req = self
                        .client
                        .send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                            token: req_token.clone(),
                        })
                        .await;
                    if create_req.is_ok() {
                        self.client
                            .send_notification::<Progress>(ProgressParams {
                                token: req_token.clone(),
                                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                                    WorkDoneProgressBegin {
                                        title: "Compiling".to_string(),
                                        cancellable: None,
                                        message: None,
                                        percentage: None,
                                    },
                                )),
                            })
                            .await;
                    }

                    let compile_result = self.compile_project(&path);
                    debug!(res = ?compile_result, "compiling finished");
                    if compile_result.is_ok() {
                        #[allow(clippy::unwrap_used)]
                        let root = utils::get_root_path(&path).unwrap();

                        // is okay because this branch will only execute if okay and inserted
                        #[allow(clippy::unwrap_used)]
                        let output = self.project_compilation_output.get(&root).unwrap();
                        let diagnostics = self.compile_errors_to_diagnostic(&root, output.clone());
                        debug!(
                            diagnostics = ?diagnostics
                                .iter()
                                .map(crate::utils::diagnostic_to_string)
                                .collect::<Vec<String>>(),
                            "diagnostics found"
                        );
                        self.solc_diagnostics.insert(root.clone(), diagnostics);
                        self.on_solc_diagnostics_update(&root).await;
                    }
                    if create_req.is_ok() {
                        self.client
                            .send_notification::<Progress>(ProgressParams {
                                token: req_token,
                                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(
                                    WorkDoneProgressEnd {
                                        message: Some("Compiling finished".to_string()),
                                    },
                                )),
                            })
                            .await
                    }
                }
            }
        }
    }

    #[instrument(skip_all)]
    pub async fn queue_job(&self, job: Job) -> anyhow::Result<()> {
        debug!(job = ?job, "queueing job");

        let sender = self.jobs_sender.clone();
        Ok(sender.send(job).await?)
    }

    #[instrument(skip_all)]
    fn compile_project(&self, path: &Url) -> anyhow::Result<()> {
        debug!(path = path.to_string(), "compiling project");
        let root_path = utils::get_root_path(path)?;
        let config = utils::get_foundry_config(path)?;
        let project = config.project()?;
        let mut sources = project.paths.read_input_files()?;

        // overwrite the sources for client owned documents
        for doc in self.documents.iter() {
            let key = doc.key().as_str();
            let val = doc.value();
            debug!(path = key, "patching client owned docs in source");
            sources.insert(
                key.replace("file://", "").to_string().into(),
                FoundrySource::new(val.text.clone()),
            );
        }

        let output = ProjectCompiler::with_sources(&project, sources)?.compile()?;
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
                let src_location = err.source_location();
                let processable = src_location.is_some()
                    && (std::path::Path::new(
                        // okay because already checked if is_some
                        #[allow(clippy::unwrap_used)]
                        &root.join(&src_location.as_ref().unwrap().file),
                    )
                    .exists())
                    && ({
                        // okay because already checked if is_some
                        #[allow(clippy::unwrap_used)]
                        root.join(&src_location.as_ref().unwrap().file)
                            .to_str()
                            .is_some()
                    });
                if !processable {
                    debug!(
                        err = ?err,
                        "solc error not processable"
                    );
                }

                processable
            })
            .filter_map(|err| {
                if let MultiCompilerError::Solc(err) = err {
                    Some(err)
                } else {
                    None
                }
            })
            .map(
                |err: &foundry_compilers::artifacts::Error| -> anyhow::Result<Diagnostic> {
                    let severity = match err.severity() {
                        Severity::Error => Some(DiagnosticSeverity::ERROR),
                        Severity::Warning => Some(DiagnosticSeverity::WARNING),
                        Severity::Info => Some(DiagnosticSeverity::INFORMATION),
                    };

                    // already checked and filtered out none values
                    #[allow(clippy::unwrap_used)]
                    let source_location = err.source_location();
                    let source_location = source_location.as_ref();
                    let source_location = source_location.expect("source location is none");
                    let path = root.join(&source_location.file);
                    // okay because already checked if is_some
                    #[allow(clippy::unwrap_used)]
                    let url_string = format!("file://{}", path.to_str().unwrap());
                    let url = Url::parse(&url_string)?;
                    let file_contents = self.read_file(url)?;
                    let src = Source::new(file_contents);
                    debug!(err = ?err, "error");

                    let source_location_to_range =
                        |src: &Source, source_location: &SourceLocation| -> anyhow::Result<Range> {
                            let index_to_position = |index| -> anyhow::Result<Position> {
                                if index > 0 {
                                    Ok(src.byte_index_to_position(index as usize)?)
                                } else {
                                    Ok(Position::default())
                                }
                            };
                            Ok(Range {
                                start: index_to_position(source_location.start)?,
                                end: index_to_position(source_location.end)?,
                            })
                        };

                    let related_informations = err
                        .secondary_source_locations
                        .iter()
                        .filter_map(|secondary_loc| {
                            if let SecondarySourceLocation {
                                start: Some(start),
                                end: Some(end),
                                file: Some(file),
                                message: Some(message),
                            } = secondary_loc
                            {
                                let path = {
                                    let fpath = root.join(file);
                                    fpath.to_str().map(|p| p.to_string())
                                };
                                if let Some(path) = path {
                                    let url_string = format!("file://{}", path);
                                    if let Ok(url) = Url::parse(&url_string) {
                                        if let Ok(file_contents) = self.read_file(url.clone()) {
                                            let src = Source::new(file_contents);
                                            debug!(err = ?err, "error");
                                            if let Ok(range) = source_location_to_range(
                                                &src,
                                                &SourceLocation {
                                                    file: "".to_string(),
                                                    start: *start,
                                                    end: *end,
                                                },
                                            ) {
                                                return Some((url, message.clone(), range));
                                            }
                                        }
                                    }
                                }
                            }

                            debug!(
                                secondary_loc = ?secondary_loc,
                                "secondary location not processable"
                            );
                            None
                        })
                        .map(|(file, message, range)| DiagnosticRelatedInformation {
                            location: Location { uri: file, range },
                            message,
                        })
                        .collect::<Vec<DiagnosticRelatedInformation>>();

                    let primary_diagnostic = Diagnostic {
                        range: source_location_to_range(&src, source_location)?,
                        severity,
                        source: Some("solc".to_string()),
                        code: err
                            .error_code()
                            .map(|x| x as i32)
                            .map(NumberOrString::Number),
                        code_description: None,
                        message: err.message.clone(),
                        related_information: if related_informations.is_empty() {
                            None
                        } else {
                            Some(related_informations)
                        },
                        tags: None,
                        data: Some(serde_json::json!({
                            "url": url_string
                        })),
                    };

                    Ok(primary_diagnostic)
                },
            )
            .filter_map(|res| {
                if res.is_err() {
                    error!(err = ?res, "error converting solc error to diagnostic");
                }
                res.ok()
            })
            .collect()
    }

    fn read_file(&self, file_path: Url) -> anyhow::Result<String> {
        // if the document is owned by client, use the synchronized
        // copy in server's memory
        if self.documents.contains_key(&file_path.to_string()) {
            // okay to unwrap because already checked if contains key
            #[allow(clippy::unwrap_used)]
            return Ok(self
                .documents
                .get(&file_path.to_string())
                .unwrap()
                .text
                .to_string());
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
                if let Some(url) = diagnostic_file_url(diagnostic) {
                    if let std::collections::hash_map::Entry::Vacant(e) = map.entry(url.clone()) {
                        e.insert(vec![diagnostic.clone()]);
                    } else {
                        // okay because already checked if is_some
                        #[allow(clippy::unwrap_used)]
                        map.get_mut(&url).unwrap().push(diagnostic.clone());
                    }
                }
            }
        }
        map
    }

    #[instrument(skip_all)]
    async fn on_solc_diagnostics_update(&self, root: &PathBuf) {
        let config = crate::utils::get_foundry_config_with_path(root);
        let error_codes_to_ignore = config
            .ok()
            .map(|c| {
                let error_codes = c.ignored_error_codes;
                tracing::info!(root_path = ?c.root.0, error_codes_to_ignore = ?error_codes, "error codes to ignore");
                error_codes
            })
            .unwrap_or_default();
        let grouped = self.get_grouped_diagnostics_by_file(root);
        let mut join_set = JoinSet::new();
        let grouped_keys: HashSet<Url> = grouped.keys().cloned().collect();
        let already_pushed = self
            .diagnostics_pushed_for
            .read()
            .await
            .keys()
            .cloned()
            .collect::<HashSet<Url>>();
        // debug!(
        //     grouped_keys = ?grouped_keys
        //         .iter()
        //         .map(|x| x.to_string())
        //         .collect::<Vec<String>>(),
        //     already_pushed = ?already_pushed
        //         .iter()
        //         .map(|x| x.to_string())
        //         .collect::<Vec<String>>(),
        //     "grouped keys and already pushed keys"
        // );

        let mut diagnostics_pushed_for = self.diagnostics_pushed_for.write().await;
        // clear fixed/stale diagnostics
        for url in already_pushed.difference(&grouped_keys) {
            let client_clone = self.client.clone();
            let uri = url.clone();
            debug!("clearing diagnostics for: {}", uri.to_string());
            join_set.spawn(async move {
                client_clone.publish_diagnostics(uri, vec![], None).await;
            });
            // diagnostics_pushed_for.remove(url);
        }

        for (uri, diags) in grouped {
            assert!(!diags.is_empty());
            let client_clone = self.client.clone();
            debug!(
                uri = uri.to_string(),
                diags = ?diags
                    .iter()
                    .map(crate::utils::diagnostic_to_string)
                    .collect::<Vec<String>>(),
                "publishing diagnostics for"
            );
            diagnostics_pushed_for.insert(uri.clone(), true);
            let diags = diags
                .into_iter()
                .filter(|diag| {
                    diag.code.is_none() || {
                        if let NumberOrString::Number(number) =
                            diag.code.as_ref().expect("shouldn't be happening")
                        {
                            !error_codes_to_ignore
                                .contains(&foundry_config::SolidityErrorCode::from(*number as u64))
                        } else {
                            true
                        }
                    }
                })
                .collect();
            // publish diagnostics of all files async
            join_set.spawn(async move {
                client_clone
                    .publish_diagnostics(uri.clone(), diags, None)
                    .await;
            });
        }

        while join_set.join_next().await.is_some() {}
    }
}

pub struct Backend {
    pub client: Client,
    pub client_capabilities: OnceLock<ClientCapabilities>,
    pub state: std::sync::Arc<BackendState>,
    language: Language,
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
    #[error("Position not found error")]
    PositionNotFoundError,
    #[error("Invalid location error")]
    InvalidLocationError,
    #[error("Option unwrap error")]
    OptionUnwrap,
    #[error("Ropey query error")]
    RopeyQueryError,
}

enum FileAction {
    Open,
    Update,
    Close,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let client_clone = client.clone();
        let language = tree_sitter_solidity::language();
        Self {
            client,
            client_capabilities: Default::default(),
            state: Arc::new(BackendState::new(client_clone)),
            language,
        }
    }

    pub fn parser(&self) -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(self.language)
            .expect("failed to load solidity grammar into tree-sitter parser");
        parser
    }

    #[instrument(skip_all)]
    pub async fn get_fmt_textedits(&self, file_path: Url) -> anyhow::Result<Option<Vec<TextEdit>>> {
        let config = utils::get_foundry_config(&file_path)?;
        let mut err = None;
        let file_contents = self.state.read_file(file_path.clone()).map_err(|_err| {
            debug!(file_path = file_path.to_string(), err = ?err, "read error");
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
        forge_fmt::format_to(&mut formatted_txt, parsed_src, config.fmt)?;
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
                            character: 0,
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
        let tree = self
            .parser()
            .parse(&file_contents, None)
            .expect("error parsing the file");
        self.state
            .documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.state.trees.insert(file_path.to_string(), tree);
        self.on_file_change(FileAction::Open, file_path).await;
    }

    pub async fn remove_file(&self, file_path: &Url) {
        self.state.documents.remove(&file_path.to_string());
        self.state.trees.remove(&file_path.to_string());
        self.on_file_change(FileAction::Close, file_path).await;
    }

    pub async fn update_file(&self, file_path: &Url, file_contents: String) {
        let tree = self
            .parser()
            .parse(&file_contents, None)
            .expect("error parsing the file");
        self.state.trees.insert(file_path.to_string(), tree);
        self.state
            .documents
            .insert(file_path.to_string(), Source::new(file_contents));
        self.on_file_change(FileAction::Update, file_path).await;
    }

    pub async fn update_file_range(&self, file_path: &Url, range: Range, text: String) {
        {
            let mut src = self
                .state
                .documents
                .get_mut(&file_path.to_string())
                .expect("given file doesnt exist in state");
            src.update_range(&range, text.as_str());
            debug!("updated file with incremental change");
        }
        {
            let src = &self
                .state
                .documents
                .get(&file_path.to_string())
                .expect("given file doesnt exist in state");
            let edit = src.change_range_to_input_edit(&range, text.as_str());

            let new_tree = {
                let mut old_tree = self
                    .state
                    .trees
                    .get_mut(&file_path.to_string())
                    .expect("given file tree doesnt exist in state");
                old_tree.edit(&edit);
                self.parser()
                    .parse(src.text.to_string(), Some(&old_tree))
                    .expect("error parsing new tree")
            };
            // let write_file = |tree: &tree_sitter::Tree| {
            //     let file = std::fs::File::create("/tmp/currently_parsed_tree.dot");
            //     if let Ok(file) = file {
            //         tree.print_dot_graph(&file);
            //     }
            // };
            // write_file(&new_tree);
            self.state.trees.insert(file_path.to_string(), new_tree);
            // *old_tree = new_tree;
            // std::fs::File::open("/tmp/currently_parsed_tree.dot")
            //     .map(|fd| old_tree.print_dot_graph(&fd))
            //     .map_err(|err| debug!(err =?err, "an error occurred"));

            debug!("finished parsing the tree with edits");
        }

        self.on_file_change(FileAction::Update, file_path).await;
    }

    async fn on_file_change(&self, action: FileAction, path: &Url) {
        if let FileAction::Update = action {
            self.state.document_symbols.remove(&path.to_string());
        }

        self.update_document_symbols(path).await;
        if self
            .state
            .queue_job(Job::ComputeSolcDiagnostics(path.clone()))
            .await
            .is_err()
        {
            // TODO: log something somewhere
        }
    }

    pub async fn update_document_symbols(&self, path: &Url) -> bool {
        let tree = self.state.trees.get(&path.to_string());
        if let Some(tree) = tree {
            let mut cursor = tree.walk();

            let src = self.state.read_file(path.clone());
            if let Ok(src) = src {
                let symbols = crate::features::document_symbols::get_document_symbols(
                    &mut cursor,
                    src.as_bytes(),
                );
                self.state
                    .document_symbols
                    .insert(path.to_string(), symbols);
                return true;
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct Source {
    pub text: Rope,
    pub line_lengths: Vec<usize>,
}

impl Source {
    pub fn new(source: String) -> Self {
        let line_lengths = source.as_str().lines().map(|x| x.len()).collect();

        Source {
            text: Rope::from_str(source.as_str()),
            line_lengths,
        }
    }

    pub fn update_range(&mut self, range: &Range, text: &str) {
        Self::edit_rope(&mut self.text, range, text);
    }

    pub fn position_to_char(&self, position: Position) -> usize {
        let start = self.text.line_to_char(position.line as usize);

        start + (position.character as usize)
    }

    pub fn position_to_byte(&self, position: Position) -> usize {
        self.text.char_to_byte(self.position_to_char(position))
    }

    pub fn char_to_byte(&self, char_idx: usize) -> usize {
        self.text.char_to_byte(char_idx)
    }

    pub fn edit_rope(rope: &mut Rope, range: &Range, text: &str) {
        let start = rope.line_to_char(range.start.line as usize);
        let start_char_idx = start + (range.start.character as usize);
        let end = rope.line_to_char(range.end.line as usize);
        let end_char_idx = end + (range.end.character as usize);
        rope.remove(start_char_idx..end_char_idx);
        rope.insert(start_char_idx, text);
    }

    pub fn change_range_to_input_edit(&self, range: &Range, text: &str) -> InputEdit {
        let start_char = self.position_to_char(range.start);
        let start_byte = self.char_to_byte(start_char);

        let end_char = self.position_to_char(range.end);
        let end_byte = self.char_to_byte(end_char);

        let n_char_next = text.len();
        let new_end_char = start_char + n_char_next;

        let mut next_rope = self.text.clone();
        Self::edit_rope(&mut next_rope, range, text);
        let new_end_byte = next_rope.char_to_byte(new_end_char);
        let new_end_line = next_rope.byte_to_line(new_end_byte);
        let new_end_line_start_char_idx = next_rope.line_to_char(new_end_line);
        let new_column = new_end_char - new_end_line_start_char_idx;

        InputEdit {
            start_byte,
            old_end_byte: end_byte,
            new_end_byte,
            start_position: Point {
                row: range.start.line as usize,
                column: range.start.character as usize,
            },
            old_end_position: Point {
                row: range.end.line as usize,
                column: range.end.character as usize,
            },
            new_end_position: Point {
                row: new_end_line,
                column: new_column,
            },
        }
    }

    #[instrument]
    pub fn byte_index_to_position(
        &self,
        index: usize,
    ) -> Result<tower_lsp::lsp_types::Position, BackendError> {
        let line_idx = self
            .text
            .try_byte_to_line(index)
            .map_err(|_| BackendError::RopeyQueryError)?;
        let line_char_idx = self
            .text
            .try_line_to_char(line_idx)
            .map_err(|_| BackendError::RopeyQueryError)?;
        let char_idx = self
            .text
            .try_byte_to_char(index)
            .map_err(|_| BackendError::RopeyQueryError)?;

        return Ok(tower_lsp::lsp_types::Position {
            line: line_idx as u32,
            character: (char_idx - line_char_idx) as u32,
        });
    }
}

pub fn diagnostic_file_url(diagnostic: &Diagnostic) -> Option<Url> {
    diagnostic
        .data
        .as_ref()
        .and_then(|data| data.get("url").and_then(|url| url.as_str()))
        .and_then(|url| Url::parse(url).ok())
}

#[allow(clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use super::*;

    mod source_tests {
        use super::*;

        #[allow(dead_code)]
        fn source_example_1() -> String {
            r#"contract flipper {
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
}"#
            .to_string()
        }

        #[test]
        fn test_edit_rope() {
            let source_1 = r#"
pragma solidity 0.8.26;

function pureFunction() pure returns (uint) {
    return 10;
}

contract Contract {
    uint stateUint = 10;

    constructor() payable {}
}
            "#;

            let mut rope = Rope::from_str(source_1);
            let range = Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            };
            Source::edit_rope(&mut rope, &range, "r");

            assert_eq!(
                rope.to_string().as_str(),
                r#"rpragma solidity 0.8.26;

function pureFunction() pure returns (uint) {
    return 10;
}

contract Contract {
    uint stateUint = 10;

    constructor() payable {}
}
            "#
            )
        }
    }
}
