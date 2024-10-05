use crate::backend::BackendError;
use similar::{DiffOp, TextDiff};
use tower_lsp::lsp_types::{Position, Range, TextEdit};

pub async fn fmt(
    file_contents: String,
    config: foundry_config::Config,
) -> Result<Vec<TextEdit>, BackendError> {
    let parsed_src = forge_fmt::parse(&file_contents).map_err(|_| BackendError::ParseError);
    let parsed_src = parsed_src?;
    let mut formatted_txt = String::default();
    forge_fmt::format_to(&mut formatted_txt, parsed_src, config.fmt)
        .map_err(|_| BackendError::FormatError)?;
    let formatted_txt_lines = formatted_txt.lines().collect::<Vec<&str>>();

    let diff: TextDiff<'_, '_, '_, str> = TextDiff::from_lines(&file_contents, &formatted_txt);
    let text_edits = diff
        .ops()
        .iter()
        .flat_map(|diff_op| match diff_op {
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
        .collect();
    Ok(text_edits)
}
