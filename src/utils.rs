use std::default::Default;
use std::path::{Path, PathBuf};

use foundry_config::{figment::Figment, Config};
use tower_lsp::lsp_types::{Diagnostic, Position, Range, Url};
use tracing::{debug, instrument};

use crate::backend::BackendError;

pub fn url_to_path(url: &Url) -> Result<PathBuf, BackendError> {
    let url = url.as_str();
    if !url.starts_with("file://") {
        Err(BackendError::UnprocessableUrlError)
    } else {
        let path = url.trim_start_matches("file://");
        Ok(PathBuf::from(path))
    }
}

pub fn get_foundry_config_with_path(path: &PathBuf) -> Result<Config, BackendError> {
    let root_path = get_root_path_from_path(path).or(Ok(path.clone()))?;
    debug!(root_path = ?root_path.to_string_lossy(), path = format!("{:?}", path), "root path found");

    let config = Config::from_provider(Into::<Figment>::into(Config {
        root: root_path,
        ..Default::default()
    }))
    .map_err(|_| BackendError::ConfigError)?;
    // crate::append_to_file!("/Users/meet/solidity-analyzer.log", "config: {:#?}", config);
    Ok(config)
}

#[instrument(skip_all)]
pub fn get_foundry_config(url: &Url) -> Result<Config, BackendError> {
    let path = url_to_path(url)?;
    get_foundry_config_with_path(&path)
}

pub fn get_root_path_from_path(path: &Path) -> anyhow::Result<PathBuf> {
    let path = dir(path).ok_or(anyhow::anyhow!("file has no parent??"))?;
    Ok(foundry_config::find_project_root(Some(path))?)
}

pub fn get_root_path(path: &Url) -> anyhow::Result<PathBuf> {
    let path = url_to_path(path)?;
    get_root_path_from_path(&path)
}

fn dir(path: &Path) -> Option<&Path> {
    if path.is_file() {
        path.parent()
    } else {
        Some(path)
    }
}

#[macro_export]
macro_rules! append_to_file {
    ($file:expr, $($content:tt)*) => {
        {
            use std::io::Write;
            // Open the file in append mode
            let mut file = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open($file)
                .unwrap_or_else(|err| {
                    panic!("Failed to open the file: {}", err);
                });

            // Format the content and write it to the file
            writeln!(file, $($content)*).unwrap_or_else(|err| {
                panic!("Failed to write to the file: {}", err);
            });
        }
    };
}

pub fn position_to_string(position: &Position) -> String {
    format!("{}:{}", position.line + 1, position.character + 1)
}

pub fn range_to_string(range: &Range) -> String {
    format!(
        "{}..{}",
        position_to_string(&range.start),
        position_to_string(&range.end)
    )
}

pub fn diagnostic_to_string(diagnostic: &Diagnostic) -> String {
    format!(
        "{source}[{error_code}][{file_name}][{range}] {message}",
        source = diagnostic
            .source
            .as_ref()
            .unwrap_or(&"-".to_string())
            .clone(),
        error_code = diagnostic
            .code
            .as_ref()
            .map_or("-".into(), |code| match code {
                tower_lsp::lsp_types::NumberOrString::Number(code) => code.to_string(),
                tower_lsp::lsp_types::NumberOrString::String(code) => code.clone(),
            }),
        range = range_to_string(&diagnostic.range),
        file_name = crate::backend::diagnostic_file_url(diagnostic)
            .map(|url| url.to_string())
            .unwrap_or("UNKNOWN".to_string()),
        message = diagnostic.message,
    )
}

#[allow(clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_to_path() {
        let url = Url::parse("file:///home/user/file.txt").unwrap();
        let path = url_to_path(&url).unwrap();
        assert_eq!(path, PathBuf::from("/home/user/file.txt"));

        let url = Url::parse("https:///home/user/file.txt").unwrap();
        let path = url_to_path(&url).err().unwrap();
        assert!(matches!(path, BackendError::UnprocessableUrlError));
    }

    #[test]
    fn test_get_root_path_from_path() {
        let cwd = std::env::current_dir().unwrap();
        let self_path = file!();
        let path = cwd.join(self_path);
        let parent = path.parent().unwrap().parent().unwrap();
        let root = get_root_path_from_path(&path).unwrap();
        assert_eq!(root, parent.to_path_buf());

        let root2 = get_root_path_from_path(parent).unwrap();
        assert_eq!(root2, parent.to_path_buf());
    }
}
