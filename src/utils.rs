use std::default::Default;
use std::path::PathBuf;

use foundry_config::{figment::Figment, find_project_root_path, Config, RootPath};
use tower_lsp::lsp_types::Url;

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

pub fn get_foundry_config(url: &Url) -> Result<Config, BackendError> {
    let path = url_to_path(url)?;
    let dir = path.parent().unwrap().to_path_buf();
    let root_path = find_project_root_path(Some(&dir)).map_or(RootPath(dir), RootPath);
    // crate::append_to_file!(
    //     "/Users/meet/solidity-analyzer.log",
    //     "root_path: {:?}",
    //     root_path
    // );
    let config = Config::from_provider(Into::<Figment>::into(Config {
        __root: root_path,
        ..Default::default()
    }));
    // crate::append_to_file!("/Users/meet/solidity-analyzer.log", "config: {:#?}", config);
    Ok(config)
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
        assert_eq!(matches!(path, BackendError::UnprocessableUrlError), true);
    }
}
