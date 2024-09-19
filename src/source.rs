use anyhow::{Context, Result};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

pub trait SourceProvider {
    fn get_source<T>(&mut self, path: &Path, get: impl FnOnce(&str) -> T) -> Result<Option<T>>;
}

pub struct FileSourceProvider;

impl SourceProvider for FileSourceProvider {
    fn get_source<T>(&mut self, path: &Path, get: impl FnOnce(&str) -> T) -> Result<Option<T>> {
        if path.extension().is_some_and(|ext| ext == "ctl") {
            let buffer = std::fs::read_to_string(path)
                .with_context(|| format!("loading path {}", path.display()))?;
            Ok(Some(get(&buffer)))
        } else {
            Ok(None)
        }
    }
}

#[derive(Default)]
pub struct CachingSourceProvider {
    cache: HashMap<PathBuf, String>,
}

impl CachingSourceProvider {
    pub fn new() -> Self {
        Self::default()
    }
}

impl SourceProvider for CachingSourceProvider {
    fn get_source<T>(&mut self, path: &Path, get: impl FnOnce(&str) -> T) -> Result<Option<T>> {
        if let Some(buffer) = self.cache.get(path) {
            Ok(Some(get(buffer)))
        } else {
            let buffer = std::fs::read_to_string(path)
                .with_context(|| format!("loading path {}", path.display()))?;
            let res = get(&buffer);
            self.cache.insert(path.into(), buffer);
            Ok(Some(res))
        }
    }
}
