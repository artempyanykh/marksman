use anyhow::Result;

use glob::Pattern;
use once_cell::sync::OnceCell;
use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};
use tokio::fs;

use tracing::debug;

use crate::{
    parsing::{self, ElementWithLoc},
    text::OffsetMap,
};

#[derive(Debug)]
pub struct Note {
    pub version: Version,
    pub content: Arc<str>,
    lazy_offsets: OnceCell<OffsetMap<Arc<str>>>,
    lazy_elements: OnceCell<Vec<ElementWithLoc>>,
}

impl Note {
    pub fn new(version: Version, content: Arc<str>) -> Self {
        Self {
            version,
            content: content.clone(),
            lazy_offsets: OnceCell::new(),
            lazy_elements: OnceCell::new(),
        }
    }

    pub fn offsets(&self) -> &OffsetMap<Arc<str>> {
        self.lazy_offsets
            .get_or_init(|| OffsetMap::new(self.content.clone()))
    }

    pub fn elements(&self) -> &[ElementWithLoc] {
        self.lazy_elements
            .get_or_init(|| parsing::scrape(self.content.borrow()))
    }
}

#[derive(Debug, Clone)]
pub enum Version {
    Fs(SystemTime),
    Vs(i32),
}

pub async fn read_note(path: &Path, ignores: &[Pattern]) -> Result<Option<Note>> {
    if is_note_file(path, ignores) {
        let content = fs::read_to_string(path).await?;
        let meta = fs::metadata(path).await?;
        let version = Version::Fs(meta.modified()?);

        Ok(Some(Note::new(version, content.into())))
    } else {
        return Ok(None);
    }
}

pub async fn find_notes(root_path: &Path, ignores: &[Pattern]) -> Result<Vec<PathBuf>> {
    find_notes_inner(root_path, ignores).await
}

async fn find_notes_inner<'a>(root_path: &Path, ignores: &[Pattern]) -> Result<Vec<PathBuf>> {
    let mut remaining_dirs = vec![root_path.to_path_buf()];
    let mut found_files = vec![];
    while let Some(dir_path) = remaining_dirs.pop() {
        let mut dir_contents = fs::read_dir(dir_path).await?;
        while let Some(entry) = dir_contents.next_entry().await? {
            let entry_type = entry.file_type().await?;
            let entry_path = entry.path();
            if entry_type.is_file() && is_note_file(&entry_path, ignores) {
                found_files.push(entry_path);
            } else if entry_type.is_dir() {
                remaining_dirs.push(entry_path);
            }
        }
    }
    Ok(found_files)
}

fn is_note_file(path: &Path, ignores: &[Pattern]) -> bool {
    let path_str = match path.to_str() {
        Some(str) => str,
        _ => return false,
    };
    let is_md = path
        .extension()
        .filter(|ext| ext.to_string_lossy().to_lowercase() == "md")
        .is_some();
    if !is_md {
        return false;
    }

    for pat in ignores {
        if pat.matches(path_str) {
            return false;
        }
    }

    return true;
}

pub async fn find_ignores(root_path: &Path) -> Result<Vec<Pattern>> {
    let supported_ignores = [".ignore", ".gitignore"];

    for ignore in &supported_ignores {
        let file = root_path.join(ignore);
        if file.exists() {
            debug!("Found ignore file: {}", file.display());

            let content = fs::read_to_string(file).await?;
            let mut patterns = Vec::new();
            for line in content.lines() {
                if let Ok(pat) = Pattern::new(line) {
                    patterns.push(pat);
                }
            }
            debug!("Found {} ignore patterns", patterns.len());

            return Ok(patterns);
        }
    }

    debug!("Found no ignore file");
    Ok(vec![])
}
