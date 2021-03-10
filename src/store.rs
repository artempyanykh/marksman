use anyhow::Result;

use glob::Pattern;
use once_cell::sync::OnceCell;
use std::{
    borrow::Borrow,
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};
use tokio::fs;

use tracing::debug;

use crate::{
    note::{self, Element, ElementWithLoc, Heading},
    text::{Offset, OffsetMap},
};

#[derive(Debug)]
pub struct Note {
    pub version: Version,
    pub content: Arc<str>,
    lazy_offsets: OnceCell<OffsetMap<Arc<str>>>,
    lazy_elements: OnceCell<Vec<ElementWithLoc>>,
    lazy_title: OnceCell<Option<(Heading, Range<usize>)>>,
}

impl Note {
    pub fn new(version: Version, content: Arc<str>) -> Self {
        Self {
            version,
            content: content,
            lazy_offsets: OnceCell::new(),
            lazy_elements: OnceCell::new(),
            lazy_title: OnceCell::new(),
        }
    }

    pub fn offsets(&self) -> &OffsetMap<Arc<str>> {
        self.lazy_offsets
            .get_or_init(|| OffsetMap::new(self.content.clone()))
    }

    pub fn elements(&self) -> &[ElementWithLoc] {
        self.lazy_elements
            .get_or_init(|| note::scrape(self.content.borrow()))
    }

    pub fn title(&self) -> Option<(&Heading, &Range<usize>)> {
        let title = self.lazy_title.get_or_init(|| {
            let elements = self.elements();
            elements.iter().find_map(|el| match el {
                (Element::Heading(hd), span) if hd.level == 1 => Some((hd.clone(), span.clone())),
                _ => None,
            })
        });

        match title {
            Some((heading, span)) => Some((heading, span)),
            _ => None,
        }
    }

    pub fn headings(&self) -> Vec<&Heading> {
        self.elements()
            .iter()
            .filter_map(|el| match el {
                (Element::Heading(hd), _) => Some(hd),
                _ => None,
            })
            .collect()
    }

    pub fn heading_with_text(&self, text: &str) -> Option<(&Heading, &Range<usize>)> {
        self.elements().iter().find_map(|el| match el {
            (Element::Heading(hd), span) if hd.text.contains(text) => Some((hd, span)),
            _ => None,
        })
    }

    pub fn element_at_offset(&self, offset: usize) -> Option<&ElementWithLoc> {
        self.elements()
            .iter()
            .find(|(_, span)| span.contains(&offset))
    }

    pub fn element_at_pos(&self, pos: &lsp_types::Position) -> Option<&ElementWithLoc> {
        let offset = match self.offsets().lsp_pos_to_offset(pos) {
            Some(Offset::Inner(off)) => off,
            _ => return None,
        };
        self.element_at_offset(offset)
    }

    pub fn elements_in_span(&self, range: &Range<Offset>) -> Vec<&ElementWithLoc> {
        let target_span =
            range.start.to_usize(self.content.len())..range.end.to_usize(self.content.len());
        let mut elements_in_offsets = Vec::new(); // strict inclusion
        for e in self.elements() {
            if target_span.contains(&e.1.start) && target_span.contains(&e.1.end) {
                elements_in_offsets.push(e);
            }
        }
        elements_in_offsets
    }

    pub fn elements_in_range(&self, range: &lsp_types::Range) -> Option<Vec<&ElementWithLoc>> {
        let span = self.offsets().lsp_pos_to_offset(&range.start)?
            ..self.offsets().lsp_pos_to_offset(&range.end)?;
        Some(self.elements_in_span(&span))
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
