use std::{
    collections::HashMap,
    hash::Hash,
    path::{Path, PathBuf},
};

use anyhow::Result;
use glob::Pattern;
use lsp_types::{SymbolInformation, Url};

use crate::{
    note::{Element, Heading},
    store::{self, Note},
    text::text_matches_query,
};

#[derive(Debug, Default)]
pub struct GlobalIndex<T>
where
    T: Default,
{
    notes: HashMap<T, Note>,
}

impl<T> GlobalIndex<T>
where
    T: Eq + Hash + Default + std::fmt::Debug + Clone,
{
    pub fn insert(&mut self, tag: T, note: Note) {
        self.notes.insert(tag, note);
    }

    pub fn find(&self, tag: &T) -> Option<&Note> {
        self.notes.get(tag)
    }

    pub fn require(&self, tag: &T) -> &Note {
        self.find(tag)
            .unwrap_or_else(|| panic!("Required a note for non-existent tag: {:?}", tag))
    }

    pub fn notes(&self) -> impl Iterator<Item = (&T, &Note)> {
        self.notes.iter()
    }
}

impl GlobalIndex<PathBuf> {
    pub async fn from_files(files: &[PathBuf], ignores: &[Pattern]) -> Result<Self> {
        let mut empty = Self::default();

        for file in files {
            empty.with_file(file, ignores).await?;
        }

        Ok(empty)
    }

    pub async fn with_file(&mut self, file: &Path, ignores: &[Pattern]) -> Result<()> {
        let note = store::read_note(file, ignores).await?;
        if let Some(note) = note {
            self.insert(file.to_path_buf(), note);
        }

        Ok(())
    }

    #[allow(deprecated)]
    pub fn headings(&self, tag: PathBuf, query: &str) -> Vec<SymbolInformation> {
        let mut symbols = Vec::new();

        let note = match self.notes.get(&tag) {
            Some(t) => t,
            _ => return symbols,
        };

        for (element, span) in note.elements() {
            match element {
                Element::Heading(Heading {
                    text: heading_text, ..
                }) if text_matches_query(heading_text.as_str(), query) => {
                    let lsp_range = match note.offsets().range_to_lsp_range(span) {
                        Some(r) => r,
                        _ => continue,
                    };
                    let uri = Url::from_file_path(&tag).unwrap();
                    let location = lsp_types::Location::new(uri, lsp_range);
                    let symbol = lsp_types::SymbolInformation {
                        name: heading_text.clone(),
                        kind: lsp_types::SymbolKind::String,
                        tags: None,
                        deprecated: None,
                        location,
                        container_name: None,
                    };
                    symbols.push(symbol)
                }
                _ => (),
            }
        }

        symbols
    }

    pub fn headings_all(&self, query: &str) -> Vec<SymbolInformation> {
        let mut symbols = Vec::new();
        for tag in self.notes.keys() {
            symbols.append(&mut self.headings(tag.clone(), query));
        }

        symbols
    }
}
