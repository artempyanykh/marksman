use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::Hash,
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Result;
use lsp_types::{Position, SymbolInformation, SymbolKind, Url};
use tokio::fs;

use crate::{
    note::Element,
    parsing,
    text::{text_matches_query, OffsetMap},
};

#[derive(Default)]
pub struct FileIndex {
    elements: Vec<(Element, Range<usize>)>,
}

impl FileIndex {
    pub fn new(text: &str) -> Self {
        let elements = parsing::scrape(text);
        Self { elements }
    }
}

pub struct Cache<T, V, S> {
    hydrate_fn: Box<dyn Fn(S) -> V>,
    inner: HashMap<T, V>,
}

impl<T: Eq + Hash, V, S: Borrow<str>> Cache<T, V, S> {
    pub fn new<F: Fn(S) -> V + 'static>(hydrate_fn: F) -> Self {
        Self {
            hydrate_fn: Box::new(hydrate_fn),
            inner: HashMap::new(),
        }
    }

    pub fn erase(&mut self, tag: &T) {
        self.inner.remove(tag);
    }

    pub fn retrieve(&mut self, tag: T, text: S) -> &V {
        self.inner.entry(tag).or_insert((self.hydrate_fn)(text))
    }
}

pub struct GlobalIndex<T>
where
    T: Default,
{
    content_map: HashMap<T, Arc<str>>,
    offset_cache: Cache<T, OffsetMap<Arc<str>>, Arc<str>>,
    file_index_cache: Cache<T, FileIndex, Arc<str>>,
}

impl<T> GlobalIndex<T>
where
    T: Eq + Hash + Default + std::fmt::Debug + Clone,
{
    pub fn empty() -> Self {
        let offset_cache = Cache::new(OffsetMap::new);
        let file_index_cache = Cache::new(|rs: Arc<str>| FileIndex::new(rs.borrow()));
        Self {
            content_map: HashMap::new(),
            offset_cache,
            file_index_cache,
        }
    }

    pub fn insert(&mut self, tag: T, content: String) {
        self.offset_cache.erase(&tag);
        self.file_index_cache.erase(&tag);
        self.content_map.insert(tag, content.into());
    }
}

impl GlobalIndex<PathBuf> {
    pub async fn from_files(files: &[PathBuf]) -> Result<Self> {
        let mut empty = Self::empty();

        for file in files {
            empty.with_file(file).await?;
        }

        Ok(empty)
    }

    pub async fn with_file(&mut self, file: &Path) -> Result<()> {
        let content = fs::read_to_string(file).await?;
        self.insert(file.to_path_buf(), content);

        Ok(())
    }

    pub fn headings_all(&mut self, query: &str) -> Vec<SymbolInformation> {
        let mut symbols = Vec::new();

        for (tag, text) in self.content_map.iter() {
            let file_index = self.file_index_cache.retrieve(tag.clone(), text.clone());
            let line_offset = self.offset_cache.retrieve(tag.clone(), text.clone());

            for (element, span) in &file_index.elements {
                match element {
                    Element::Heading {
                        text: heading_text, ..
                    } if text_matches_query(heading_text, query) => {
                        let lsp_range = match line_offset.range_to_lsp_range(span) {
                            Some(r) => r,
                            _ => continue,
                        };
                        let uri = Url::from_file_path(tag).unwrap();
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
        }

        symbols
    }
}
