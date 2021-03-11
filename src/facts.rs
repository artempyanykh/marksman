use std::{
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Result;

use glob::Pattern;
use salsa;

use crate::{
    note::{self, ElementID, ElementIndex, Heading, HeadingID, LinkRefID, NoteID},
    store::{self, NoteFile, NoteIndex, NoteText},
    text::{Offset, OffsetMap},
};

#[salsa::query_group(FactsStorage)]
pub trait Facts: salsa::Database {
    #[salsa::input]
    fn note_index(&self, key: ()) -> Arc<NoteIndex>;

    #[salsa::input]
    fn note_text(&self, note_id: NoteID) -> Arc<NoteText>;

    fn note_indexed_text(&self, note_id: NoteID) -> Arc<OffsetMap<Arc<str>>>;

    fn note_element_index(&self, note_id: NoteID) -> Arc<ElementIndex>;

    fn note_title(&self, note_id: NoteID) -> Option<HeadingID>;

    fn note_elements(&self, note_id: NoteID) -> Arc<[ElementID]>;

    fn note_headings(&self, note_id: NoteID) -> Arc<[HeadingID]>;

    fn note_refs(&self, note_id: NoteID) -> Arc<[LinkRefID]>;
}

#[salsa::database(FactsStorage)]
#[derive(Default)]
pub struct FactsDBInternal {
    storage: salsa::Storage<Self>,
}

impl std::fmt::Debug for FactsDBInternal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("facts::DB(..)")
    }
}

impl salsa::Database for FactsDBInternal {}

#[derive(Debug, Default)]
pub struct FactsDB(FactsDBInternal);

impl FactsDB {
    pub fn insert_note(&mut self, note_file: NoteFile, note: NoteText) {
        let note_path = note_file.path.clone();
        let idx = self.note_index();
        let new_idx = idx.with_note_file(note_file);
        let new_id = new_idx.find_by_path(&note_path).unwrap();
        self.0.set_note_index((), new_idx.into());
        self.0.set_note_text(new_id, note.into());
    }

    pub fn remove_note(&self) {
        todo!()
    }

    pub fn update_note(&mut self, note_id: NoteID, note: NoteText) {
        self.0.set_note_text(note_id, note.into());
    }

    pub async fn from_files(root: &Path, files: &[PathBuf], ignores: &[Pattern]) -> Result<Self> {
        let mut empty = Self::default();

        for file in files {
            empty.with_file(root, file, ignores).await?;
        }

        Ok(empty)
    }

    pub async fn with_file(&mut self, root: &Path, file: &Path, ignores: &[Pattern]) -> Result<()> {
        let note = store::read_note(file, ignores).await?;
        if let Some(note) = note {
            let note_file = NoteFile {
                root: root.to_path_buf(),
                path: file.to_path_buf(),
            };
            self.insert_note(note_file, note);
        }

        Ok(())
    }

    pub fn note_facts(&self, note_id: NoteID) -> NoteFactsDB {
        NoteFactsDB {
            id: note_id,
            db: self,
        }
    }

    pub fn note_index(&self) -> Arc<NoteIndex> {
        self.0.note_index(())
    }

    pub fn note_text(&self, note_id: NoteID) -> Arc<NoteText> {
        self.0.note_text(note_id)
    }

    pub fn note_indexed_text(&self, note_id: NoteID) -> Arc<OffsetMap<Arc<str>>> {
        self.0.note_indexed_text(note_id)
    }

    pub fn note_element_index(&self, note_id: NoteID) -> Arc<ElementIndex> {
        self.0.note_element_index(note_id)
    }

    pub fn note_title(&self, note_id: NoteID) -> Option<HeadingID> {
        self.0.note_title(note_id)
    }

    pub fn note_elements(&self, note_id: NoteID) -> Arc<[ElementID]> {
        self.0.note_elements(note_id)
    }

    pub fn note_headings(&self, note_id: NoteID) -> Arc<[HeadingID]> {
        self.0.note_headings(note_id)
    }

    pub fn note_refs(&self, note_id: NoteID) -> Arc<[LinkRefID]> {
        self.0.note_refs(note_id)
    }
}

// Narrow facts to a particular note (simpler UX)

pub trait NoteFacts {
    fn text(&self) -> Arc<NoteText>;
    fn indexed_text(&self) -> Arc<OffsetMap<Arc<str>>>;
    fn element_index(&self) -> Arc<ElementIndex>;
    fn title(&self) -> Option<HeadingID>;
    fn elements(&self) -> Arc<[ElementID]>;
    fn headings(&self) -> Arc<[HeadingID]>;
    fn refs(&self) -> Arc<[LinkRefID]>;
}
pub trait NoteFactsExt {
    fn file(&self) -> Arc<NoteFile>;
    fn headings_matching(&self, pred: impl Fn(&Heading) -> bool) -> Vec<HeadingID>;
    fn heading_with_text(&self, text: &str) -> Option<HeadingID>;
    fn element_at_offset(&self, offset: usize) -> Option<ElementID>;
    fn element_at_lsp_pos(&self, pos: &lsp_types::Position) -> Option<ElementID>;
    fn elements_in_range(&self, range: &Range<Offset>) -> Vec<ElementID>;
    fn elements_in_lsp_range(&self, range: &lsp_types::Range) -> Option<Vec<ElementID>>;
}
pub struct NoteFactsDB<'a> {
    id: NoteID,
    db: &'a FactsDB,
}

impl<'a> NoteFactsDB<'a> {}

impl<'a> NoteFacts for NoteFactsDB<'a> {
    fn text(&self) -> Arc<NoteText> {
        self.db.note_text(self.id)
    }

    fn indexed_text(&self) -> Arc<OffsetMap<Arc<str>>> {
        self.db.note_indexed_text(self.id)
    }

    fn element_index(&self) -> Arc<ElementIndex> {
        self.db.note_element_index(self.id)
    }

    fn title(&self) -> Option<HeadingID> {
        self.db.note_title(self.id)
    }

    fn elements(&self) -> Arc<[ElementID]> {
        self.db.note_elements(self.id)
    }

    fn headings(&self) -> Arc<[HeadingID]> {
        self.db.note_headings(self.id)
    }

    fn refs(&self) -> Arc<[LinkRefID]> {
        self.db.note_refs(self.id)
    }
}

impl<'a> NoteFactsExt for NoteFactsDB<'a> {
    fn headings_matching(&self, pred: impl Fn(&Heading) -> bool) -> Vec<HeadingID> {
        let index = self.element_index();
        self.headings()
            .iter()
            .filter_map(|&id| {
                let (hd, _) = index.heading_by_id(id);
                if pred(hd) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn heading_with_text(&self, text: &str) -> Option<HeadingID> {
        self.headings_matching(|h| h.text == text)
            .first()
            .map(|x| *x)
    }

    fn element_at_offset(&self, offset: usize) -> Option<ElementID> {
        let index = self.element_index();
        index.elements_with_loc().into_iter().find_map(|(id, ewl)| {
            if ewl.1.contains(&offset) {
                Some(id)
            } else {
                None
            }
        })
    }

    fn element_at_lsp_pos(&self, pos: &lsp_types::Position) -> Option<ElementID> {
        let indexed_text = self.indexed_text();
        let offset = match indexed_text.lsp_pos_to_offset(pos) {
            Some(Offset::Inner(off)) => off,
            _ => return None,
        };
        self.element_at_offset(offset)
    }

    fn elements_in_range(&self, range: &Range<Offset>) -> Vec<ElementID> {
        let indexed_text = self.indexed_text();
        let target_span = range.start.to_usize(indexed_text.text.len())
            ..range.end.to_usize(indexed_text.text.len());

        let index = self.element_index();
        let mut elements_in_offsets = Vec::new(); // strict inclusion
        for (id, ewl) in index.elements_with_loc() {
            if target_span.contains(&ewl.1.start) && target_span.contains(&ewl.1.end) {
                elements_in_offsets.push(id);
            }
        }

        elements_in_offsets
    }

    fn elements_in_lsp_range(&self, lsp_range: &lsp_types::Range) -> Option<Vec<ElementID>> {
        let indexed_text = self.indexed_text();
        let range = indexed_text.lsp_pos_to_offset(&lsp_range.start)?
            ..indexed_text.lsp_pos_to_offset(&lsp_range.end)?;

        Some(self.elements_in_range(&range))
    }

    fn file(&self) -> Arc<NoteFile> {
        self.db.note_index().find_by_id(self.id)
    }
}

// Derived queries

fn note_indexed_text(db: &dyn Facts, note_id: NoteID) -> Arc<OffsetMap<Arc<str>>> {
    let note_text = db.note_text(note_id);
    Arc::new(OffsetMap::new(note_text.content.clone()))
}

fn note_element_index(db: &dyn Facts, note_id: NoteID) -> Arc<ElementIndex> {
    let text = db.note_text(note_id);
    let elements = note::scrape(&text.content);
    ElementIndex::new(elements).into()
}

fn note_elements(db: &dyn Facts, note_id: NoteID) -> Arc<[ElementID]> {
    db.note_element_index(note_id).elements().into()
}

fn note_headings(db: &dyn Facts, note_id: NoteID) -> Arc<[HeadingID]> {
    db.note_element_index(note_id).headings().into()
}

fn note_title(db: &dyn Facts, note_id: NoteID) -> Option<HeadingID> {
    let index = db.note_element_index(note_id);
    db.note_headings(note_id)
        .iter()
        .find(|id| index.heading_by_id(**id).0.level == 1)
        .map(|x| *x)
}

fn note_refs(db: &dyn Facts, note_id: NoteID) -> Arc<[LinkRefID]> {
    db.note_element_index(note_id).refs().into()
}
