use std::{
    ops::{Deref, Range},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Result;

use glob::Pattern;
use salsa;

use crate::{
    diag::{self, DiagWithLoc},
    store::{self, NoteFile, NoteIndex, NoteText},
    structure::{self, ElementID, Heading, HeadingID, LinkRefID, NoteID, Structure},
};
use lsp_text::{IndexedText, Pos, TextAdapter, TextMap};

#[salsa::query_group(FactsStorage)]
pub trait Facts<'a>: salsa::Database {
    #[salsa::input]
    fn note_index(&self, key: ()) -> NoteIndex;

    #[salsa::input]
    fn note_content(&self, note_file: NoteFile) -> NoteText;

    fn note_text(&self, note_id: NoteID) -> NoteText;
    fn note_indexed_text(&self, note_id: NoteID) -> Arc<IndexedText<Arc<str>>>;
    fn note_structure(&self, note_id: NoteID) -> Structure;
    fn note_title(&self, note_id: NoteID) -> Option<HeadingID>;
    fn note_elements(&self, note_id: NoteID) -> Arc<[ElementID]>;
    fn note_headings(&self, note_id: NoteID) -> Arc<[HeadingID]>;
    fn note_refs(&self, note_id: NoteID) -> Arc<[LinkRefID]>;
    fn note_valid_refs(&self, note_id: NoteID) -> Arc<[(LinkRefID, NoteID, Option<HeadingID>)]>;
    fn note_refs_to_heading(
        &self,
        note_id: NoteID,
        headind_id: HeadingID,
    ) -> Arc<[(NoteID, LinkRefID)]>;
    fn note_diag(&self, note_id: NoteID) -> Arc<[DiagWithLoc]>;
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
    pub fn empty() -> Self {
        let mut db = Self::default();
        db.0.set_note_index((), NoteIndex::default().into());
        db
    }

    pub fn insert_note(&mut self, note_file: NoteFile, note: NoteText) {
        let idx = self.note_index();
        let new_idx = idx.with_note_file(note_file.clone());
        self.0.set_note_index((), new_idx.into());
        self.0.set_note_content(note_file, note.into());
    }

    pub fn remove_note(&self) {
        todo!()
    }

    pub fn update_note(&mut self, note_id: NoteID, note: NoteText) {
        let file = self.note_index().find_by_id(note_id);
        self.0.set_note_content(file.clone(), note.into());
    }

    pub async fn from_files(root: &Path, files: &[PathBuf], ignores: &[Pattern]) -> Result<Self> {
        let mut empty = Self::empty();

        for file in files {
            empty.with_file(root, file, ignores).await?;
        }

        Ok(empty)
    }

    pub async fn with_file(&mut self, root: &Path, path: &Path, ignores: &[Pattern]) -> Result<()> {
        let note = store::read_note(path, root, ignores).await?;
        if let Some(note) = note {
            let note_file = NoteFile::new(root, path);
            self.insert_note(note_file, note);
        }

        Ok(())
    }

    pub fn note_facts(&self, note_id: NoteID) -> NoteFactsDB {
        NoteFactsDB {
            id: note_id,
            db: &self.0,
        }
    }

    pub fn note_index(&self) -> NoteIndex {
        self.0.note_index(())
    }
}

// Narrow facts to a particular note (simpler UX)

pub trait NoteFacts {
    fn text(&self) -> NoteText;
    fn indexed_text(&self) -> Arc<IndexedText<Arc<str>>>;
    fn structure(&self) -> Structure;
    fn title(&self) -> Option<HeadingID>;
    fn elements(&self) -> Arc<[ElementID]>;
    fn headings(&self) -> Arc<[HeadingID]>;
    fn refs(&self) -> Arc<[LinkRefID]>;
    fn valid_refs(&self) -> Arc<[(LinkRefID, NoteID, Option<HeadingID>)]>;
    fn refs_to_heading(&self, heading_id: HeadingID) -> Arc<[(NoteID, LinkRefID)]>;
    fn diag(&self) -> Arc<[DiagWithLoc]>;
}
pub trait NoteFactsExt: NoteFacts {
    fn id(&self) -> NoteID;
    fn file(&self) -> NoteFile;
    fn headings_matching(&self, pred: impl Fn(&Heading) -> bool) -> Vec<HeadingID>;
    fn heading_with_text(&self, text: &str) -> Option<HeadingID>;
    fn element_at_pos(&self, pos: Pos) -> Option<ElementID>;
    fn element_at_lsp_pos(&self, pos: &lsp_types::Position) -> Option<ElementID>;
    fn elements_in_range(&self, range: &Range<Pos>) -> Vec<ElementID>;
    fn elements_in_lsp_range(&self, range: &lsp_types::Range) -> Option<Vec<ElementID>>;
}
pub struct NoteFactsDB<'a> {
    db: &'a dyn Facts,
    pub id: NoteID,
}

impl<'a> NoteFactsDB<'a> {
    pub fn new(db: &'a dyn Facts, id: NoteID) -> Self {
        Self { db, id }
    }
}

impl<'a> NoteFacts for NoteFactsDB<'a> {
    fn text(&self) -> NoteText {
        self.db.note_text(self.id)
    }

    fn indexed_text(&self) -> Arc<IndexedText<Arc<str>>> {
        self.db.note_indexed_text(self.id)
    }

    fn structure(&self) -> Structure {
        self.db.note_structure(self.id)
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

    fn diag(&self) -> Arc<[DiagWithLoc]> {
        self.db.note_diag(self.id)
    }

    fn valid_refs(&self) -> Arc<[(LinkRefID, NoteID, Option<HeadingID>)]> {
        self.db.note_valid_refs(self.id)
    }

    fn refs_to_heading(&self, heading_id: HeadingID) -> Arc<[(NoteID, LinkRefID)]> {
        self.db.note_refs_to_heading(self.id, heading_id)
    }
}

impl<'a> NoteFactsExt for NoteFactsDB<'a> {
    fn headings_matching(&self, pred: impl Fn(&Heading) -> bool) -> Vec<HeadingID> {
        let structure = self.structure();
        self.headings()
            .iter()
            .filter_map(|&id| {
                let (hd, _) = structure.heading_by_id(id);
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

    fn element_at_pos(&self, pos: Pos) -> Option<ElementID> {
        let structure = self.structure();
        let text = self.indexed_text();

        let mut candidates = structure
            .elements_with_loc()
            .into_iter()
            .filter_map(
                |(id, ewl)| {
                    if ewl.1.contains(&pos) {
                        Some(id)
                    } else {
                        None
                    }
                },
            )
            .collect::<Vec<_>>();

        // Since elements can overlap (e.g. a link within a heading) try to find
        // the most specific element
        candidates.sort_by_key(|id| {
            let (_, range) = structure.element_by_id(*id);
            let text_len = text
                .substr(range.clone())
                .map(|s| s.len())
                .unwrap_or_default();
            text_len
        });

        candidates.first().cloned()
    }

    fn element_at_lsp_pos(&self, pos: &lsp_types::Position) -> Option<ElementID> {
        let indexed_text = self.indexed_text();
        let pos = indexed_text.lsp_pos_to_pos(pos)?;
        self.element_at_pos(pos)
    }

    fn elements_in_range(&self, range: &Range<Pos>) -> Vec<ElementID> {
        let structure = self.structure();
        let mut els_in_range = Vec::new();
        for (id, ewl) in structure.elements_with_loc() {
            // strict inclusion
            if range.contains(&ewl.1.start) && range.contains(&ewl.1.end) {
                els_in_range.push(id);
            }
        }

        els_in_range
    }

    fn elements_in_lsp_range(&self, lsp_range: &lsp_types::Range) -> Option<Vec<ElementID>> {
        let indexed_text = self.indexed_text();
        let range = indexed_text.lsp_range_to_range(lsp_range)?;
        Some(self.elements_in_range(&range))
    }

    fn file(&self) -> NoteFile {
        self.db.note_index(()).find_by_id(self.id)
    }

    fn id(&self) -> NoteID {
        self.id
    }
}

// Derived queries

fn note_text(db: &dyn Facts, note_id: NoteID) -> NoteText {
    let file = db.note_index(()).find_by_id(note_id);
    db.note_content(file.clone())
}

fn note_indexed_text(db: &dyn Facts, note_id: NoteID) -> Arc<IndexedText<Arc<str>>> {
    let note_text = db.note_text(note_id);
    Arc::new(IndexedText::new(note_text.content.clone()))
}

fn note_structure(db: &dyn Facts, note_id: NoteID) -> Structure {
    let text = db.note_indexed_text(note_id);
    let elements = structure::scrape(&*text);
    Structure::new(elements)
}

fn note_elements(db: &dyn Facts, note_id: NoteID) -> Arc<[ElementID]> {
    db.note_structure(note_id).elements().into()
}

fn note_headings(db: &dyn Facts, note_id: NoteID) -> Arc<[HeadingID]> {
    db.note_structure(note_id).headings().into()
}

fn note_title(db: &dyn Facts, note_id: NoteID) -> Option<HeadingID> {
    let index = db.note_structure(note_id);
    db.note_headings(note_id)
        .iter()
        .find(|id| index.heading_by_id(**id).0.level == 1)
        .map(|x| *x)
}

fn note_refs(db: &dyn Facts, note_id: NoteID) -> Arc<[LinkRefID]> {
    db.note_structure(note_id).refs().into()
}

fn note_valid_refs(
    db: &dyn Facts,
    note_id: NoteID,
) -> Arc<[(LinkRefID, NoteID, Option<HeadingID>)]> {
    let cur_note = NoteFactsDB::new(db, note_id);
    let cur_strukt = cur_note.structure();

    cur_note
        .refs()
        .iter()
        .filter_map(|&rid| {
            let (linkref, _) = cur_strukt.ref_by_id(rid);
            let target_note_name = linkref
                .note_name
                .clone()
                .unwrap_or(cur_note.file().name.deref().clone());

            if let Some(target_id) = db.note_index(()).find_by_name(&target_note_name) {
                let target_note = NoteFactsDB::new(db, target_id);
                match &linkref.heading {
                    Some(heading_text) => match target_note.heading_with_text(&heading_text) {
                        Some(id) => Some((rid, target_id, Some(id))),
                        _ => None,
                    },
                    _ => Some((rid, target_id, target_note.title())),
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .into()
}

fn note_refs_to_heading(
    db: &dyn Facts,
    note_id: NoteID,
    heading_id: HeadingID,
) -> Arc<[(NoteID, LinkRefID)]> {
    let encl_note = NoteFactsDB::new(db, note_id);
    let encl_note_title = encl_note.title();
    let is_ref_to_title = encl_note_title.filter(|&tid| tid == heading_id).is_some();
    let mut found_refs = Vec::new();

    for cur_id in db.note_index(()).ids() {
        let cur_note = NoteFactsDB::new(db, cur_id);
        for (cur_rid, ref_to_note_id, ref_to_head_id) in cur_note.valid_refs().iter() {
            if *ref_to_note_id == note_id {
                match ref_to_head_id {
                    Some(targ_hd_id) => {
                        if *targ_hd_id == heading_id {
                            found_refs.push((cur_id, *cur_rid))
                        }
                    }
                    _ => {
                        if is_ref_to_title {
                            found_refs.push((cur_id, *cur_rid))
                        }
                    }
                }
            }
        }
    }

    found_refs.into()
}

fn note_diag(db: &dyn Facts, note_id: NoteID) -> Arc<[DiagWithLoc]> {
    let note_facts = NoteFactsDB::new(db, note_id);
    let mut diags = Vec::new();
    diags.append(&mut diag::check_title(&note_facts));
    diags.append(&mut diag::check_headings(&note_facts));
    diags.append(&mut diag::check_refs(db, &note_facts));

    diags.into()
}
