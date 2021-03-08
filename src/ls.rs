use std::borrow::Borrow;

use anyhow::Result;
use glob::Pattern;
use lsp_types::{
    DidChangeTextDocumentParams, TextDocumentContentChangeEvent, TextDocumentIdentifier,
    TextDocumentItem,
};

use crate::{
    store::{self, Note, Version},
    text,
};

fn note_apply_change(note: Note, change: &TextDocumentContentChangeEvent) -> Note {
    let new_text = text::apply_change(
        note.content.borrow(),
        note.offsets(),
        change.range,
        &change.text,
    );
    let infligh_version = Version::Vs(-1);
    Note::new(infligh_version, new_text.into())
}

pub fn note_apply_changes(note: &Note, changes: &DidChangeTextDocumentParams) -> Note {
    let mut final_note = Note::new(note.version.clone(), note.content.clone());

    for change in &changes.content_changes {
        final_note = note_apply_change(final_note, change);
    }

    let final_version = Version::Vs(changes.text_document.version);
    final_note.version = final_version;
    final_note
}

pub fn note_open(document: &TextDocumentItem) -> Note {
    Note::new(Version::Vs(document.version), document.text.clone().into())
}

pub async fn note_close(id: &TextDocumentIdentifier, ignores: &[Pattern]) -> Result<Option<Note>> {
    let path = id.uri.to_file_path().expect("Failed to turn uri into path");
    store::read_note(&path, ignores).await
}
