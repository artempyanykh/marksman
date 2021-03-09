use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
};

use anyhow::Result;
use glob::Pattern;
use lsp_types::{
    CompletionItem, DidChangeTextDocumentParams, Documentation, MarkupContent,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
};
use serde::{Deserialize, Serialize};
use serde_json;
use tracing::debug;

use crate::{
    db::GlobalIndex,
    note::{self, Element, Link, NoteID},
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

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
pub enum CompletionType {
    NoteCompletion { note_id: NoteID },
    HeadingCompletion { note_id: NoteID, heading: String },
}

pub fn completion_candidates(
    root: &Path,
    index: &GlobalIndex<PathBuf>,
    current_tag: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Vec<CompletionItem>> {
    let encl_note = index.find(current_tag)?;
    let (enclosing_el, _) = encl_note.element_at_pos(pos)?;
    let (encl_text, encl_note_id, encl_heading) = match enclosing_el {
        Element::Link(Link::Ref {
            text,
            note_id,
            heading,
        }) => (text, note_id, heading),
        _ => return None,
    };

    let tries_to_match_note = encl_heading.is_none() && !encl_text.contains('@');

    let mut candidates = Vec::new();

    if tries_to_match_note {
        debug!("Mathing notes...");
        let partial_input = encl_note_id.clone().unwrap_or_default();

        for (tag, note) in index.notes() {
            if current_tag == tag {
                // Don't try to complete the current note
                continue;
            }

            if note.title().is_some()
                && text::text_matches_query(&note.title().unwrap().text, &partial_input)
            {
                let id = note::note_id_from_path(tag, root);
                let data = serde_json::to_value(CompletionType::NoteCompletion {
                    note_id: id.clone(),
                })
                .unwrap();
                candidates.push(CompletionItem {
                    label: note.title().unwrap().text.clone(),
                    kind: Some(lsp_types::CompletionItemKind::File),
                    detail: Some(id.clone()),
                    insert_text: Some(id),
                    data: Some(data),
                    ..CompletionItem::default()
                })
            }
        }
    } else {
        // tries to match a heading inside a note
        let target_note_id = match encl_note_id {
            Some(id) => id.to_string(),
            _ => note::note_id_from_path(current_tag, root),
        };
        let target_tag = match encl_note_id {
            Some(id) => root.join(id).with_extension("md"),
            _ => current_tag.to_path_buf(),
        };
        debug!("Mathing headings inside {:?}...", target_tag);

        let target_note = index.find(&target_tag)?;
        let query = encl_heading.clone().unwrap_or_default();
        let candidate_headings: Vec<_> = target_note
            .headings()
            .into_iter()
            .filter(|hd| text::text_matches_query(&hd.text, &query))
            .collect();

        for hd in candidate_headings {
            if hd.level == 1 {
                // no need to complete on heading level 1 as it should be unique
                // in the document and file link points to it
                continue;
            }
            let data = serde_json::to_value(CompletionType::HeadingCompletion {
                note_id: target_note_id.clone(),
                heading: hd.text.to_string(),
            })
            .unwrap();
            candidates.push(CompletionItem {
                label: hd.text.to_string(),
                kind: Some(lsp_types::CompletionItemKind::Text),
                data: Some(data),
                ..CompletionItem::default()
            })
        }
    }

    if candidates.is_empty() {
        None
    } else {
        Some(candidates)
    }
}

pub fn completion_resolve(
    root: &Path,
    index: &GlobalIndex<PathBuf>,
    unresolved: &CompletionItem,
) -> Option<CompletionItem> {
    let completion_type = unresolved
        .data
        .clone()
        .map(serde_json::from_value::<CompletionType>)
        .and_then(Result::ok)?;

    match completion_type {
        CompletionType::NoteCompletion { note_id, .. } => {
            let tag = root.join(note_id).with_extension("md");
            let note = index.find(&tag)?;

            let documentation = Documentation::MarkupContent(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: note.content.to_string(),
            });

            Some(CompletionItem {
                documentation: Some(documentation),
                ..unresolved.clone()
            })
        }
        CompletionType::HeadingCompletion { note_id, heading } => {
            let tag = root.join(note_id).with_extension("md");
            let note = index.find(&tag)?;
            let heading = note.headings().into_iter().find(|hd| hd.text == heading)?;
            let content = &note.content[heading.scope.clone()];
            let documentation = Documentation::MarkupContent(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: content.to_string(),
            });

            Some(CompletionItem {
                documentation: Some(documentation),
                ..unresolved.clone()
            })
        }
    }
}
