use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
};

use anyhow::Result;
use glob::Pattern;
use lsp_types::{
    CompletionItem, DidChangeTextDocumentParams, Documentation, Hover, HoverContents, Location,
    MarkupContent, SemanticToken, SemanticTokenType, SemanticTokensLegend,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Url,
};
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use serde_json;

use tracing::debug;

use crate::{
    db::GlobalIndex,
    note::{Element, ElementWithLoc, NoteName},
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

pub fn status_notification(num_notes: usize) -> lsp_server::Notification {
    let value = serde_json::json!({"state": "ok", "notes": num_notes});
    lsp_server::Notification {
        method: "zeta-note/status".to_string(),
        params: value,
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
pub enum CompletionType {
    NoteCompletion {
        note_name: NoteName,
    },
    HeadingCompletion {
        note_name: NoteName,
        heading: String,
    },
}

pub fn completion_candidates(
    root: &Path,
    index: &GlobalIndex<PathBuf>,
    current_tag: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Vec<CompletionItem>> {
    let encl_note = index.find(current_tag)?;
    let (enclosing_el, _) = encl_note.element_at_pos(pos)?;
    let enclosing_link_ref = match enclosing_el {
        Element::LinkRef(r) => r,
        _ => return None,
    };

    let tries_to_match_note =
        enclosing_link_ref.heading.is_none() && !enclosing_link_ref.text.contains('@');

    let mut candidates = Vec::new();

    if tries_to_match_note {
        debug!("Mathing notes...");
        let partial_input = enclosing_link_ref
            .note_name
            .clone()
            .map(NoteName::into_string)
            .unwrap_or_default();

        for (tag, note) in index.notes() {
            if current_tag == tag {
                // Don't try to complete the current note
                continue;
            }

            if let Some((title, _)) = note.title() {
                if !text::text_matches_query(&title.text, &partial_input) {
                    continue;
                }

                let name = NoteName::from_path(tag, root);
                let data = serde_json::to_value(CompletionType::NoteCompletion {
                    note_name: name.clone(),
                })
                .unwrap();
                candidates.push(CompletionItem {
                    label: title.text.clone(),
                    kind: Some(lsp_types::CompletionItemKind::File),
                    detail: Some(name.to_string()),
                    insert_text: Some(name.to_string()),
                    data: Some(data),
                    ..CompletionItem::default()
                })
            }
        }
    } else {
        // tries to match a heading inside a note
        let target_note_name = match &enclosing_link_ref.note_name {
            Some(name) => name.clone(),
            _ => NoteName::from_path(current_tag, root),
        };
        let target_tag = match &enclosing_link_ref.note_name {
            Some(name) => name.to_path(root),
            _ => current_tag.to_path_buf(),
        };
        debug!("Mathing headings inside {:?}...", target_tag);

        let target_note = index.find(&target_tag)?;
        let query = enclosing_link_ref.heading.clone().unwrap_or_default();
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
                note_name: target_note_name.clone(),
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
        CompletionType::NoteCompletion { note_name, .. } => {
            let tag = note_name.to_path(root);
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
        CompletionType::HeadingCompletion { note_name, heading } => {
            let tag = note_name.to_path(root);
            let note = index.find(&tag)?;
            let (heading, _) = note.heading_with_text(&heading)?;
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

pub fn hover(
    root: &Path,
    index: &GlobalIndex<PathBuf>,
    path: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Hover> {
    let note = index.find(path)?;
    let (hovered_el, span) = note.element_at_pos(pos)?;

    if let Element::LinkRef(link_ref) = hovered_el {
        let range = note.offsets().range_to_lsp_range(&span);

        let target_note_name = link_ref
            .note_name
            .clone()
            .map(|name| name.to_path(root))
            .unwrap_or_else(|| path.clone());

        let note = index.find(&target_note_name)?;
        let text = if let Some(heading) = &link_ref.heading {
            let (heading, _) = note.heading_with_text(&heading)?;

            let text = &note.content[heading.scope.clone()];
            text
        } else {
            let text = &note.content[..];
            text
        };

        let markup = MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: text.to_string(),
        };

        return Some(Hover {
            contents: HoverContents::Markup(markup),
            range,
        });
    }

    None
}

pub fn goto_definition(
    root: &Path,
    index: &GlobalIndex<PathBuf>,
    path: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Location> {
    let note = index.find(path)?;
    let (encl_el, _) = note.element_at_pos(pos)?;

    if let Element::LinkRef(link_ref) = encl_el {
        let taget_note_name = link_ref
            .note_name
            .clone()
            .unwrap_or_else(|| NoteName::from_path(path, root));

        let target_tag = taget_note_name.to_path(root);
        let target_note = index.find(&target_tag)?;
        let (_, target_span) = if let Some(link_heading) = &link_ref.heading {
            target_note.heading_with_text(link_heading)?
        } else {
            target_note.title()?
        };
        let range = target_note
            .offsets()
            .range_to_lsp_range(target_span)
            .unwrap();

        return Some(Location {
            uri: Url::from_file_path(&target_tag).unwrap(),
            range,
        });
    }

    None
}

//////////////////////////////////////////
// Semantic tokens
/////////////////////////////////////////

pub fn semantic_token_type_mapping(tok_type: &SemanticTokenType) -> u32 {
    if *tok_type == SemanticTokenType::CLASS {
        // Heading
        0
    } else if *tok_type == SemanticTokenType::PROPERTY {
        // LinkRef
        1
    } else {
        unimplemented!("Unsupported token type: {}", tok_type.as_str())
    }
}

static LAZY_SEMANTIC_TOKENS_LEGEND: Lazy<SemanticTokensLegend> = Lazy::new(|| {
    let token_types = vec![SemanticTokenType::CLASS, SemanticTokenType::PROPERTY];
    let token_modifiers = Vec::new();
    SemanticTokensLegend {
        token_types,
        token_modifiers,
    }
});

pub fn semantic_tokens_legend() -> &'static SemanticTokensLegend {
    LAZY_SEMANTIC_TOKENS_LEGEND.borrow()
}

pub fn semantic_tokens_range(
    index: &GlobalIndex<PathBuf>,
    path: &PathBuf,
    range: &lsp_types::Range,
) -> Option<Vec<SemanticToken>> {
    let note = index.find(path)?;
    let elements = note.elements_in_range(range)?;
    Some(semantic_tokens_encode(note, elements))
}

pub fn semantic_tokens_full(
    index: &GlobalIndex<PathBuf>,
    path: &PathBuf,
) -> Option<Vec<SemanticToken>> {
    let note = index.find(path)?;
    let elements = note.elements().iter().collect();
    Some(semantic_tokens_encode(note, elements))
}

fn semantic_tokens_encode(note: &Note, mut elements: Vec<&ElementWithLoc>) -> Vec<SemanticToken> {
    // Sort before so that deltas are ok to calculate
    elements.sort_by_key(|(_, span)| span.start);

    let mut encoded = Vec::new();
    let mut cur_line = 0;
    let mut cur_char_offset = 0;

    for (el, el_span) in elements {
        let token_type = match el {
            Element::Heading(..) => SemanticTokenType::CLASS,
            Element::LinkRef(..) => SemanticTokenType::PROPERTY,
            _ => continue,
        };
        let el_pos = note.offsets().range_to_lsp_range(&el_span).unwrap();
        // Can't handle multiline tokens properly so skip.
        // Would be nice to improve at some point
        if el_pos.end.line > el_pos.start.line {
            continue;
        }

        let delta_line = el_pos.start.line - cur_line;
        let delta_start = if delta_line == 0 {
            el_pos.start.character - cur_char_offset
        } else {
            el_pos.start.character
        };
        let length = el_pos.end.character - el_pos.start.character;

        let token = SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: semantic_token_type_mapping(&token_type),
            token_modifiers_bitset: 0,
        };
        encoded.push(token);
        cur_line = el_pos.start.line;
        cur_char_offset = el_pos.start.character;
    }

    encoded
}
