use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
};

use anyhow::Result;
use glob::Pattern;
use lsp_types::{
    CompletionItem, DidChangeTextDocumentParams, Documentation, Hover, HoverContents, Location,
    MarkupContent, SemanticToken, SemanticTokenType, SemanticTokensLegend, SymbolInformation,
    TextDocumentIdentifier, TextDocumentItem, Url,
};
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use serde_json;

use tracing::debug;

use crate::{
    facts::{FactsDB, NoteFacts, NoteFactsDB, NoteFactsExt},
    store::{NoteFile, NoteText, Version},
    structure::{Element, ElementWithLoc, NoteName},
    text::{self, text_matches_query, OffsetMap},
};

//////////////////////////////////////////
// Text Sync
/////////////////////////////////////////

pub fn note_apply_changes(facts: &mut FactsDB, path: &Path, changes: &DidChangeTextDocumentParams) {
    if let Some(note_id) = facts.note_index().find_by_path(path) {
        let note = facts.note_facts(note_id);
        let note_text = note.text();
        let mut final_text = note_text.content.to_string();

        for change in &changes.content_changes {
            final_text = text::apply_change(
                &final_text,
                &OffsetMap::new(final_text.as_str()),
                change.range,
                &change.text,
            );
        }

        let final_version = Version::Vs(changes.text_document.version);
        let final_note = NoteText::new(final_version, final_text.into());
        facts.update_note(note_id, final_note);
    }
}

pub fn note_open(facts: &mut FactsDB, root: &Path, path: &Path, document: &TextDocumentItem) {
    let note = NoteText::new(Version::Vs(document.version), document.text.clone().into());
    let note_file = NoteFile::new(root, path);
    facts.insert_note(note_file, note);
}

pub async fn note_close(
    facts: &mut FactsDB,
    root: &Path,
    id: &TextDocumentIdentifier,
    ignores: &[Pattern],
) -> Result<()> {
    let path = id.uri.to_file_path().expect("Failed to turn uri into path");
    facts.with_file(root, &path, ignores).await
}

pub fn status_notification(num_notes: usize) -> lsp_server::Notification {
    let value = serde_json::json!({"state": "ok", "notes": num_notes});
    lsp_server::Notification {
        method: "zeta-note/status".to_string(),
        params: value,
    }
}

//////////////////////////////////////////
// Symbols
/////////////////////////////////////////

#[allow(deprecated)]
pub fn document_symbols(facts: &FactsDB, path: &Path, query: &str) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();

    let note_id = match facts.note_index().find_by_path(path) {
        Some(t) => t,
        _ => return symbols,
    };
    let note = facts.note_facts(note_id);
    let structure = note.structure();

    let matching_ids = note.headings_matching(|hd| text_matches_query(hd.text.as_str(), query));
    let matching_els = structure.headings_with_ids(&matching_ids);

    for (hd, span) in matching_els {
        let lsp_range = match note.indexed_text().range_to_lsp_range(&span) {
            Some(r) => r,
            _ => continue,
        };
        let uri = Url::from_file_path(&note.file().path).unwrap();
        let location = lsp_types::Location::new(uri, lsp_range);
        let symbol = lsp_types::SymbolInformation {
            name: hd.text.clone(),
            kind: lsp_types::SymbolKind::String,
            tags: None,
            deprecated: None,
            location,
            container_name: None,
        };
        symbols.push(symbol)
    }

    symbols
}

pub fn workspace_symbols(facts: &FactsDB, query: &str) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let note_index = facts.note_index();
    let files = note_index.files();
    for nf in files {
        symbols.append(&mut document_symbols(facts, &nf.path, query));
    }

    symbols
}

//////////////////////////////////////////
// Completion
/////////////////////////////////////////

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
    facts: &FactsDB,
    current_tag: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Vec<CompletionItem>> {
    let encl_note_id = facts.note_index().find_by_path(current_tag)?;
    let encl_note = facts.note_facts(encl_note_id);
    let encl_structure = encl_note.structure();

    let (enclosing_el, _) = encl_structure.elements_by_id(encl_note.element_at_lsp_pos(pos)?);
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

        for candidate_id in facts.note_index().ids() {
            if candidate_id == encl_note_id {
                // Don't try to complete the current note
                continue;
            }

            let cand = facts.note_facts(candidate_id);
            let cand_struct = cand.structure();

            if let Some((title, _)) = cand.title().map(|id| cand_struct.heading_by_id(id)) {
                if !text::text_matches_query(&title.text, &partial_input) {
                    continue;
                }

                let name = NoteName::from_path(&cand.file().path, root);
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

        let cand_id = facts.note_index().find_by_path(&target_tag)?;
        let cand = facts.note_facts(cand_id);
        let cand_struct = cand.structure();

        let query = enclosing_link_ref.heading.clone().unwrap_or_default();
        let candidate_headings: Vec<_> =
            cand.headings_matching(|hd| text::text_matches_query(&hd.text, &query));
        let candidate_headings = cand_struct.headings_with_ids(&candidate_headings);

        for (hd, _) in candidate_headings {
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

pub fn completion_resolve(facts: &FactsDB, unresolved: &CompletionItem) -> Option<CompletionItem> {
    let completion_type = unresolved
        .data
        .clone()
        .map(serde_json::from_value::<CompletionType>)
        .and_then(Result::ok)?;

    match completion_type {
        CompletionType::NoteCompletion { note_name, .. } => {
            let note_id = facts.note_index().find_by_name(&note_name)?;
            let note = facts.note_facts(note_id);

            let documentation = Documentation::MarkupContent(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: note.text().content.to_string(),
            });

            Some(CompletionItem {
                documentation: Some(documentation),
                ..unresolved.clone()
            })
        }
        CompletionType::HeadingCompletion { note_name, heading } => {
            let note_id = facts.note_index().find_by_name(&note_name)?;
            let note = facts.note_facts(note_id);
            let structure = note.structure();
            let (heading, _) = structure.heading_by_id(note.heading_with_text(&heading)?);
            let content = &note.text().content[heading.scope.clone()];
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

//////////////////////////////////////////
// Hover, Go to
/////////////////////////////////////////

pub fn hover(
    root: &Path,
    facts: &FactsDB,
    path: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Hover> {
    let note_id = facts.note_index().find_by_path(path)?;
    let note_name = NoteName::from_path(path, root);
    let note = facts.note_facts(note_id);
    let note_structure = note.structure();
    let (hovered_el, span) = note_structure.elements_by_id(note.element_at_lsp_pos(pos)?);

    if let Element::LinkRef(link_ref) = hovered_el {
        let range = note.indexed_text().range_to_lsp_range(&span);

        let target_note_name = link_ref.note_name.clone().unwrap_or_else(|| note_name);

        let target_id = facts.note_index().find_by_name(&target_note_name)?;
        let target_note = facts.note_facts(target_id);
        let target_struct = target_note.structure();
        let target_text = target_note.text();
        let text = if let Some(heading) = &link_ref.heading {
            let (heading, _) =
                target_struct.heading_by_id(target_note.heading_with_text(&heading)?);

            &target_text.content[heading.scope.clone()]
        } else {
            &target_text.content[..]
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
    facts: &FactsDB,
    path: &PathBuf,
    pos: &lsp_types::Position,
) -> Option<Location> {
    let source_id = facts.note_index().find_by_path(path)?;
    let source_note = facts.note_facts(source_id);
    let souce_index = source_note.structure();
    let (encl_el, _) = souce_index.elements_by_id(source_note.element_at_lsp_pos(pos)?);

    if let Element::LinkRef(link_ref) = encl_el {
        let target_note_name = link_ref
            .note_name
            .clone()
            .unwrap_or_else(|| NoteName::from_path(path, root));

        let target_id = facts.note_index().find_by_name(&target_note_name)?;
        let target_note = facts.note_facts(target_id);
        let target_struct = target_note.structure();
        let (_, target_range) = if let Some(link_heading) = &link_ref.heading {
            target_struct.heading_by_id(target_note.heading_with_text(link_heading)?)
        } else {
            target_struct.heading_by_id(target_note.title()?)
        };
        let range = target_note
            .indexed_text()
            .range_to_lsp_range(&target_range)
            .unwrap();

        return Some(Location {
            uri: Url::from_file_path(&target_note.file().path).unwrap(),
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
    facts: &FactsDB,
    path: &PathBuf,
    range: &lsp_types::Range,
) -> Option<Vec<SemanticToken>> {
    let note_id = facts.note_index().find_by_path(path)?;
    let note = facts.note_facts(note_id);
    let element_ids = note.elements_in_lsp_range(range)?;
    let strukt = note.structure();
    let elements = strukt.elements_with_ids(&element_ids).collect();
    Some(semantic_tokens_encode(note, elements))
}

pub fn semantic_tokens_full(facts: &FactsDB, path: &PathBuf) -> Option<Vec<SemanticToken>> {
    let note_id = facts.note_index().find_by_path(path)?;
    let note = facts.note_facts(note_id);
    let strukt = note.structure();

    let elements = strukt
        .elements_with_loc()
        .into_iter()
        .map(|(_, ewl)| ewl)
        .collect();
    Some(semantic_tokens_encode(note, elements))
}

fn semantic_tokens_encode(
    note: NoteFactsDB<'_>,
    mut elements: Vec<&ElementWithLoc>,
) -> Vec<SemanticToken> {
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
        let el_pos = note.indexed_text().range_to_lsp_range(&el_span).unwrap();
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
