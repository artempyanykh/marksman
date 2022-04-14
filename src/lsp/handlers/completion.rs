use std::path::PathBuf;

use anyhow::Result;

use lsp_document::{Pos, TextAdapter, TextMap};

use lsp_types::{CompletionItem, CompletionParams, Documentation, MarkupContent, TextEdit};

use serde::{Deserialize, Serialize};
use serde_json;

use tracing::debug;

use crate::facts::NoteFactsDB;
use crate::parser::{self, ElementWithLoc};
use crate::store::Workspace;
use crate::util::text_matches_query;
use crate::{
    facts::{NoteFacts, NoteFactsExt},
    parser::{Element, NoteName},
};

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
enum CompletionType {
    NoteCompletion {
        root: PathBuf,
        note_name: NoteName,
        note_title: String,
    },
    HeadingCompletion {
        root: PathBuf,
        note_name: NoteName,
        heading: String,
    },
}

impl CompletionType {
    pub fn note_name(&self) -> &NoteName {
        match self {
            CompletionType::NoteCompletion { note_name, .. } => note_name,
            CompletionType::HeadingCompletion { note_name, .. } => note_name,
        }
    }
}

pub fn completion_candidates(
    workspace: &Workspace,
    params: CompletionParams,
) -> Option<Vec<CompletionItem>> {
    let target_note_path = params
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let pos = params.text_document_position.position;

    let (folder, facts) = workspace.owning_folder(&target_note_path)?;
    let root = &folder.root;

    let encl_note_id = facts.note_index().find_by_path(&target_note_path)?;
    let encl_note = facts.note_facts(encl_note_id);
    let encl_structure = encl_note.structure();

    let enclosing_el_with_loc = encl_structure.element_by_id(encl_note.element_at_lsp_pos(&pos)?);
    let enclosing_link = match &enclosing_el_with_loc.0 {
        Element::InternLink(r) => r,
        _ => return None,
    };

    let tries_to_match_note =
        enclosing_link.heading.is_none() && !enclosing_link.text.contains(['|', '@']);

    let mut candidates = Vec::new();

    if tries_to_match_note {
        debug!("Matching notes...");
        let partial_input = enclosing_link
            .note_name
            .clone()
            .map(|n| n.to_string())
            .unwrap_or_default();

        for candidate_id in facts.note_index().ids() {
            if candidate_id == encl_note_id {
                // Don't try to complete the current note
                continue;
            }

            let cand = facts.note_facts(candidate_id);
            let cand_struct = cand.structure();

            if let Some((title, _)) = cand.title().map(|id| cand_struct.heading_by_id(id)) {
                let title = title.text.trim_start_matches('#').trim_start().to_string();
                if !text_matches_query(&title, &partial_input) {
                    continue;
                }

                let name = NoteName::from_path(&cand.file().path, root);
                let completion_type = CompletionType::NoteCompletion {
                    root: root.clone(),
                    note_name: name.clone(),
                    note_title: title.clone(),
                };
                let completion_item =
                    completion_item(&encl_note, enclosing_el_with_loc, &completion_type);
                candidates.push(completion_item)
            }
        }
    } else {
        // tries to match a heading inside a note
        let target_note_name = match &enclosing_link.note_name {
            Some(name) => name.clone(),
            _ => NoteName::from_path(&target_note_path, root),
        };
        let target_tag = match &enclosing_link.note_name {
            Some(name) => name.to_path(root),
            _ => target_note_path,
        };
        debug!("Mathing headings inside {:?}...", target_tag);

        let cand_id = facts.note_index().find_by_path(&target_tag)?;
        let cand = facts.note_facts(cand_id);
        let cand_struct = cand.structure();

        let query = enclosing_link.heading.clone().unwrap_or_default();
        let candidate_headings: Vec<_> =
            cand.headings_matching(|hd| text_matches_query(&hd.text, &query));
        let candidate_headings = cand_struct.headings_with_ids(&candidate_headings);

        for (hd, _) in candidate_headings {
            if hd.level == 1 {
                // no need to complete on heading level 1 as it should be unique
                // in the document and file link points to it
                continue;
            }
            let completion_type = CompletionType::HeadingCompletion {
                root: root.clone(),
                note_name: target_note_name.clone(),
                heading: hd.text.to_string(),
            };
            let completion_item =
                completion_item(&encl_note, enclosing_el_with_loc, &completion_type);
            candidates.push(completion_item)
        }
    }

    if candidates.is_empty() {
        None
    } else {
        Some(candidates)
    }
}

pub fn completion_resolve(
    workspace: &Workspace,
    unresolved: &CompletionItem,
) -> Option<CompletionItem> {
    let completion_type = unresolved
        .data
        .clone()
        .map(serde_json::from_value::<CompletionType>)
        .and_then(Result::ok)?;

    match completion_type {
        CompletionType::NoteCompletion {
            root, note_name, ..
        } => {
            let (_, facts) = workspace.owning_folder(&root)?;
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
        CompletionType::HeadingCompletion {
            root,
            note_name,
            heading,
        } => {
            let (_, facts) = workspace.owning_folder(&root)?;
            let note_id = facts.note_index().find_by_name(&note_name)?;
            let note = facts.note_facts(note_id);
            let structure = note.structure();
            let (heading, _) = structure.heading_by_id(note.heading_with_text(&heading)?);
            let content = note
                .indexed_text()
                .substr(heading.scope.clone())?
                .to_string();
            let documentation = Documentation::MarkupContent(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: content,
            });

            Some(CompletionItem {
                documentation: Some(documentation),
                ..unresolved.clone()
            })
        }
    }
}

fn completion_item(
    note_facts: &NoteFactsDB,
    complete_on: &ElementWithLoc,
    completion: &CompletionType,
) -> CompletionItem {
    let data = serde_json::to_value(completion).unwrap();
    let sep = if complete_on.0.text().contains(parser::SEP_AT) {
        parser::SEP_AT
    } else {
        parser::SEP_BAR
    };
    let is_intralink = *note_facts.file().name == *completion.note_name();
    let (kind, label, detail, filter_text, sort_text) = match completion {
        CompletionType::NoteCompletion {
            note_name,
            note_title,
            ..
        } => (
            lsp_types::CompletionItemKind::FILE,
            note_title.clone(),
            Some(note_name.to_string()),
            None,
            None,
        ),
        CompletionType::HeadingCompletion { heading, .. } => (
            lsp_types::CompletionItemKind::TEXT,
            heading.clone(),
            None,
            Some(completion_text(completion, is_intralink, sep)),
            Some(heading.to_string()),
        ),
    };
    let text_edit = completion_edit(note_facts, complete_on, completion, is_intralink, sep);
    CompletionItem {
        label,
        detail,
        kind: Some(kind),
        filter_text,
        sort_text,
        text_edit: Some(text_edit.into()),
        data: Some(data),
        ..CompletionItem::default()
    }
}

fn completion_edit(
    note_facts: &NoteFactsDB,
    completion_item: &ElementWithLoc,
    completion: &CompletionType,
    is_intralink: bool,
    sep: char,
) -> TextEdit {
    let (start_offset, end_offset) = match completion {
        CompletionType::NoteCompletion { .. } => (2, 1),
        CompletionType::HeadingCompletion { .. } => (2, 1),
    };
    let element_range = completion_item.1.clone();
    let completion_range = Pos::new(
        element_range.start.line,
        element_range.start.col + start_offset,
    )
        ..Pos::new(element_range.end.line, element_range.end.col - end_offset);

    let completion_range = note_facts
        .indexed_text()
        .range_to_lsp_range(&completion_range)
        .unwrap();

    let completion_text = completion_text(completion, is_intralink, sep);

    TextEdit {
        range: completion_range,
        new_text: completion_text,
    }
}

fn completion_text(completion: &CompletionType, is_intralink: bool, sep: char) -> String {
    match completion {
        CompletionType::NoteCompletion {
            root: _, note_name, ..
        } => format!("{note_name}"),
        CompletionType::HeadingCompletion {
            note_name, heading, ..
        } => {
            if is_intralink {
                format!("{sep}{heading}")
            } else {
                format!("{note_name}{sep}{heading}")
            }
        }
    }
}
