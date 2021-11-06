use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use lsp_document::{Pos, TextAdapter};
use lsp_types::{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Url};
use tracing::debug;

use crate::{
    facts::{Facts, FactsDB, NoteFacts, NoteFactsDB, NoteFactsExt},
    store::NoteFile,
    structure::{Heading, NoteName},
};

#[derive(Debug, Default)]
pub struct DiagCollection {
    pub store: HashMap<NoteFile, HashSet<DiagWithLoc>>,
}

pub fn to_publish(
    file: &NoteFile,
    diags: &HashSet<DiagWithLoc>,
    facts: &FactsDB,
) -> Option<PublishDiagnosticsParams> {
    let index = facts.note_index();

    let note = facts.note_facts(index.find_by_path(&file.path)?);
    let text_version = note.text().version.to_lsp_version();
    let indexed_text = note.indexed_text();

    let lsp_diags: Vec<Diagnostic> = diags
        .iter()
        .map(|(d, r)| {
            let range = match indexed_text.range_to_lsp_range(r) {
                Some(r) => r,
                _ => return None,
            };

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: d.to_message(),
                ..Diagnostic::default()
            })
        })
        .flatten()
        .collect();

    let param = PublishDiagnosticsParams {
        uri: Url::from_file_path(file.path.clone()).unwrap(),
        diagnostics: lsp_diags,
        version: text_version,
    };

    Some(param)
}

pub type DiagWithLoc = (Diag, Range<Pos>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Diag {
    DupTitle { title: Heading },
    DupHeading { heading: Heading },
    BrokenRefNote { ref_note: NoteName },
    BrokenRefHeading { ref_note: NoteName, heading: String },
}

impl Diag {
    pub fn to_message(&self) -> String {
        match self {
            Diag::DupTitle { title } => format!(
                "Duplicate title `{}`. Each note should have at most one title",
                title.text
            ),
            Diag::DupHeading { heading } => format!("Duplicate heading `{}`", heading.text),
            Diag::BrokenRefNote { ref_note } => {
                format!("Reference to non-existent note `{}`", ref_note)
            }
            Diag::BrokenRefHeading { ref_note, heading } => format!(
                "Reference to non-existent heading `{}`{}",
                ref_note, heading
            ),
        }
    }
}

pub fn check_title(note: &impl NoteFactsExt) -> Vec<DiagWithLoc> {
    debug!("check_title: start");

    let hd_ids = note.headings_matching(|hd| hd.level == 1);
    debug!("check_title: found {} title ids", hd_ids.len());

    let strukt = note.structure();
    let duplicates = strukt.headings_with_ids(&hd_ids).into_iter().skip(1);

    let duplicate_diags = duplicates
        .map(|(t, r)| (Diag::DupTitle { title: t.clone() }, r))
        .collect::<Vec<_>>();

    debug!("check_title: reporting {}", duplicate_diags.len());
    duplicate_diags
}

pub fn check_headings(note: &impl NoteFactsExt) -> Vec<DiagWithLoc> {
    debug!("check_headings: start");

    let mut hd_ids_to_inspect = note
        .headings_matching(|hd| hd.level > 1)
        .into_iter()
        .collect::<HashSet<_>>();
    debug!(
        "check_headings: found {} heading ids",
        hd_ids_to_inspect.len()
    );

    let strukt = note.structure();
    let mut duplicates = Vec::new();
    while let Some(&cur_id) = hd_ids_to_inspect.iter().next() {
        hd_ids_to_inspect.remove(&cur_id);
        let cur_hd = strukt.heading_by_id(cur_id);

        let similar_text_ids = note
            .headings_matching(|hd| hd.text == cur_hd.0.text)
            .into_iter()
            .filter(|&id| id != cur_id)
            .collect::<Vec<_>>();

        if !similar_text_ids.is_empty() {
            for id in &similar_text_ids {
                hd_ids_to_inspect.remove(id);
            }

            duplicates.append(&mut strukt.headings_with_ids(&similar_text_ids));
        }
    }

    let duplicate_diags = duplicates
        .into_iter()
        .map(|(h, r)| (Diag::DupHeading { heading: h.clone() }, r))
        .collect::<Vec<_>>();

    debug!("check_headings: reporting {}", duplicate_diags.len());
    duplicate_diags
}

pub fn check_refs(facts: &dyn Facts, note: &impl NoteFactsExt) -> Vec<DiagWithLoc> {
    let mut diags = Vec::new();

    let strukt = note.structure();
    let ref_ids = note.refs();
    let refs = strukt.refs_with_ids(&ref_ids);

    for (link_ref, range) in refs {
        let target_name = link_ref
            .note_name
            .clone()
            .unwrap_or_else(|| (*note.file().name).clone());
        let target_id = facts.note_index(()).find_by_name(&target_name);
        match target_id {
            Some(id) => {
                let target_note = NoteFactsDB::new(facts, id);
                if let Some(heading) = &link_ref.heading {
                    if target_note.heading_with_text(heading).is_none() {
                        diags.push((
                            Diag::BrokenRefHeading {
                                ref_note: target_name,
                                heading: heading.to_string(),
                            },
                            range,
                        ));
                    }
                }
            }
            _ => {
                diags.push((
                    Diag::BrokenRefNote {
                        ref_note: target_name,
                    },
                    range,
                ));
            }
        }
    }

    diags
}
