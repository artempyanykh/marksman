use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use lsp_types::{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Url};
use tracing::debug;

use crate::{
    facts::{FactsDB, NoteFacts, NoteFactsExt},
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
                severity: Some(DiagnosticSeverity::Error),
                message: d.to_message(),
                ..Diagnostic::default()
            })
        })
        .filter_map(|x| x)
        .collect();

    let param = PublishDiagnosticsParams {
        uri: Url::from_file_path(file.path.clone()).unwrap(),
        diagnostics: lsp_diags,
        version: text_version,
    };

    Some(param)
}

// TODO: Use line + col for range as this is much better in terms of caching of results
pub type DiagWithLoc = (Diag, Range<usize>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Diag {
    DupTitle {
        title: Heading,
    },
    DupHeading {
        heading: Heading,
    },
    BrokenRefNote {
        ref_note: NoteName,
    },
    BrokenRefHeading {
        ref_note: Option<String>,
        heading: String,
    },
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
            Diag::BrokenRefHeading { ref_note, heading } => match ref_note {
                Some(ref_note) => format!(
                    "Reference to non-existent heading `{}`{}",
                    ref_note, heading
                ),
                _ => format!("Reference to non-existent heading {}", heading),
            },
        }
    }
}

pub fn check_title(note: &impl NoteFactsExt) -> Vec<DiagWithLoc> {
    debug!("check_title: start");

    let hd_ids = note.headings_matching(|hd| hd.level == 1);
    debug!("check_title: found {} title ids", hd_ids.len());

    let strukt = note.structure();
    let duplicates = strukt.headings_with_ids(&hd_ids).skip(1);

    let duplicate_diags = duplicates
        .map(|(t, r)| (Diag::DupTitle { title: t.clone() }, r.clone()))
        .collect::<Vec<_>>();

    debug!("check_title: reporting {}", duplicate_diags.len());
    duplicate_diags
}
