use std::{
    borrow::Borrow,
    collections::HashSet,
    path::{Path, PathBuf},
};

use anyhow::Result;

use lsp_document::TextMap;

use lsp_types::{
    CodeLens, CodeLensParams, Command, CompletionItem, CompletionParams,
    DidChangeTextDocumentParams, DocumentLink, DocumentLinkParams, Documentation,
    GotoDefinitionParams, Hover, HoverContents, HoverParams, Location, MarkupContent, Position,
    PublishDiagnosticsParams, SemanticToken, SemanticTokenType, SemanticTokensLegend,
    SemanticTokensParams, SemanticTokensRangeParams, SymbolInformation, TextDocumentIdentifier,
    TextDocumentItem, Url, WorkspaceFoldersChangeEvent,
};
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use serde_json;

use tracing::debug;

use crate::{
    diag::{self, DiagCollection, DiagWithLoc},
    facts::{NoteFacts, NoteFactsDB, NoteFactsExt},
    store::{NoteFile, NoteText, Version},
    structure::{Element, ElementWithLoc, NoteName},
};
use crate::{lsp::server::ClientName, store::Workspace};
use crate::{store::NoteFolder, util::text_matches_query};
use lsp_document::{self, IndexedText, TextAdapter};

//////////////////////////////////////////
// Workspace
/////////////////////////////////////////

pub async fn note_change_workspace_folders(
    workspace: &mut Workspace,
    event: &WorkspaceFoldersChangeEvent,
) -> Result<()> {
    for removed in &event.removed {
        let path = removed
            .uri
            .to_file_path()
            .expect("Couldn't convert URI to a file path");
        workspace.remove_folder(&path)
    }

    for added in &event.added {
        let folder = NoteFolder::from_workspace_folder(added);
        workspace.add_folder(folder).await?;
    }

    Ok(())
}

//////////////////////////////////////////
// Text Sync
/////////////////////////////////////////

pub fn note_apply_changes(
    workspace: &mut Workspace,
    client_name: ClientName,
    path: &Path,
    changes: &DidChangeTextDocumentParams,
) {
    let (_, facts, _) = match workspace.owning_folder_mut(path) {
        Some(x) => x,
        _ => return,
    };

    if let Some(note_id) = facts.note_index().find_by_path(path) {
        let note = facts.note_facts(note_id);

        let our_version = note.text().version;
        // The problem with neovim's LSP is that text document versions are not consecutive:
        // * First notification is didOpen with version 0.
        // - Second notification is didChange with version 4.
        // - The version number increases by 2 when you make a change after an "Undo".
        // No idea whether this is a quirk in their text sync implementation but
        // the sync itself works fine or if the partial sync is borked, but it
        // seems we can't use this consistency check for Neovim.
        if !(client_name == ClientName::Neovim) {
            let target_version = Version::Vs(changes.text_document.version - 1);
            debug_assert!(
                our_version == target_version,
                "Document versions don't match: our({:?}), external({:?})",
                our_version,
                target_version
            );
        }

        let mut final_text = note.text().content.to_string();

        for lsp_change in &changes.content_changes {
            let indexed = IndexedText::new(final_text.as_ref());
            let change = indexed
                .lsp_change_to_change(lsp_change.clone())
                .expect("Couldn't translate LSP document change event");
            final_text = lsp_document::apply_change(&indexed, change);
        }

        let final_version = Version::Vs(changes.text_document.version);
        let final_note = NoteText::new(final_version, final_text.into());
        facts.update_note(note_id, final_note);
    }
}

pub fn note_open(workspace: &mut Workspace, path: &Path, document: &TextDocumentItem) {
    if let Some((folder, facts, _)) = workspace.owning_folder_mut(path) {
        let note = NoteText::new(Version::Vs(document.version), document.text.clone().into());
        let note_file = NoteFile::new(&folder.root, path);
        facts.insert_note(note_file, note);
    }
}

pub async fn note_close(workspace: &mut Workspace, id: &TextDocumentIdentifier) -> Result<()> {
    let path = id.uri.to_file_path().expect("Failed to turn uri into path");
    if let Some((folder, facts, ignores)) = workspace.owning_folder_mut(&path) {
        facts.with_file(&folder.root, &path, ignores).await
    } else {
        Ok(())
    }
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
pub fn document_symbols(workspace: &Workspace, path: &Path, query: &str) -> Vec<SymbolInformation> {
    debug!("document_symbols: start");

    let mut symbols = Vec::new();

    let (_, facts) = match workspace.owning_folder(path) {
        Some(x) => x,
        _ => return symbols,
    };

    let note_id = match facts.note_index().find_by_path(path) {
        Some(t) => t,
        _ => return symbols,
    };
    debug!("document_symbols: note_id={:?}", note_id);

    let note = facts.note_facts(note_id);
    let structure = note.structure();

    let matching_ids = note.headings_matching(|hd| text_matches_query(hd.text.as_str(), query));
    debug!("document_symbols: found {} ids", matching_ids.len());

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

pub fn workspace_symbols(workspace: &Workspace, query: &str) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    for (_, facts, _) in &workspace.folders {
        let note_index = facts.note_index();
        let files = note_index.files();
        for nf in files {
            symbols.append(&mut document_symbols(workspace, &nf.path, query));
        }
    }

    symbols
}

//////////////////////////////////////////
// Completion
/////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
pub enum CompletionType {
    NoteCompletion {
        root: PathBuf,
        note_name: NoteName,
    },
    HeadingCompletion {
        root: PathBuf,
        note_name: NoteName,
        heading: String,
    },
}

pub fn completion_candidates(
    workspace: &Workspace,
    params: CompletionParams,
) -> Option<Vec<CompletionItem>> {
    let current_tag = params
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let pos = params.text_document_position.position;

    let (folder, facts) = workspace.owning_folder(&current_tag)?;
    let root = &folder.root;

    let encl_note_id = facts.note_index().find_by_path(&current_tag)?;
    let encl_note = facts.note_facts(encl_note_id);
    let encl_structure = encl_note.structure();

    let (enclosing_el, _) = encl_structure.element_by_id(encl_note.element_at_lsp_pos(&pos)?);
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
                if !text_matches_query(&title.text, &partial_input) {
                    continue;
                }

                let name = NoteName::from_path(&cand.file().path, root);
                let data = serde_json::to_value(CompletionType::NoteCompletion {
                    root: root.clone(),
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
            _ => NoteName::from_path(&current_tag, root),
        };
        let target_tag = match &enclosing_link_ref.note_name {
            Some(name) => name.to_path(root),
            _ => current_tag,
        };
        debug!("Mathing headings inside {:?}...", target_tag);

        let cand_id = facts.note_index().find_by_path(&target_tag)?;
        let cand = facts.note_facts(cand_id);
        let cand_struct = cand.structure();

        let query = enclosing_link_ref.heading.clone().unwrap_or_default();
        let candidate_headings: Vec<_> =
            cand.headings_matching(|hd| text_matches_query(&hd.text, &query));
        let candidate_headings = cand_struct.headings_with_ids(&candidate_headings);

        for (hd, _) in candidate_headings {
            if hd.level == 1 {
                // no need to complete on heading level 1 as it should be unique
                // in the document and file link points to it
                continue;
            }
            let data = serde_json::to_value(CompletionType::HeadingCompletion {
                root: root.clone(),
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
    workspace: &Workspace,
    unresolved: &CompletionItem,
) -> Option<CompletionItem> {
    let completion_type = unresolved
        .data
        .clone()
        .map(serde_json::from_value::<CompletionType>)
        .and_then(Result::ok)?;

    match completion_type {
        CompletionType::NoteCompletion { root, note_name } => {
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

//////////////////////////////////////////
// Hover, Go to
/////////////////////////////////////////

pub fn hover(workspace: &Workspace, params: HoverParams) -> Option<Hover> {
    let path = params
        .text_document_position_params
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let pos = params.text_document_position_params.position;

    let (folder, facts) = workspace.owning_folder(&path)?;
    let root = &folder.root;

    let note_id = facts.note_index().find_by_path(&path)?;
    let note_name = NoteName::from_path(&path, root);
    let note = facts.note_facts(note_id);
    let note_structure = note.structure();
    let (hovered_el, span) = note_structure.element_by_id(note.element_at_lsp_pos(&pos)?);

    if let Element::LinkRef(link_ref) = hovered_el {
        let range = note.indexed_text().range_to_lsp_range(span);

        let target_note_name = link_ref.note_name.clone().unwrap_or(note_name);

        let target_id = facts.note_index().find_by_name(&target_note_name)?;
        let target_note = facts.note_facts(target_id);
        let target_struct = target_note.structure();
        let target_text = target_note.indexed_text();
        let text = if let Some(heading) = &link_ref.heading {
            let (heading, _) = target_struct.heading_by_id(target_note.heading_with_text(heading)?);

            target_text.substr(heading.scope.clone())?.to_string()
        } else {
            target_text.text().to_string()
        };

        let markup = MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: text,
        };

        return Some(Hover {
            contents: HoverContents::Markup(markup),
            range,
        });
    }

    None
}

pub fn goto_definition(workspace: &Workspace, params: GotoDefinitionParams) -> Option<Location> {
    let path = params
        .text_document_position_params
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let pos = params.text_document_position_params.position;

    let (folder, facts) = workspace.owning_folder(&path)?;
    let root = &folder.root;

    let source_id = facts.note_index().find_by_path(&path)?;
    let source_note = facts.note_facts(source_id);
    let souce_index = source_note.structure();
    let (encl_el, _) = souce_index.element_by_id(source_note.element_at_lsp_pos(&pos)?);

    if let Element::LinkRef(link_ref) = encl_el {
        let target_note_name = link_ref
            .note_name
            .clone()
            .unwrap_or_else(|| NoteName::from_path(&path, root));

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
    workspace: &Workspace,
    params: SemanticTokensRangeParams,
) -> Option<Vec<SemanticToken>> {
    let path = params.text_document.uri.to_file_path().unwrap();
    let (_, facts) = workspace.owning_folder(&path)?;
    let range = params.range;
    let note_id = facts.note_index().find_by_path(&path)?;
    let note = facts.note_facts(note_id);
    let element_ids = note.elements_in_lsp_range(&range)?;
    let strukt = note.structure();
    let elements = strukt.elements_with_ids(&element_ids).collect();
    Some(semantic_tokens_encode(note, elements))
}

pub fn semantic_tokens_full(
    workspace: &Workspace,
    params: SemanticTokensParams,
) -> Option<Vec<SemanticToken>> {
    let path = params.text_document.uri.to_file_path().unwrap();
    let (_, facts) = workspace.owning_folder(&path)?;
    let note_id = facts.note_index().find_by_path(&path)?;
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
            // SemanticTokenType::CLASS but skip for now as markdown syntax highlighting is already good eneough for headings
            Element::Heading(..) => continue,
            Element::LinkRef(..) => SemanticTokenType::PROPERTY,
            _ => continue,
        };
        let el_pos = note.indexed_text().range_to_lsp_range(el_span).unwrap();
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

//////////////////////////////////////////
// Diagnostics
/////////////////////////////////////////

pub fn diag(
    workspace: &Workspace,
    prev_diag_col: &DiagCollection,
) -> Option<(Vec<PublishDiagnosticsParams>, DiagCollection)> {
    debug!("Diagnostic check initiated");

    let mut changed = false;

    let mut new_col = DiagCollection::default();
    let mut diag_params = Vec::new();

    for (_, facts, _) in &workspace.folders {
        for note_id in facts.note_index().ids() {
            let note = facts.note_facts(note_id);
            let file = note.file();
            let diag = note.diag();
            let diag: HashSet<DiagWithLoc> = diag.iter().cloned().collect();

            let changed_for_file = if let Some(prev_set) = prev_diag_col.store.get(&file) {
                *prev_set != diag
            } else {
                true
            };

            if changed_for_file {
                changed = true;
                if let Some(param) = diag::to_publish(&file, &diag, facts) {
                    diag_params.push(param);
                }
            }

            new_col.store.insert(file, diag);
        }
    }

    if changed {
        Some((diag_params, new_col))
    } else {
        None
    }
}

//////////////////////////////////////////
// Code Lenses
/////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReferenceData {
    note_path: PathBuf,
    heading_text: String,
}

pub fn code_lenses(workspace: &Workspace, params: CodeLensParams) -> Option<Vec<CodeLens>> {
    let path = params.text_document.uri.to_file_path().unwrap();
    let (_, facts) = workspace.owning_folder(&path)?;
    let note = facts.note_facts(facts.note_index().find_by_path(&path)?);

    // Just generate dummy "references" lens for each heading
    // They will get resolved to actual commands separately
    let strukt = note.structure();
    let indexed_text = note.indexed_text();
    let mut lenses = Vec::new();

    for &h_id in &strukt.headings() {
        // Don't generate lenses for headings with no references
        let ref_count = note.refs_to_heading(h_id).len();
        if ref_count == 0 {
            continue;
        }

        let (heading, range) = strukt.heading_by_id(h_id);
        let lsp_range = match indexed_text.range_to_lsp_range(&range) {
            Some(lr) => lr,
            None => continue,
        };
        let ref_data = ReferenceData {
            note_path: path.clone(),
            heading_text: heading.text.to_string(),
        };
        let lens = CodeLens {
            range: lsp_range,
            command: None,
            data: Some(serde_json::to_value(ref_data).unwrap()),
        };

        lenses.push(lens);
    }

    Some(lenses)
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShowReferencesData {
    uri: Url,
    position: Position,
    locations: Vec<Location>,
}

pub fn code_lens_resolve(workspace: &Workspace, lens: &CodeLens) -> Option<CodeLens> {
    debug!("code_lens_resolve: start");

    let lens_data = lens.data.clone()?;
    let ref_data: ReferenceData = serde_json::from_value(lens_data).ok()?;
    let (_, facts) = workspace.owning_folder(&ref_data.note_path)?;
    let note = facts.note_facts(facts.note_index().find_by_path(&ref_data.note_path)?);
    let strukt = note.structure();

    let heading_id = note.heading_with_text(&ref_data.heading_text)?;
    let (_, heading_range) = strukt.heading_by_id(heading_id);
    let heading_lsp_pos = note.indexed_text().pos_to_lsp_pos(&heading_range.start)?;

    debug!(
        "code_lens_resolve: note_id={:?}, heading_id={:?}",
        note.id, heading_id
    );

    let references = note.refs_to_heading(heading_id);
    debug!("code_lens_resolve: found {} references", references.len());

    let mut locations: Vec<Location> = Vec::new();
    for (src_note_id, src_ref_id) in references.iter() {
        let src_note = facts.note_facts(*src_note_id);
        let src_indexed_text = src_note.indexed_text();
        let src_strukt = src_note.structure();

        let (_, src_range) = src_strukt.ref_by_id(*src_ref_id);
        let lsp_range = match src_indexed_text.range_to_lsp_range(&src_range) {
            Some(r) => r,
            _ => continue,
        };

        let loc = Location {
            uri: Url::from_file_path(src_note.file().path).unwrap(),
            range: lsp_range,
        };

        locations.push(loc)
    }

    let num_locs = locations.len();
    let arguments = if locations.is_empty() {
        None
    } else {
        let data = ShowReferencesData {
            uri: Url::from_file_path(note.file().path).unwrap(),
            position: heading_lsp_pos,
            locations,
        };
        Some(vec![serde_json::to_value(data).unwrap()])
    };
    let command = Command {
        title: format!("{} references", num_locs),
        command: "zetaNote.showReferences".to_string(),
        arguments,
    };

    Some(CodeLens {
        command: Some(command),
        ..lens.clone()
    })
}

//////////////////////////////////////////
// Document Links
/////////////////////////////////////////

pub fn document_links(
    workspace: &Workspace,
    params: DocumentLinkParams,
) -> Option<Vec<DocumentLink>> {
    let path = params.text_document.uri.to_file_path().unwrap();
    let (_, facts) = workspace.owning_folder(&path)?;
    let note = facts.note_facts(facts.note_index().find_by_path(&path)?);
    let strukt = note.structure();

    let mut links = Vec::new();

    for (ref_id, target_note_id, target_heading_id) in note.valid_refs().iter() {
        let (_, source_range) = strukt.ref_by_id(*ref_id);
        let target_note = facts.note_facts(*target_note_id);
        let target_strukt = target_note.structure();
        let target_heading_id = match target_heading_id.or(target_note.title()) {
            Some(id) => id,
            _ => continue,
        };
        let (_, target_range) = target_strukt.heading_by_id(target_heading_id);
        let target_range = target_note
            .indexed_text()
            .range_to_lsp_range(&target_range)
            .unwrap();

        let from_loc = Location {
            uri: Url::from_file_path(note.file().path).unwrap(),
            range: note
                .indexed_text()
                .range_to_lsp_range(&source_range)
                .unwrap(),
        };
        let to_loc = Location {
            uri: Url::from_file_path(target_note.file().path).unwrap(),
            range: target_range,
        };

        let data = serde_json::json!({
            "from": &from_loc,
            "to": &to_loc,
        });
        let data = serde_json::to_string(&data).unwrap();

        let mut target = Url::parse("command:zetaNote.followLink").unwrap();
        target.query_pairs_mut().append_key_only(&data);

        let link = DocumentLink {
            range: note
                .indexed_text()
                .range_to_lsp_range(&source_range)
                .unwrap(),
            target: Some(target),
            tooltip: None,
            data: None,
        };

        links.push(link)
    }

    Some(links)
}
