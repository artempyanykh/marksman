use anyhow::Result;

use glob::Pattern;
use lsp_types::WorkspaceFolder;

use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};
use tokio::fs;

use tracing::debug;

use crate::{
    facts::{self, FactsDB},
    store,
    structure::NoteID,
    parser::NoteName,
};

#[derive(Default)]
pub struct Workspace {
    pub folders: Vec<(NoteFolder, FactsDB, Vec<Pattern>)>,
}

impl Workspace {
    pub async fn new(input_folders: &[NoteFolder]) -> Result<Workspace> {
        let mut workspace = Workspace::default();
        for f in input_folders {
            workspace.add_folder(f.clone()).await?;
        }

        Ok(workspace)
    }

    pub fn note_count(&self) -> usize {
        self.folders
            .iter()
            .map(|(_, facts, _)| facts.note_index().size())
            .sum()
    }

    pub fn owning_folder(&self, file: &Path) -> Option<(&NoteFolder, &FactsDB)> {
        self.folders
            .iter()
            .find(|(folder, _, _)| file.starts_with(&folder.root))
            .map(|(folder, facts, _)| (folder, facts))
    }

    pub fn owning_folder_mut(
        &mut self,
        file: &Path,
    ) -> Option<(&mut NoteFolder, &mut FactsDB, &[Pattern])> {
        self.folders
            .iter_mut()
            .find(|(folder, _, _)| file.starts_with(&folder.root))
            .map(|(folder, facts, ignores)| (folder, facts, ignores.as_slice()))
    }

    pub fn remove_folder(&mut self, path: &Path) {
        if let Some((idx, _)) = self
            .folders
            .iter()
            .enumerate()
            .find(|(_, (folder, _, _))| folder.root == path)
        {
            self.folders.remove(idx);
        }
    }

    pub async fn add_folder(&mut self, folder: NoteFolder) -> Result<()> {
        if self.owning_folder(&folder.root).is_some() {
            return Ok(());
        }

        let ignores = store::find_ignores(&folder.root).await?;
        let note_files = store::find_notes(&folder.root, &ignores).await?;
        debug!(
            "Workspace {}: found {} note files",
            folder.root.display(),
            note_files.len()
        );
        let facts = facts::FactsDB::from_files(&folder.root, &note_files, &ignores).await?;
        self.folders.push((folder, facts, ignores));
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct NoteFolder {
    pub root: PathBuf,
    pub name: String,
}

impl NoteFolder {
    pub fn from_workspace_folder(workspace_folder: &WorkspaceFolder) -> NoteFolder {
        NoteFolder {
            root: workspace_folder
                .uri
                .to_file_path()
                .expect("Failed to turn URI into a path"),
            name: workspace_folder.name.clone(),
        }
    }

    pub fn from_root_path(root: &Path) -> NoteFolder {
        NoteFolder {
            root: root.to_path_buf(),
            name: root
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| root.to_string_lossy().to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct NoteFile {
    pub root: Arc<Path>,
    pub path: Arc<Path>,
    pub name: Arc<NoteName>,
}

impl NoteFile {
    pub fn new(root: &Path, path: &Path) -> Self {
        let name: NoteName = NoteName::from_path(path, root);

        Self {
            root: root.into(),
            path: path.into(),
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoteIndex {
    notes: Arc<[NoteFile]>,
}

impl Default for NoteIndex {
    fn default() -> Self {
        Self {
            notes: Vec::new().into(),
        }
    }
}

impl NoteIndex {
    pub fn size(&self) -> usize {
        self.notes.len()
    }

    pub fn ids(&self) -> impl Iterator<Item = NoteID> {
        (0..self.notes.len()).into_iter().map(|i| i.into())
    }

    pub fn files(&self) -> impl Iterator<Item = &NoteFile> {
        self.notes.iter()
    }

    pub fn find_by_path(&self, path: &Path) -> Option<NoteID> {
        self.notes.iter().enumerate().find_map(|(idx, nf)| {
            if nf.path.as_ref() == path {
                Some(idx.into())
            } else {
                None
            }
        })
    }

    pub fn find_by_name(&self, name: &NoteName) -> Option<NoteID> {
        self.notes.iter().enumerate().find_map(|(idx, nf)| {
            if NoteName::from_path(&nf.path, &nf.root) == *name {
                Some(idx.into())
            } else {
                None
            }
        })
    }

    pub fn find_by_id(&self, id: NoteID) -> NoteFile {
        self.notes[id.to_usize()].clone()
    }

    pub fn with_note_file(&self, file: NoteFile) -> NoteIndex {
        let mut notes: HashSet<NoteFile> = self
            .notes
            .iter()
            .map(|x| x.to_owned())
            .collect::<HashSet<_>>();
        notes.insert(file);

        let notes = notes.into_iter().collect::<Vec<_>>();
        NoteIndex {
            notes: notes.into(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NoteText {
    pub version: Version,
    pub content: Arc<str>,
}

impl NoteText {
    pub fn new(version: Version, content: Arc<str>) -> Self {
        Self { version, content }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Version {
    Fs(SystemTime),
    Vs(i32),
}

impl Version {
    pub fn to_lsp_version(&self) -> Option<i32> {
        match self {
            Version::Vs(v) => Some(*v),
            _ => None,
        }
    }
}

pub async fn read_note(path: &Path, root: &Path, ignores: &[Pattern]) -> Result<Option<NoteText>> {
    if is_note_file(path, root, ignores) {
        let content = fs::read_to_string(path).await?;
        let meta = fs::metadata(path).await?;
        let version = Version::Fs(meta.modified()?);

        Ok(Some(NoteText::new(version, content.into())))
    } else {
        Ok(None)
    }
}

pub async fn find_notes(root_path: &Path, ignores: &[Pattern]) -> Result<Vec<PathBuf>> {
    find_notes_inner(root_path, ignores).await
}

async fn find_notes_inner<'a>(root_path: &Path, ignores: &[Pattern]) -> Result<Vec<PathBuf>> {
    let mut remaining_dirs = vec![root_path.to_path_buf()];
    let mut found_files = vec![];
    while let Some(dir_path) = remaining_dirs.pop() {
        let mut dir_contents = fs::read_dir(dir_path).await?;
        while let Some(entry) = dir_contents.next_entry().await? {
            let entry_type = entry.file_type().await?;
            let entry_path = entry.path();
            if entry_type.is_file() && is_note_file(&entry_path, root_path, ignores) {
                found_files.push(entry_path);
            } else if entry_type.is_dir() {
                remaining_dirs.push(entry_path);
            }
        }
    }
    Ok(found_files)
}

fn is_note_file(path: &Path, root: &Path, ignores: &[Pattern]) -> bool {
    let path_str = match path.strip_prefix(root).ok().and_then(|p| p.to_str()) {
        Some(str) => str,
        _ => return false,
    };
    let is_md = path
        .extension()
        .filter(|ext| ext.to_string_lossy().to_lowercase() == "md")
        .is_some();
    if !is_md {
        return false;
    }

    for pat in ignores {
        if pat.matches(path_str) {
            return false;
        }
    }

    true
}

pub async fn find_ignores(root_path: &Path) -> Result<Vec<Pattern>> {
    let supported_ignores = [".ignore", ".gitignore"];

    for ignore in &supported_ignores {
        let file = root_path.join(ignore);

        if file.exists() {
            debug!("Found ignore file: {}", file.display());

            let content = fs::read_to_string(file).await?;
            let mut patterns = Vec::new();
            for line in content.lines() {
                if let Ok(pat) = Pattern::new(line) {
                    patterns.push(pat);
                }

                // Because 'glob' searches for a "full match" we need to add a
                // _catch-all_ tail to all patterns
                let rest_pattern = if line.ends_with('/') {
                    line.to_string() + "**/*"
                } else {
                    line.to_string() + "/**/*"
                };
                if let Ok(pat) = Pattern::new(&rest_pattern) {
                    patterns.push(pat);
                }
            }
            debug!("Found {} ignore patterns", patterns.len());

            return Ok(patterns);
        }
    }

    debug!("Found no ignore file");
    Ok(vec![])
}
