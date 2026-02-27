# RFC: Cross-Folder Link Resolution via `extra_folders`

## Problem

Marksman resolves markdown links strictly within a single LSP workspace folder. Users with
multiple related projects — for example a shared notes repository referenced from several
project wikis — cannot create links across project boundaries:

- `[[shared-note]]` in a primary project flags as a broken link even when the note exists
  in an adjacent directory.
- Go-to-definition returns nothing for such links.
- Rename does not propagate to documents in other directories.

## Goal

Allow a project to declare one or more additional directories as *extra folders*. Links in
the primary project are resolved against both the primary folder and any extra folders.
Diagnostics, go-to-definition, hover, find-references, and rename all work across the
boundary.

## Principles

**The extra-folder transparency principle**: from a user's perspective, documents in extra
folders behave as if they were part of the primary collection. Every LSP feature that
operates on documents should include extra-folder documents.

**Concrete laws:**

| Feature | Law |
|---|---|
| Diagnostics | A link that resolves in any extra folder is not a broken link |
| Go-to-definition / Hover | Navigates to the extra-folder file (editor switches context) |
| Completion | Extra-folder docs appear as candidates alongside primary docs |
| Code action "create file" | Suppressed when the link resolves in any extra folder |
| Find references | Primary-folder docs that reference an extra-folder element are returned |
| Rename (from definition) | Propagates to all referencing docs in primary folders |
| Rename (from reference in extra folder) | Session-dependent; not guaranteed |
| Extra-folder index freshness | When a file in an extra folder changes on disk (e.g. saved by another LSP session), marksman re-indexes it automatically via `workspace/didChangeWatchedFiles` — no manual server restart required |

## Non-Goals

- Live reload of extra folders when the config changes (requires server restart).
- Recursive extra-folder chaining (A→B→C). A can declare B as extra, and B can be a
  primary folder with its own extra folders, but B's extra folders are not automatically
  visible from A.
- Diagnostics for broken links *inside* extra-folder documents. Those belong to the extra
  project's own LSP session.
- Automatic cross-session rename propagation when the two folders are attached to separate
  LSP instances. File-watcher notifications keep indices in sync, but rename edits are only
  computed and applied within a single session.

## Configuration

The feature is opt-in via a new `[core] extra_folders` array in `.marksman.toml` (or the
user-level config):

```toml
[core]
extra_folders = [
  "../shared-notes",          # relative to the config file's directory
  "/home/user/global-wiki",   # absolute path
]
```

Resolution order: the primary folder is searched first; extra folders are tried in order
only if the primary folder yields no result.

Both user-level (`~/.config/marksman/config.toml`) and project-level (`.marksman.toml`)
configs are supported. Project config takes priority over user config (same merge semantics
as all other settings).

## Architecture

### Key Principle

Per-folder `Conn` graphs are kept unchanged. Cross-folder resolution is added as a
**fallback at the query layer** rather than by extending the graph. This keeps the change
minimally invasive and avoids rebuilding the incremental reference machinery.

### Workspace Model

```
Workspace
├── folders: Map<FolderId, Folder>   ← primary + extra, all loaded as Folder objects
└── extraFolderIds: Set<FolderId>    ← which folders are "extra" (not primary)
```

Extra folders are real `Folder` objects loaded at startup via `Folder.tryLoad`. They are
tagged in `extraFolderIds` so that the rest of the system can distinguish them from primary
folders.

Helper functions:
- `Workspace.primaryFolders ws` — folders not in `extraFolderIds`
- `Workspace.extraFoldersFor folder ws` — extra folders declared by a given primary folder
- `Workspace.primaryFoldersReferencing extraFolderId ws` — inverse: which primary folders
  list the given folder as extra (used by rename)

### Cross-Folder Resolution (`Refs.fs`)

`Dest.tryResolveSym` materialises the primary-folder `Conn` result into an array. If the
array is empty it falls back to `tryResolveSymInExtraFolder` for each extra folder:

```
tryResolveSym folder extraFolders doc sym
  1. resolve via Conn graph (primary folder)
  2. if empty → for each extraFolder:
       tryResolveSymInExtraFolder complStyle doc.Id sym extraFolder
```

`tryResolveSymInExtraFolder` handles only `CrossRef` (intra-refs cannot cross folders).
It calls `Folder.filterDocsByName` directly — bypassing the Conn graph — and mirrors the
`Oracle.resolveInDoc` logic for `CrossDoc` and `CrossSection` refs.

### Diagnostics (`Diag.fs`)

`checkFolder`, `FolderDiag.mk`, and `WorkspaceDiag.mk` are extended with an
`extraFolders: seq<Folder>` parameter. `WorkspaceDiag.mk` iterates **primary folders
only** and passes each folder's extra folders into `FolderDiag.mk`. Extra-folder documents
do not receive diagnostics from the primary project's LSP session.

### Go-to-Definition / Hover (`Server.fs`)

```fsharp
let extraFolders = Workspace.extraFoldersFor folder (State.workspace state)
Dest.tryResolveElement folder extraFolders srcDoc atPos
```

The LSP `Location` type is just URI + range, so cross-folder destinations work without
further change.

### Find References / Rename (`Refs.fs`, `Refactor.fs`)

For *find references* and *rename*, the question is inverted: given a symbol in the current
folder, which documents in *other* folders reference it?

```fsharp
let referencingFolders = Workspace.primaryFoldersReferencing (Folder.id folder) ws
Dest.findElementRefs includeDecl folder referencingFolders srcDoc srcEl
```

`findDefRefs` iterates `referencingFolders` and scans each document's symbol list for
`CrossRef` syms whose `.Doc` slug matches the target document's slug. Matching elements
are yielded alongside the in-folder results. `Refactor.rename` receives `referencingFolders`
and threads it through to `findElementRefs`, so rename edits are produced for all
referencing folders.

### Code Lens (`Lenses.fs`)

`Lenses.forDoc` also receives `referencingFolders` and passes it through to
`findElementRefs` so reference counts and locations include cross-folder references.

### Extra-Folder File Watching (`Server.fs`)

When the `initialized` notification is received and the client supports dynamic registration
(`workspace.didChangeWatchedFiles.dynamicRegistration = true`), marksman sends
`client/registerCapability` with a `workspace/didChangeWatchedFiles` watcher for every
extra folder root loaded in the workspace.

**Watcher registration — RelativePattern is required.** The LSP spec allows two forms for
`globPattern`: a plain string, or a `RelativePattern` object `{ baseUri, pattern }`. Marksman
uses the `RelativePattern` form. A plain absolute-path string (e.g.
`/home/user/people/**/*.md`) is interpreted by clients such as neovim as relative to the
client's own workspace folders, so it would be silently prefixed with the workspace root and
never match a path outside it. The `RelativePattern` form names the base directory
explicitly and is set up as an independent fs-event watcher rooted at that directory,
delivering events correctly regardless of the client's workspace root.

When the client fires `workspace/didChangeWatchedFiles`:

- **Created / Changed** — the file is reloaded via `Doc.tryLoad` and inserted into the
  folder with `Folder.withDoc`. The next diagnostic cycle picks up the updated symbols.
- **Deleted** — the doc is removed with `Folder.withoutDoc`.

This ensures that when two projects declare each other as `extra_folders` and are attached
to *separate* LSP sessions, changes saved by one session are automatically visible to the
other without requiring a manual `LspRestart`.

**Diagnostic latency after a cross-folder rename.** When the primary LSP session applies a
rename, it writes workspace edits directly to the open buffers via `workspace/applyEdit`.
The buffer content changes immediately, but the file watcher only fires after the edited
buffers are **saved to disk**. Between the rename and the save, the other session's index is
stale: it sees the updated link text (delivered via `textDocument/didChange` for the buffer
it owns) but the old title in the extra-folder doc (not yet flushed). This produces a
transient broken-link diagnostic that disappears automatically within the client's event
coalesce window (≈100 ms in neovim) once all buffers are saved. The practical workflow is:
rename → `:wa` → diagnostics clear. No `LspRestart` is required.

## File Map

| File | Change |
|------|--------|
| `Marksman/Config.fs` | Add `coreExtraFolders` field; TOML parse; merge; accessor; `resolveExtraFolderPath` helper |
| `Marksman/Folder.fs` | Extend `MultiFile` with `extraFolderRoots: AbsPath[]`; populate in `tryLoad`; expose accessor |
| `Marksman/Workspace.fs` | Add `extraFolderIds`; `loadExtraFolders`; helper functions; logging |
| `Marksman/Refs.fs` | `tryResolveSymInExtraFolder`; thread `extraFolders` / `referencingFolders` through all entry points |
| `Marksman/Diag.fs` | Thread `extraFolders`; `WorkspaceDiag.mk` uses `primaryFolders` only |
| `Marksman/Lenses.fs` | Thread `referencingFolders` |
| `Marksman/Refactor.fs` | Add `referencingFolders` parameter |
| `Marksman/Server.fs` | Wire `extraFolders` / `referencingFolders` from workspace helpers into each handler; register extra-folder file watchers on `initialized`; implement `WorkspaceDidChangeWatchedFiles` |
| `Marksman/Compl.fs` | Thread `extraFolders` through `findDocCandidates`, `findHeadingCandidates`, `findTagCandidates`, `findCandidatesForCompl`, `findCandidatesInDoc` |
| `Marksman/CodeActions.fs` | Add `extraFolders` param to `createMissingFile`; use in `Dest.tryResolveSym` to suppress "create file" action when target exists in extra folder |
| `Marksman/Folder.fs` / `Folder.fsi` | Expose `extraFolderRoots`; add `withExtraFolderRoots` (used in tests) |
| `Marksman/Workspace.fsi` | Expose new helpers |
| `Marksman/Refs.fsi` | Update signatures |
| `Tests/default.marksman.toml` | Add commented `extra_folders` example |
| `Tests/ConfigTests.fs` | Parse and merge tests |
| `Tests/WorkspaceTest.fs` | `ExtraFolderTest` module |
| `Tests/DiagTest.fs` | `noBrokenLinkWhenExtraFolderHasDoc` |
| `Tests/RefsTests.fs` | `CrossFolderResolutionTests` module |
| `Tests/StateTests.fs` | `ClientDescriptionTests` (capability parsing); `WatchedFilesStateTests` (reload/delete mutations) |

## Known Limitations

- Paths that do not exist at startup are silently skipped (a warning is written to the LSP
  log). No user-visible diagnostic is produced.
- **Rename must be initiated from the definition, not from a link.** This follows existing
  single-folder behaviour: rename is invoked on the heading or document title, and all
  references update. Placing the cursor on `[[link]]` and renaming is not supported
  (the operation is silently ignored). A future PR could add rename-from-reference for the
  unambiguous case.
- **Rename scope by symbol type.** Cross-folder rename propagation depends on the symbol
  being renamed:

  | What you rename | Cross-folder refs updated? | Notes |
  |---|---|---|
  | Document title (H1 / `Def.Title`) | **Yes** | `[[doc]]` and `[[doc#section]]` refs in all referencing folders are updated |
  | Document itself (`Def.Doc`, no H1) | **Yes** | same as above |
  | Sub-heading (H2+, `Def.Header`) | **Yes** | `[[doc#subtitle]]` refs in referencing folders are updated; only the exact section slug is matched |
  | Link definition (`Def.LinkDef`) | **No** | link defs are local to their folder |

  Implementation: in `findDefRefs`, `Def.Header` now produces `(docSlug, Some sectionSlug)`
  for the cross-folder scan. The scan matches only `CrossSection` refs whose `.Doc` slug
  equals the document slug and whose `.Section` slug equals the heading id, leaving
  `Doc`/`Title` behaviour unchanged.
- **Cross-folder rename propagation is session-dependent.** When rename is initiated from a
  heading in an extra-folder document, marksman propagates the edit to referencing primary
  folders only if both folders were loaded in the *same LSP session*. In practice, if the
  extra folder has its own `.marksman.toml` or `.git` root, most editors (including Neovim
  with the default lspconfig `root_dir` heuristic) will attach a *second* marksman instance
  to that folder. That instance has no knowledge of the primary folder, so rename edits will
  not propagate back. The reliable workaround is to initiate the rename while the cursor is
  in a buffer attached to the primary workspace's LSP session: use go-to-definition to jump
  to the extra-folder file (which stays attached to the primary session), then rename from
  there. If both workspaces declare each other as `extra_folders`, go-to-definition works
  from either side.
- Extra folders are loaded once at startup. Adding or removing an `extra_folders` entry
  requires restarting the LSP server.
- **Transient broken-link diagnostic after a cross-folder rename.** The file watcher syncs
  extra-folder state on disk save, not on buffer edit. After a rename, the referencing
  buffer receives the new link text immediately (via the rename's `workspace/applyEdit`),
  but the renamed document is not re-indexed in the other session until its file is written.
  Between the rename and `:wa`, a spurious broken-link diagnostic may appear in the other
  session. It clears automatically once all buffers are saved — no `LspRestart` needed.

## Test Coverage

Tests are split across several files:

- `Tests/DiagTest.fs` — diagnostic-layer tests (what warnings/errors are produced)
- `Tests/RefsTests.fs` — resolution-layer tests (what destinations are returned)
- `Tests/ComplTests.fs` — completion tests (extra-folder candidates)
- `Tests/CodeActionTests.fs` — code action tests (suppress "create file" for extra-folder docs)

### Diagnostic tests (`DiagTest.fs`)

| Test | What it verifies |
|------|-----------------|
| `noBrokenLinkWhenExtraFolderHasDoc` | Basic extra-folder resolution: link resolves → no diag; without extra folder → diag appears |
| `noBrokenLinkOnWikiCrossRef` | Bidirectional wiki-link cross-refs (by slug) produce no diag in either direction |
| `noBrokenLinkOnMarkdownCrossRef` | Bidirectional markdown cross-refs (bare filename) produce no diag in either direction |
| `brokenLinkDiagWhenExtraFolderLacksTarget` | Valid cross-ref suppresses its diag; genuinely broken link still diagnosed |
| `noBrokenLinkOnWikiCrossRefByH1Title` | `[[Doc B]]` matches H1 title in extra folder even when H2 sub-sections are present; regression for `isTitle` vs `isHeaderOrTitle` bug |
| `noBrokenLinkOnWikiCrossRefToSection` | `[[doc-b#section-one]]` targeting a heading in an extra-folder doc produces no diag |
| `noDiagForBrokenLinkInsideExtraFolderDoc` | Broken links *inside* an extra-folder doc are not diagnosed from the primary session; the extra folder's own session catches them |
| `noTransitiveExtraFolderResolution` | Non-goal: A→B→C chaining is not supported — a link in A that only exists in C is diagnosed as broken even when B declares C as extra |

### Resolution tests (`RefsTests.fs`, `CrossFolderResolutionTests` module)

| Test | What it verifies |
|------|-----------------|
| `crossFolderWikiLink_resolvesToExtraFolder` | Wiki link resolves to a doc in the extra folder; without extra folder returns empty |
| `crossFolderWikiLink_sectionRef` | `[[doc#section]]` resolves the heading in an extra-folder doc |
| `crossFolderNotAmbiguous_primaryTakesPrecedence` | When primary folder has the doc, extra folder is not consulted and result count is unchanged |

### Completion tests (`ComplTests.fs`, `ExtraFolderCompletion` module)

| Test | What it verifies |
|------|-----------------|
| `extraFolderDocAppearsInCompletion` | Typing `[[brett` yields a candidate from an extra-folder doc (`brett-scorza.md`) |
| `extraFolderTagAppearsInCompletion` | Tags defined in extra-folder documents appear in tag completion |

### Code action tests (`CodeActionTests.fs`, `CreateMissingFileTests` module)

| Test | What it verifies |
|------|-----------------|
| `noCreateFileActionWhenDocInExtraFolder` | "Create missing file" action is suppressed when the target doc exists in an extra folder |

### File-watcher tests (`StateTests.fs`)

| Test | What it verifies |
|------|-----------------|
| `supportsDidChangeWatchedFiles_true` | `ClientDescription.SupportsDidChangeWatchedFiles` returns true when client caps include `dynamicRegistration = true` |
| `supportsDidChangeWatchedFiles_false` | Returns false when `dynamicRegistration = false` |
| `supportsDidChangeWatchedFiles_absent` | Returns false when capability is absent |
| `watchedFileChange_updatesDocInExtraFolder` | Reloading a doc via `Folder.withDoc` after a file-change event makes the new content visible through `State.tryFindDoc` |
| `watchedFileDelete_removesDocFromExtraFolder` | Removing a doc via `Folder.withoutDoc` after a file-delete event makes the doc absent from `State.tryFindDoc` |

## Markdown Link Path Constraints for Cross-Folder Resolution

When using markdown-style links (as opposed to wiki-style `[[...]]` links), the path
syntax determines whether cross-folder resolution is attempted:

| Link form | Resolved as | Cross-folder? |
|-----------|-------------|---------------|
| `[text](doc-b.md)` | `Approx` — suffix match on filename | Yes |
| `[text](some-folder/doc-b.md)` | `Approx` — suffix match on path | Yes |
| `[text](../folderB/doc-b.md)` | `ExactRel` — resolved within source folder root | No |
| `[text](/doc-b.md)` | `ExactAbs` — resolved within source folder root | No |

The key rule: any path **without `..` components and without a leading `/`** becomes an
`Approx` lookup, which is suffix-matched against all docs in the extra folder. Paths with
`..` or a leading `/` are resolved as exact paths anchored to the source folder's own root,
so they can never reach into a different folder root.

**Practical guidance:** to link from folder A to `folderB/some-folder/doc-b.md`, write:

```markdown
[text](some-folder/doc-b.md)   ✓ resolves via extra-folder suffix match
[text](doc-b.md)               ✓ also works (less specific, may be ambiguous)
[text](../folderB/doc-b.md)    ✗ evaluated inside folderA's root, never finds folderB's doc
```
