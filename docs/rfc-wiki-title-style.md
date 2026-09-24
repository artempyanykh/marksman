# RFC: Wiki Completion `title` Style

## Problem

Some Markdown editors (notably Obsidian) prefer wiki links that preserve the document title text,
for example `[[My Note Title]]`, instead of slugified output like `[[my-note-title]]`.

## Goal

Add a new completion style `completion.wiki.style = "title"` so generated wiki links can
use the raw title text and remain Obsidian-compatible.

## Configuration

```toml
[completion]
wiki.style = "title"
```

## Behavior

Available styles:

- `title-slug` (default): slugified title
- `title`: raw title text
- `file-stem`: filename without extension
- `file-path-stem`: relative path without extension

When `title` is selected, completion emits `[[<Doc title as written>]]`.

## Compatibility

- Existing style values remain unchanged.
- Link resolution behavior is preserved; this RFC only adds a new output style option.
