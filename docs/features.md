# Features


✅ - done; 🗓 - planned.

- ✅ Document symbols from headings.
- ✅ Workspace symbols from headings.
    * Query matching is subsequence-based, that is `lsp` will match both `LSP` and `Low Seismic Profile`.
- ✅ Completion for links (inline, reference, wiki).
- ✅ Hover prevew for links.
- ✅ "Go to definition" for links.
- ✅ "Find references" for headings and links.
- ✅ Diagnostics for wiki-links.
- ✅ Support multi-folder workspaces.
- ✅ Custom parser for more fine-grained note structure.
- 🗓 Code Lens with "# references" on headings.
- ✅ Rename refactor.
- 🗓 Add support for images (diagnostics, completion, goto).
- 🗓 Add "check" command for standalone workspace checking.
- 🗓 Add "build" command that rewrites all cross-references into proper
  relative markdown links for further embedding into a static site generator
  such as Jekyll or Hakyll.
- 🗓 Support for Jupyter notebooks.

## Configuration

See [Configuration](/docs/configuration.md) docs for more details.

## Wiki links

Alongside regular markdown links, Marksman also supports wiki-style links, e.g. ``[[some-doc]]``
or ``[[#some-heading]]``. This is particularly convenient when working with a Zettelkasten-like repository of markdown
notes, as it streamlines linking and cross-linking of notes. This is what tool like [Obsidian][obsidian]
and [Emanote][emanote] use.

By default Marksman uses a document **title's slug** when referencing a document, however there is an configuration
setting to use a **file name** or a file path instead. This functionality is currently **experimental** and may change
in future depending on user's feedback. See [Configuration](/docs/configuration.md) for more details.

## Code actions

Code actions usually can be enabled/disabled via a configuration option. See
[configuration](#configuration) for more details.

### Table of Contents

Marksman has a code action to create and update a table of contents of a document.

![Table of Contents](/assets/readme/gifs/toc.gif)

## Ignore files

Marksman by default reads ignore globs from `.gitignore`, `.hgignore`, and
`.ignore` and doesn't scan directories matching any of the glob patterns.
Marksman will search for and read ignore files in all sub-folders of the
workspace. similarly to what Git does.

## Workspace folders, project roots, and single-file mode

The LSP specification is designed to work with projects rather than individual
files[^single-file-mode]. Marksman has a custom **single-file mode** that
provides a *subset* of language features for markdown files open outside of any
project. This works well for small one-off edits or when opening random
markdown files. However, when you have several interconnected documents do
consider setting up a project folder for them for an improved experience.

How a folder (aka project, aka root) is found varies between editors, but
usually it's either

1. a root of a VCS repository (applicable to all languages),
2. a folder with `.marksman.toml` marker file (specific to Marksman
   integrations).

When Marksman doesn't provide cross-file language assist for your files and you
don't understand why, you can either:

1. check your project into version control, or
2. create a `.marksman.toml` at the root folder of your project, or
3. refer to your editor/LSP client documentation regarding how a project root
   is defined.


[obsidian]: https://obsidian.md

[emanote]: https://emanote.srid.ca
