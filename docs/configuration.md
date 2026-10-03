# Configuration

Marksman supports user-level and project-level configuration:
1. User-level configuration is read from:
   * `$HOME/.config/marksman/config.toml` on Linux,
   * `$HOME/Library/Application Support/marksman/config.toml` on macOS,
   * `$HOME\\AppData\\Roaming\\marksman\\config.toml` on Windows.
2. Project-level configuration is read from `.marksman.toml` located in the project's root folder.

For each configuration option the precedence is: project config > user config > global default.

[This config file](../Tests/default.marksman.toml) shows all configuration options with their
default values. You need to specify ONLY the options you wish to override in your user- or
project-config.

## Citation completion
List BibTeX files in `.marksman.toml` to enable citation completion:

```toml
[completion]
bibliography = ["references.bib", "literature/other.bib"]
```

Paths may be absolute or relative to the workspace root. In single-file mode,
relative paths use the Markdown file's directory. These rules also apply to
user-level configuration. The `~` shorthand is not expanded.

Completion is disabled by default. Set `bibliography = []` in the project
configuration to disable it even if the user-level configuration lists files.

Completion supports `@key`, `[@key]`, and `[@first; @second]`, including lists
split across lines. It replaces only the current key, preserving locators and
surrounding text. Matching ignores case and uses subsequence matching. A
matching Markdown reference definition takes precedence.

Duplicate keys appear once. Missing or unreadable files are logged and skipped.
Saved bibliography changes appear on the next completion request.

Bibliography paths in YAML front matter and braced citation keys (`@{key}`) are
not supported.
