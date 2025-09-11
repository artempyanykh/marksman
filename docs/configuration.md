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
