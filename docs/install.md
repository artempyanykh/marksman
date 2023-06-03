# Installation options

## Option 1: via a package manager

### Homebrew

Available for MacOS and Linux:

```bash
brew install marksman
```

#### Nix

`marksman` is available [via `nixpkgs`](https://search.nixos.org/packages?query=marksman) for Linux
and MacOS.

For example, on non-NixOS you can run the following to permanently install `marksman` into your local profile:
```
nix-env -iA nixpkgs.marksman
```

### Snap

Available for a number of Linux distros supporting Snap [via Snapcraft](https://snapcraft.io/marksman).

To install the latest stable release:

```
sudo snap install marksman
```

To install the edge release (from main branch):
```
sudo snap install --edge marksman
```

## Option 2: use pre-built binary

1. Go to [Releases](https://github.com/artempyanykh/marksman/releases) page: each release has pre-built binaries for
   Linux, MacOS, and Windows. Download the binary for your OS.
2. Rename the binary and make it executable:
    * MacOS: `mv marksman-macos marksman && chmod +x marksman`
    * Linux: `mv marksman-linux marksman && chmod +x marksman`
    * Windows: rename `marksman-windows.exe` to `marksman.exe`.
3. Place the binary somewhere in your `PATH`.
    * XDG recommends using `$HOME/.local/bin/` (make sure this folder is in your `PATH`).

**NOTE**: If you're on MacOS and are getting a popup about:

> “marksman” can’t be opened because Apple cannot check it for malicious software...

Then you can run the following command to bypass it and let Mac know that it's
fine:

```sh
xattr -d com.apple.quarantine <path-to-marksman-bin>
```

## Option 3: build from source

0. Install [Dotnet SDK](https://dotnet.microsoft.com/en-us/download) for your OS.
1. Clone the repository: `git clone https://github.com/artempyanykh/marksman.git`
2. Inside `marksman` folder run `make install`
3. The binary will be installed under `$HOME/.local/bin` (make sure this folder is in your `PATH`).

