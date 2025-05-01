# System

This repo holds my system config, which as of March 2025, is a macOS.

It uses:

- [nix-darwin](https://github.com/nix-darwin/nix-darwin)
- Emacs
- Others:
  - Homebrew
  - WezTerm
  - Zsh
  - Tmux

## Installation

- Install [Homebrew](https://brew.sh)

- Install Nix using [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer?tab=readme-ov-file#determinate-nix-installer), using the recommended vanilla upstream Nix (answer No when prompted to install `Determinate Nix`)

- Clone this repo into `~/src/system`

- Run `darwin-rebuild switch --flake ~/src/system/nix-darwin`

> This will create a symlink `/etc/nix-darwin -> ~/src/system/nix-darwin`, so it's possible to run `darwin-rebuild switch` the next time.
