# System

This repo holds my system config, which as of [April 2025](https://log.beshr.com/switching-to-macos-again/), is macOS.

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

- All tooling config files are symlinked and not managed by nix-darwin (or home-manager)


## Cheatsheet

- List generations

```
darwin-rebuild --list-generations
```

- Rollback to previous generation

```
darwin-rebuild switch --rollback
```

- Rollback to specific generation

```
sudo nix-env -p /nix/var/nix/profiles/system --switch-generation NUMBER /run/current-system/sw/bin/darwin-rebuild switch
```

- Run garbage collection manually

```
nix-collect-garbage
```

or a more aggressive garbage collection (removes old generations)

```
nix-collect-garbage -d
```

**Rebuild current config after a GC run:**

```
darwin-rebuild switch
```
