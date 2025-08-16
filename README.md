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

- Install Nix using [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer?tab=readme-ov-file#determinate-nix-installer):

```bash
curl -fsSL https://install.determinate.systems/nix | sh -s -- install --determinate
```

- Clone this repo into `~/src/system` and cd to it

- Install nix-darwin

```
sudo nix run nix-darwin/master#darwin-rebuild -- switch
```

- Use nix-darwin

```
darwin-rebuild switch --flake .#deucalion
```

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

- Run garbage collection manually

```
nix-collect-garbage
```

or a more aggressive garbage collection (removes old generations)

```
sudo nix-collect-garbage -d
```

- Clean up leftover generations older than 7 days with

```
nix-collect-garbage --delete-older-than 1d
```


**Rebuild current config after a GC run:**

```
darwin-rebuild switch
```
