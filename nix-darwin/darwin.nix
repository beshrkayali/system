{ pkgs, self, config, ... }: {

  # General Config
  # --------------
  networking.hostName = "deucalion";
  nix.settings.experimental-features = "nix-command flakes";  # Necessary for using flakes on this system.
  system.configurationRevision = self.rev or self.dirtyRev or null;  # Set Git commit hash for darwin-version.
  system.stateVersion = 6;  # Used for backwards compatibility, please read the changelog before changing. $ darwin-rebuild changelog
  nixpkgs.hostPlatform = "aarch64-darwin";  # The platform the configuration will be used on.

  # Security
  # --------
  security.pam.services.sudo_local.touchIdAuth = true;  # Enable Touch ID for sudo

  # System Packages
  # ---------------
  environment.systemPackages = with pkgs; [
    curl
    git
    ripgrep
    fd
    tree
    wget
    htop
  ];

  nix.gc = {
    automatic = true;
    interval =   {
      Hour = 3;
      Minute = 15;
      Weekday = 7;
    };
    options = "--delete-older-than 7d";
  };

  # Homebrew
  # ---------
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
    };

    brews = [
      "llvm"
      # For clang-format & clang tidy
      # ln -s $(brew --prefix llvm)/bin/clang-format /opt/homebrew/bin/clang-format
      # ln -s $(brew --prefix llvm)/bin/clang-tidy /opt/homebrew/bin/clang-tidy
      "autoconf"
      "autoconf-archive"
      "automake"
      "ccache"
      "cmake"
      "xmake"
      "gdb"
      "p7zip"
      "nasm"
      "ninja"
      "pkg-config"
      "libpq"
      "mosh"
      "tmux"
      "tpm"
      "mg"
      "ag"
      "eza"
      "nmap"
      "dust"
      "delta"
      "fzf"
      "glow"
      "mpv"
      "bandwhich"
      "difftastic"
      "pandoc"
      "zola"
      "go"
      "rust"
      "uv"
      "podman"
      "podman-compose"
      "spaceship"
      "node"
      "pnpm"
      "pre-commit"
      "zsh-syntax-highlighting"
      "zsh-autosuggestions"
      "zsh-autopair"
    ];

    # Mac App Store applications
    masApps = {
      # "App Name" = App Store ID;
      "Xcode" = 497799835;
    };

    # Homebrew casks
    casks = [
      "emacs"
      "orion"
      "kdenlive"
      "waterfox"
      "netnewswire"
      "the-unarchiver"
      "wezterm"
      "ghostty"
      "godot"
      "inkscape"
      "hammerspoon"
      "cyberduck"
      "ollama"
      "visual-studio-code"
      "iina"
      "cap"
      "itsycal"
      "numi"
      "openemu"
      "handbrake"
      "little-snitch"
      "jellyfin-media-player"
      "rectangle"
      "nextcloud"
      "obsidian"
      "squeak"
      "telegram"
    ];
  };

  # Global env vars
  # ---------------
  environment.variables = {
    EDITOR = "mg";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
  };

  # Symlinks
  # --------
  system.activationScripts.extraActivation.text = let
    homeDir = config.users.users.beshr.home;
    repoPath = "${homeDir}/src/system";

    # Define symlinks as source -> target pairs
    symlinks = [
      # Format: [source target]
      # - Nix Darwin
      ["${repoPath}/nix-darwin" "/etc/nix-darwin"]
      # - Emacs
      ["${repoPath}/emacs" "${homeDir}/.emacs.d"]
      # - Other dotfiles
      ["${repoPath}/wezterm.lua" "${homeDir}/.wezterm.lua"]
      ["${repoPath}/zshrc.zsh" "${homeDir}/.zshrc"]
      ["${repoPath}/tmux.conf" "${homeDir}/.tmux.conf"]
      ["${repoPath}/gitconfig" "${homeDir}/.gitconfig"]
      ["${repoPath}/ghostty" "${homeDir}/.config/ghostty/config"]
    ];

    # Function to generate symlink commands
    mkSymlinkCmd = link: let
      source = builtins.elemAt link 0;
      target = builtins.elemAt link 1;
    in ''
      # For ${source} -> ${target}
      if [ -L "${target}" ]; then
        echo "Removing existing symlink at ${target}..."
        rm "${target}"
      elif [ -e "${target}" ]; then
        echo "Backing up existing file at ${target}..."
        mv "${target}" "${target}.backup-$(date +%Y%m%d-%H%M%S)"
      fi
      echo "Creating symlink: ${source} -> ${target}"
      mkdir -p "$(dirname "${target}")"
      ln -sfn "${source}" "${target}"
    '';

  in ''
    echo "Setting up symlinks..."
    ${builtins.concatStringsSep "\n" (map mkSymlinkCmd symlinks)}
    echo "Symlink setup complete!"
  '';

  # Users
  # -----
  users.users.beshr = {
    name = "beshr";
    home = "/Users/beshr";
    shell = pkgs.zsh;
    description = "Beshr Kayali Reinholdsson";
    gid = 20;  # Staff
    isHidden = false;
  };

  # Fonts
  # -----
  fonts.packages = with pkgs; [
    # Individual nerd fonts packages
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code

    # Regular fonts
    fira-code
    jetbrains-mono
    inter
  ];


  # System defaults
  system.defaults = {
    screencapture.location = "~/Pictures/screenshots";
    screencapture.disable-shadow = true;
    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;

    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
    };

    dock = {
      autohide = true;
      orientation = "left";
      show-process-indicators = false;
      show-recents = false;
      static-only = true;
      tilesize = 32;
    };

    finder = {
      AppleShowAllExtensions = true;
      ShowPathbar = true;
      FXEnableExtensionChangeWarning = false;
      FXPreferredViewStyle = "clmv";
      ShowStatusBar = true;
    };
  };
}
