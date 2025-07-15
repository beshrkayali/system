#######################
# ZSH CONFIGURATION   #
#######################

# ----- Environment Variables -----
# Default paths and configurations
typeset -U path  # Prevent duplicates in PATH


# ----- Shell Options -----
# Improve command line behavior
setopt AUTO_LIST           # List choices on ambiguous completion
setopt CORRECT             # Spelling correction for commands
setopt NO_BEEP             # No beep on error (add if desired)


# ----- History Configuration -----
# Manage command history settings
HISTSIZE=10000             # Number of commands to store in memory
SAVEHIST=10000             # Number of commands to save to disk
HISTFILE=~/.zsh_history    # Where to store command history

setopt SHARE_HISTORY       # Share history between sessions
setopt HIST_IGNORE_ALL_DUPS # Don't store duplicated commands
setopt HIST_SAVE_NO_DUPS   # Don't write duplicates to history file
setopt HIST_REDUCE_BLANKS  # Remove unnecessary blanks from history


# ----- Directory Navigation -----
# Improve directory movement
setopt AUTO_CD             # Type directory name to cd into it
setopt AUTO_PUSHD          # Make cd push the old directory onto the stack
setopt PUSHD_IGNORE_DUPS   # Don't store duplicates in the stack


# ----- Completion System -----
# Configure tab completion behavior
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit

# Completion styling and behavior
zstyle ':completion:*' menu select                              # Menu-style completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'      # Case-insensitive completion
zstyle ':completion:*' special-dirs true                        # Complete special directories
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colorize completions
zstyle ':completion:*' use-cache on                             # Use caching for completion
zstyle ':completion:*' cache-path ~/.zsh/cache                  # Cache location
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'


# ----- Word Separation -----
# Define what constitutes a word for keyboard shortcuts
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'


# ----- Utility Functions -----
# Archive extraction function
# Usage: extract archive.tar.gz
extract() {
  if [ -f $1 ]; then
    case $1 in
      *.tar.bz2)  tar -jxvf $1 ;;
      *.tar.gz)   tar -zxvf $1 ;;
      *.bz2)      bunzip2 $1 ;;
      *.dmg)      hdiutil mount $1 ;;
      *.gz)       gunzip $1 ;;
      *.tar)      tar -xvf $1 ;;
      *.tbz2)     tar -jxvf $1 ;;
      *.tgz)      tar -zxvf $1 ;;
      *.zip)      unzip $1 ;;
      *.Z)        uncompress $1 ;;
      *)          echo "'$1' cannot be extracted" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# File compression function
# Usage: compress file/dir archive.tar.gz
compress() {
  if [ -e "$1" ]; then
    case "$2" in
      *.tar.gz|*.tgz)   tar -czvf "$2" "$1" ;;
      *.tar.bz2|*.tbz2) tar -cjvf "$2" "$1" ;;
      *.tar)            tar -cvf "$2" "$1" ;;
      *.zip)            zip -r "$2" "$1" ;;
      *.gz)             gzip -c "$1" > "$2" ;;
      *.bz2)            bzip2 -c "$1" > "$2" ;;
      *)                echo "'$2' is not a supported format" ;;
    esac

    if [ $? -eq 0 ]; then
      echo "'$1' has been compressed as '$2'"
    else
      echo "Compression failed"
    fi
  else
    echo "'$1' is not a valid file or directory"
  fi
}

# Create and enter directory in one command
# Usage: mkcd new_directory
mkcd() {
  mkdir -p "$1" && cd "$1"
}

# Display directory size in human-readable format
# Usage: dsize directory
dsize() {
  du -sh "$1"
}

# Find files by name pattern
# Usage: ff filename_pattern
ff() {
  find . -type f -name "*$1*" -print
}

# Show PATH in a readable line-by-line format
path() {
  echo $PATH | tr ':' '\n'
}


# ----- Aliases -----
# File operations
alias ls='eza'             # Modern ls replacement

# macOS Finder operations
alias showfiles="defaults write com.apple.finder AppleShowAllFiles YES; killall Finder"
alias hidefiles="defaults write com.apple.finder AppleShowAllFiles NO; killall Finder"


# ----- Additions to PATH ------
path+=('/Applications/Godot.app/Contents/MacOS/Godot')

# ---- Global env vars


# ----- macOS Specific Settings -----
# System preferences for better macOS experience
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true  # No .DS_Store on network drives


# ----- Plugin Configuration -----
# Load Homebrew-installed ZSH plugins
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOMEBREW_PREFIX/share/zsh-autopair/autopair.zsh
source $HOMEBREW_PREFIX/opt/spaceship/spaceship.zsh
