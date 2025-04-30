# History configuration
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS

# Better directory navigation
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

# Completion system
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

# Extract some archive formats
# $ extract package.tar.gz
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

# Compress files or folders into some archive formats
# $ compress file/dir package.tar.gz
compress() {
  if [ -e "$1" ]; then
    case "$2" in
      *.tar.gz|*.tgz)  tar -czvf "$2" "$1" ;;
      *.tar.bz2|*.tbz2) tar -cjvf "$2" "$1" ;;
      *.tar)      tar -cvf "$2" "$1" ;;
      *.zip)      zip -r "$2" "$1" ;;
      *.gz)       gzip -c "$1" > "$2" ;;
      *.bz2)      bzip2 -c "$1" > "$2" ;;
      *)          echo "'$2' is not a supported format" ;;
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

# Better word separation
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>' 

# Aliases
alias ls='eza'
alias showfiles="defaults write com.apple.finder AppleShowAllFiles YES; killall Finder"
alias hidefiles="defaults write com.apple.finder AppleShowAllFiles NO; killall Finder"

# Source homebrew installed zsh plugins
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOMEBREW_PREFIX/share/zsh-autopair/autopair.zsh
source $HOMEBREW_PREFIX/opt/spaceship/spaceship.zsh

