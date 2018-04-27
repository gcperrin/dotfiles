# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Load custom theme
source $ZSH/oh-my-zsh.sh
source ~/.dotfiles/gcp.zsh-theme

export TERM="xterm-256color"
export EDITOR="emacs"

# NVM (Node JS) activation
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

export DISABLE_AUTO_TITLE=true

# Golang path
export GOPATH="$HOME/.go"
export PATH=$HOME/.go/bin:/usr/local/bin:$PATH

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=24"
