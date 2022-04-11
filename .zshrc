export LANG=en_US.UTF-8

# Path things
export PATH=$HOME/.local/bin:$PATH
# export PATH="$(yarn global bin):$PATH"

# Path to your oh-my-zsh installation.
os=$(uname)
if [ "$os" = "Darwin" ]; then
    export ZSH="/Users/gcperrin/.oh-my-zsh"
else
    export ZSH="/home/gcperrin/.oh-my-zsh"
fi
    

export EDITOR=emacs
export GOPATH=$HOME/go
# export GOROOT="$(brew --prefix golang)/libexec"
# export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"

# Load custom theme
export ZSH_THEME=gcp
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=24"
source $ZSH/oh-my-zsh.sh

# ENABLE_CORRECTION="true"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
VIM_MODE_VICMD_KEY='^D'
VIM_MODE_TRACK_KEYMAP=no
#source ~/.oh-my-zsh/custom/plugins/zsh-vim-mode/zsh-vim-mode.plugin.zsh
plugins=(git zsh-syntax-highlighting docker docker-compose)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# iTerm tab title naming
# export DISABLE_AUTO_TITLE=true
function title {
    echo -ne "\033]0;"$*"\007"
}

# OS-specific action
if [ "$os" = "Darwin" ]; then
    alias lsl="ls -l -G"
    alias ls="ls -la -G"
    alias lf="ls -la -G"
    alias emacs="Emacs --no-window-system"
else
    alias lsl="ls -l"
    alias l="ls -la"
    alias ls="ls -la"
    alias lf="ls -la" # For weird KDE aliases?
		alias em="emacs --no-window-system"
    alias emacs="emacs --no-window-system"
fi


# Git aliases
alias gs="git status"
alias gl="git log"
alias gls="git log --pretty=oneline"
alias gld="git log -1 --format=\%ad"
alias gd="git diff HEAD "
alias gtl="git tag -l -n5"

# General aliases
alias cl="clear"
alias rzsh="source ~/.zshrc"

# Docker aliases
alias dk="docker"
alias dki="docker image"
alias dkc="docker container"
alias dkv="docker volume"

# Tmux aliases
alias tmuxn="tmux new-session -s"

# Tree aliases
alias t2="tree -L 2"
alias t3="tree -L 3"
alias t4="tree -L 4"

# LHR
alias mono="./monolith"

# Color stuff
ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# Show all 256 colors with color number
function spectrum_ls() {
    for code in {000..255}; do
        print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
    done
}

# Fuzzy finder
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ZSH autosuggest
source $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^ ' autosuggest-accept

# Docker autocomplete
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i
### Fix for making Docker plugin work
autoload -U compinit && compinit
###
ctags=/usr/local/bin/ctags

# Syntax highlighting
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[builtin]='fg=#ff875f,light'
ZSH_HIGHLIGHT_STYLES[function]='fg=#ff875f,light'
ZSH_HIGHLIGHT_STYLES[command]='fg=#ff875f,light'
ZSH_HIGHLIGHT_STYLES[alias]='fg=#ff5f5f,light'
ZSH_HIGHLIGHT_STYLES[path]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#ff0087,light'

# source $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Emscripten path mapping
alias emload="source \"/Users/gcperrin/.emsdk/emsdk_env.sh\""

# Emscripten for linux
# source /home/gcperrin/.local/lib/emsdk/emsdk_env.sh

# Oh-my-zsh and FZF loading
#source $ZSH/oh-my-zsh.sh
#source $ZSH/key-bindings.zsh
#source $ZSH/completion.zsh 

# The next line updates PATH for the Google Cloud SDK.
#if [ -f '/Users/gcperrin/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/gcperrin/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
# if [ -f '/Users/gcperrin/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/gcperrin/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# Rust shell init
# Kubernetes
source <(kubectl completion zsh)
echo "[[ $commands[kubectl] ]] && source <(kubectl completion zsh)" >> ~/.zshrc # add autocomplete permanently to your zsh shell

# Solana
export PATH="/home/gcperrin/.local/share/solana/install/active_release/bin:$PATH"

# Rust
source $HOME/.cargo/env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
[[ /opt/homebrew/bin/kubectl ]] && source <(kubectl completion zsh)
[[ /opt/homebrew/bin/kubectl ]] && source <(kubectl completion zsh)
