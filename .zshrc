# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="dean"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Change this value to set how frequently ZSH updates¬
export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Need to add before plugins so that virtualenvwrapper can be found in the path
# on Snow Leopard
source ~/.profile

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew brew-cask bundler docker gem git pyenv rbenv)

# zsh-completions (homebrew)
fpath=(/usr/local/share/zsh-completions $fpath)

# turn off autocorrection
DISABLE_CORRECTION="true"

source $ZSH/oh-my-zsh.sh

alias s='source ~/.zshrc'
# TODO find a better solution
alias ls='/bin/ls -G'
alias mv='mv -i'
alias cp='cp -i'
alias -g airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport'
alias pg_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'

# Ignore duplicate history entries (but still store them for record-keeping)
setopt HIST_FIND_NO_DUPS
# Save the time and how long a command ran
setopt EXTENDED_HISTORY
# Pretty   obvious,  right?
setopt HIST_REDUCE_BLANKS

# ignore binary files in vim autocompletion
zstyle ':completion:*:*:vi(m|):*' ignored-patterns '*.beam|*.class|*.o|*.pyc'

source $(brew --prefix)/etc/profile.d/z.sh
