#!/usr/bin/env bash

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

is_osx() {
    [[ $OSTYPE =~ [Dd]arwin ]]
}

has_homebrew() {
    [[ -f /usr/local/bin/brew ]]
}

source_if_exists() {
    [[ -f $1 ]] && source $1
}

# colors for OSX
di=ex   # Directory
ln=dx   # Symbolic Link
so=dx   # Socket
pi=hx   # Pipe
ex=cx   # Executable
bd=hx   # Block (buffered) special file
cf=hx   # Character (unbuffered) special file
eu=hx   # Executable with setuid bit set
eg=hx   # Executable with setgid bit set
ds=hx   # Director writable to others, with sticky bit
dw=hx   # Director writable to others, without sticky bit
export LSCOLORS="$di$ln$so$pi$ex$bd$cf$eu$eg$ds$dw"

# path
userbin=$HOME/bin
if is_osx && has_homebrew; then
    homebrew=/usr/local/bin:/usr/local/sbin:~/Applications
    coreutils="$(brew --prefix coreutils)/libexec/gnubin"
    gnused="$(brew --prefix gnu-sed)/libexec/gnubin"
    node=/usr/local/share/npm/bin
    PATH=$homebrew::$coreutils:$gnused:$node:$PATH
fi
export PATH=$userbin:$PATH

# rbenv setup
which rbenv &>/dev/null && eval "$(rbenv init - $ZSH_NAME)"
# jenv setup
which jenv &>/dev/null && eval "$(jenv init -)"
# docker-machine setup
# TODO put this bit in an launchctl plist
which docker-machine &>/dev/null && { [[ $(docker-machine status default) = Running ]] || docker-machine start default; }
which docker-machine &>/dev/null && eval "$(docker-machine env default)"

export EDITOR=vim
export SVN_EDITOR=vim
export JRUBY_OPTS=--debug
export ARKRC=.ackrc
export PYENV_ROOT=/usr/local/opt/pyenv
#export PGSSLMODE=require

# if running bash
if [ -n "$BASH_VERSION" ]; then
    source_if_exists "$HOME/Dropbox/scripts/setup/.bashrc"
    source_if_exists "$HOME/.bashlocal"  # run machine specific bash preferences
fi
source_if_exists "$HOME/.aliases"      # Aliases
source_if_exists "$HOME/.dbext"        # Parameters for the dbext vim plugin
source_if_exists "$HOME/.aws"          # AWS config
source_if_exists "$HOME/.github"       # Github tokens
source_if_exists "$HOME/.profilelocal" # Local shell preferences
