#!/usr/bin/env bash

function backup_dotfile()
{
    dotfile="$1"
    echo "> Backing up existing $dotfile"
    current_date=$(date +"%Y-%m-%d")
    mv $dotfile $dotfile"_"$current_date.bak
}

echo "> Creating dotfile symlinks"
dotfiles=(.bashrc .gitconfig .gitignore .oh-my-zsh .profile .pylintrc .zshrc)
dotfiles_dir="$(cd "$(dirname "$0")" && pwd)"

for df in ${dotfiles[@]}; do
    [ -f "$HOME/$df" ] && [ ! -L "$HOME/$df" ] && backup_dotfile "$HOME/$df"
    [ -L "$HOME/$df" ] && rm "$HOME/$df"
    ln -s "$dotfiles_dir/$df" "$HOME/$df"
done
