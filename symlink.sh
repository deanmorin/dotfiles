#!/usr/bin/env bash
dotfile_dir="$(cd "$(dirname "$0")" && pwd)"


get_dotfiles() {
    local dotfiles=$(
        find $dotfile_dir -maxdepth 1 -type f -name ".*" \
                -and -not -iname '.DS_Store' \
                -and -not -iname '.dropbox' \
                -and -not -iname '*.swp' \
                -exec basename {} \;
    )
    dotfiles="$dotfiles .oh-my-zsh"
    echo $dotfiles
}


backup_dotfile() {
    dotfile="$1"
    echo "> Backing up existing $dotfile"
    current_date=$(date +"%Y-%m-%d")
    mv $dotfile $dotfile"_"$current_date.bak
}


echo "> Creating dotfile symlinks"
dotfiles=$(get_dotfiles)

for df in ${dotfiles[@]}; do
    [[ -e "$HOME/$df" ]] && [[ ! -L "$HOME/$df" ]] && backup_dotfile "$HOME/$df"
    { [[ -L "$HOME/$df" ]] && rm "$HOME/$df"; } || echo $df
    ln -s "$dotfile_dir/$df" "$HOME/$df"
done

ln -s "$dotfile_dir/init.el" "$HOME/.emacs.d/"
