#!/bin/sh
set -eu

DIR="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
TARGET=$PWD

make_link() {
    DEST="$TARGET/$2"
    if test -e "$DEST"; then
        echo "$DEST already exists"
    else
        mkdir -p "$(dirname "$DEST")"
        ln -sf "$DIR/$1" "$DEST"
    fi
}

cd "$HOME"

make_link nvim/autoload .vim/autoload
make_link nvim/plugins.vim .vim/plugins.vim
make_link nvim/catppuccin-lightline.vim .vim/catppuccin-lightline.vim
make_link nvim .config/nvim
make_link feh .config/feh
make_link alacritty.yml .config/alacritty/alacritty.yml
make_link nvim/init.vim .vimrc
make_link nvim/init.vim .ideavimrc
make_link bashrc .bashrc
make_link kshrc .kshrc
make_link profile .profile
make_link inputrc .inputrc
make_link xinitrc .xinitrc
make_link xsession .xsession
make_link tmux.conf .tmux.conf
make_link tmate.conf .tmate.conf
make_link fonts.conf .fonts.conf
make_link ctags .ctags
make_link bin .bin
make_link hushlogin .hushlogin
make_link git-prompt.sh .git-prompt.sh
make_link ledgerrc .ledgerrc
make_link completions .completions
make_link config.nix .config/nixpkgs/config.nix
make_link dwm .config/nixpkgs/overlays/dwm
make_link st .config/nixpkgs/overlays/st
make_link terminus-font .config/nixpkgs/overlays/terminus-font
make_link direnvrc .direnvrc
make_link gitignore .gitignore
make_link Xdefaults .Xdefaults
make_link npmrc .npmrc
make_link screenrc .screenrc
make_link nix.conf .config/nix/nix.conf
make_link emacs.d/init.el .emacs.d/init.el

# link archlinux-specific packages
if type pacman 2>/dev/null >/dev/null; then
    make_link dwm pkg/dwm
    make_link st pkg/st
fi

echo Done!
