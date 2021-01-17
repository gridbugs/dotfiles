#!/bin/sh
set -eux

DIR="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
TARGET=$HOME

make_link() {
    DEST="$TARGET/$2"
    if test -e "$DEST"; then
        echo "$DEST already exists"
    else
        mkdir -p "$(dirname "$DEST")"
        ln -s "$DIR/$1" "$DEST"
    fi
}

cd "$HOME"

make_link nvim .vim
make_link nvim .config/nvim
make_link feh .config/feh
make_link nvim/init.vim .vimrc
make_link nvim/init.vim .ideavimrc
make_link bashrc .bashrc
make_link profile .profile
make_link inputrc .inputrc
make_link xinitrc .xinitrc
make_link tmux.conf .tmux.conf
make_link bin .bin
make_link hushlogin .hushlogin
make_link git-prompt.sh .git-prompt.sh
make_link ledgerrc .ledgerrc

# link archlinux-specific packages
if type pacman 2>/dev/null >/dev/null; then
    make_link dwm pkg/dwm
    make_link st pkg/st
fi

if [ $(uname) == OpenBSD ]; then
    make_link xsession .xsession
fi

echo Done!
