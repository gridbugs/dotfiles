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

make_link vimrc .vimrc
make_link nvim .config/nvim
make_link feh .config/feh
make_link dune-config .config/dune/config
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
make_link direnvrc .direnvrc
make_link gitignore .gitignore
make_link Xdefaults .Xdefaults
make_link npmrc .npmrc
make_link screenrc .screenrc
make_link emacs.d/init.el .emacs.d/init.el
make_link emacs.d/cram-mode.el .emacs.d/cram-mode.el
make_link emacs.d/flycheck-okra.el .emacs.d/flycheck-okra.el
make_link wezterm.lua .wezterm.lua
make_link nix.conf .config/nix/nix.conf
make_link mutt .mutt
make_link muttrc .muttrc
make_link mailcap .mailcap
make_link urlscan .config/urlscan
make_link gitconfig .gitconfig

if [ "$(uname)" == "Darwin" ]; then
    make_link mailcap.macos .mailcap
else
    make_link mailcap .mailcap
fi

echo Done!
