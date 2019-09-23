#!/bin/bash
set -euxo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TARGET=$HOME

function make_link {
    DEST=$TARGET/$2
    if test -e $DEST; then
        echo $DEST already exists
    else
        mkdir -v -p $(dirname $DEST)
        ln -v -s $DIR/$1 $DEST
    fi
}

cd $HOME

make_link nvim .config/nvim
if [ ! -e $HOME/.config/nvim/Vundle.vim ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.config/nvim/Vundle.vim
fi

if [ ! -e $HOME/.fzf ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git $HOME/.fzf
fi

make_link bashrc .bashrc
make_link inputrc .inputrc
make_link xinitrc .xinitrc
make_link tmux.conf .tmux.conf
make_link htoprc .config/htop/htoprc
make_link bin .bin
make_link fzf.zsh .fzf.zsh
make_link dwm pkg/dwm
make_link st pkg/st

echo Done!
