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

make_link zshrc .zshrc
make_link zsh-custom .zsh-custom
make_link vimrc .vimrc
make_link vim .vim
make_link i3 .i3
make_link xinitrc .xinitrc
make_link zfunc .zfunc
make_link i3status.conf .i3status.conf
make_link tmux.conf .tmux.conf
make_link wallpaper.png .wallpaper.png
make_link terminator.config .config/terminator/config
make_link termite.config .config/termite/config
make_link fish .config/fish
make_link htoprc .config/htop/htoprc
make_link bin .bin
make_link xmobarrc .xmobarrc
make_link fzf.zsh .fzf.zsh

if test `uname -s` = 'Linux'; then
    make_link urxvt .urxvt
    make_link Xresources .Xresources
    make_link Xresources.d .Xresources.d
    make_link xmonad .xmonad
    make_link Xmodmap .Xmodmap
fi

echo
echo Done!
