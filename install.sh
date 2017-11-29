#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function make_link {
    if test -e $2; then
        echo $2 already exists
    else
        ln -v -s $DIR/$1 $2
    fi
}

cd $HOME

sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

if [ -e $HOME/.zshrc ]; then
    mv -v $HOME/.zshrc{,.old}
fi

make_link zshrc .zshrc
make_link zsh-custom .zsh-custom
make_link vimrc .vimrc
make_link vim .vim
make_link vimperatorrc .vimperatorrc
make_link i3 .i3
make_link xinitrc .xinitrc
make_link zfunc .zfunc
make_link fonts .fonts

mkdir -v -p $HOME/.config
ln -v -s $DIR/fontconfig $HOME/.config/fontconfig

if test `uname -s` = 'Linux'; then
    make_link urxvt .urxvt
    make_link Xresources .Xresources
    make_link Xresources.d .Xresources.d
fi
