#!/bin/bash

set -e

sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

git clone https://github.com/stevebob/dotfiles.git $HOME/.dotfiles

if [ -e $HOME/.zshrc ]; then
    mv -v $HOME/.zshrc{,.old}
fi

sh $HOME/.dotfiles/install.sh
