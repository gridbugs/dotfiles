#!/bin/bash

set -e

if ! hash stack 2> /dev/null; then
    echo "Install stack first!"
    exit 1
fi

if ! test -d xmonad-git; then
    git clone "https://github.com/xmonad/xmonad" xmonad-git
    git -C xmonad-git checkout 'v0.13'
fi
if ! test -d xmonad-contrib-git; then
    git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
    git -C xmonad-contrib-git checkout 'v0.13'
fi
if ! test -d xmobar-git; then
    git clone "https://github.com/jaor/xmobar" xmobar-git
    git -C xmobar-git checkout '0.26'
fi

if ! test -e stack.yaml; then
    stack init
fi

./update-yaml.py stack.yaml

stack install
