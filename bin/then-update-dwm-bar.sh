#!/bin/sh
set -eu
$@
xsetroot -name "$($HOME/.bin/dwm-bar.sh)"

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi
