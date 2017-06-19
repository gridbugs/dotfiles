#!/bin/bash

if test -f $HOME/.i3_custom; then
    source $HOME/.i3_custom
else
    I3_MOD_KEY=Mod4
fi

xrdb -remove
xrdb -DENV_HOME=$HOME -DENV_I3_MOD_KEY=$I3_MOD_KEY -merge $HOME/.Xresources
xmodmap -e "keycode 94 = grave asciitilde"

exec i3
