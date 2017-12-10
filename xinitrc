#!/bin/bash

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
else
    export I3_MOD_KEY=Mod4
fi

feh --bg-fill $HOME/.wallpaper.png

xrdb -remove
xrdb -DENV_HOME=$HOME -DENV_I3_MOD_KEY=$I3_MOD_KEY -merge $HOME/.Xresources

xmodmap -e "keycode 94 = grave asciitilde"
xmodmap $HOME/.Xmodmap

if hash synclient 2>/dev/null; then
    synclient HorizTwoFingerScroll=1
fi

exec i3
