#!/bin/bash

source $HOME/.xdefaults

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
fi

if hash feh 2>/dev/null; then
    feh --bg-fill $HOME/.wallpaper.png
fi

xrdb -remove
xrdb -DENV_HOME=$HOME -DENV_I3_MOD_KEY=$I3_MOD_KEY -merge $HOME/.Xresources

xmodmap $HOME/.Xmodmap

if hash autocutsel 2>/dev/null; then
    autocutsel -fork &
    autocutsel -selection PRIMARY -fork &
fi

if hash synclient 2>/dev/null; then
    synclient HorizTwoFingerScroll=1
fi

exec xmonad
