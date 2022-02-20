#!/bin/sh

setxkbmap -option caps:swapescape en_US

if test -f $HOME/.xprofile; then
    . $HOME/.xprofile
fi

if type xsetroot >/dev/null 2>/dev/null; then
    xsetroot -solid black
fi

if type synclient >/dev/null 2>/dev/null; then
    synclient HorizTwoFingerScroll=1
fi

if type unclutter >/dev/null 2>/dev/null; then
    unclutter &
fi

if test -f $HOME/.Xresources; then
    xrdb -merge ~/.Xresources
fi

if type fvwm >/dev/null 2>/dev/null; then
    exec fvwm
elif type dwm >/dev/null 2>/dev/null; then
    dwm-bar-run-bg.sh
    exec dwm
fi
