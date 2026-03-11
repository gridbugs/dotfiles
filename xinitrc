#!/bin/sh

if test -f $HOME/.xinitrc_extra; then
    . $HOME/.xinitrc_extra
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

if type dwm >/dev/null 2>/dev/null; then
    setxkbmap -option altwin:swap_alt_win
    dwm-bar-run-bg.sh
    exec dwm
elif type fluxbox >/dev/null 2>/dev/null; then
    exec startfluxbox
elif type fvwm >/dev/null 2>/dev/null; then
    exec fvwm
fi
