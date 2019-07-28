#!/bin/bash

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
fi

if hash xsetroot 2>/dev/null; then
    xsetroot -solid black
fi

setxkbmap -option caps:swapescape

case $WM in
    dwm)
        $HOME/.bin/dwm-loop.sh
        ;;
    xmonad)
        exec xmonad
        ;;
    lxde)
        exec startlxde
        ;;
    *)
        exec dwm
esac
