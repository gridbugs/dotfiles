#!/bin/bash

xrdb -remove

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
fi

if hash xsetroot 2>/dev/null; then
    xsetroot -solid black
fi

xrdb -DHOME=$HOME $XRDB_ARGS -merge $HOME/.Xresources

xmodmap $HOME/.Xmodmap

exec xmonad
