#!/bin/bash

if test -f $HOME/.xdefaults; then
    source $HOME/.xdefaults
fi

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
fi

if hash xsetroot 2>/dev/null; then
    xsetroot -solid blcak
fi

xrdb -remove
xrdb -DENV_HOME=$HOME -DENV_I3_MOD_KEY=$I3_MOD_KEY -merge $HOME/.Xresources

xmodmap $HOME/.Xmodmap

exec xmonad
