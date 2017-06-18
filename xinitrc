#!/bin/bash
xrdb -remove
xrdb -DENV_HOME=$HOME -DENV_I3_MOD_KEY=Mod1 -merge $HOME/.Xresources
xmodmap -e "keycode 94 = grave asciitilde"

exec i3
