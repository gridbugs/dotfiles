#!/usr/bin/env bash

set -euo pipefail

if test -f $HOME/.xprofile; then
    . $HOME/.xprofile
fi

if hash xsetroot 2>/dev/null; then
    xsetroot -solid black
fi

if hash synclient 2>/dev/null; then
    synclient HorizTwoFingerScroll=1
fi

if hash unclutter 2>/dev/null; then
    unclutter &
fi

if hash setxkbmap 2>/dev/null; then
    setxkbmap dvorak -option caps:swapescape
fi

if hash xrdb 2>/dev/null; then
    xrdb -load $HOME/.Xresources
fi

dwm_loop() {
    new_csum=$(shasum $(which dwm))
    csum=""
    while true; do
        if [ "$csum" != "$new_csum" ]
        then
            csum=$new_csum
            dwm
        else
            exit 0
        fi
        new_csum=$(shasum $(which dwm))
        sleep 0.5
    done
}

dwm_wm() {
    if test -f $HOME/.bin/dwm-bar-run-bg.sh; then
        $HOME/.bin/dwm-bar-run-bg.sh
    fi
    dwm_loop
}

case ${WM:-DWM} in
    dwm)
        dwm_wm
        ;;
    lxde)
        exec startlxde
        ;;
    xfce4)
        exec startxfce4
        ;;
    *)
        dwm_wm
esac
