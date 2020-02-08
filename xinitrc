#!/bin/bash

if test -f $HOME/.xprofile; then
    source $HOME/.xprofile
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

setxkbmap -option caps:swapescape

dwm_loop() {
    new_csum=$(sha1sum $(which dwm))
    csum=""
    while true; do
        if [ "$csum" != "$new_csum" ]
        then
            csum=$new_csum
            dwm
        else
            exit 0
        fi
        new_csum=$(sha1sum $(which dwm))
        sleep 0.5
    done
}

dwm_wm() {
    if test -f $HOME/.bin/dwm-bar-run-bg.sh; then
        $HOME/.bin/dwm-bar-run-bg.sh
    fi
    dwm_loop
}

case $WM in
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
