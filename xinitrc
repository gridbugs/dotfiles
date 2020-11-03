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

dwm_wm() {
    dwm-bar-run-bg.sh
    dwm
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
