#!/bin/sh

USER_PATH=$HOME/.local/bin:$HOME/.bin:$HOME/bin:$PATH

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

dwm_wm() {
    if test -f $HOME/.bin/dwm-bar-run-bg.sh; then
        PATH=$USER_PATH $HOME/.bin/dwm-bar-run-bg.sh
    fi
    PATH=$USER_PATH dwm
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
