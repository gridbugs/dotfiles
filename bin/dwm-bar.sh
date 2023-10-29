#!/bin/sh

set -eu

TIME=$(date "+%a %Y-%m-%d %H:%M:%S %Z")

if type setxkbmap >/dev/null 2>/dev/null; then
    KEYMAP=$(setxkbmap -query | grep variant | awk '{print $2}')
    MAYBE_KEYMAP=" $KEYMAP |"
else
    MAYBE_KEYMAP=""
fi

if type list-ips >/dev/null 2>/dev/null; then
    IP=$(list-ips)
    MAYBE_IP=" $IP |"
else
    MAYBE_IP=""
fi

if type battery >/dev/null 2>/dev/null; then
    BATT=$(battery)
    MAYBE_BATT=" ba $BATT |"
else
    MAYBE_BATT=""
fi

if type getbacklight >/dev/null 2>/dev/null; then
    BACKLIGHT=$(printf "%.2f" $(getbacklight))
    MAYBE_BACKLIGHT=" br $BACKLIGHT |"
else
    MAYBE_BACKLIGHT=""
fi

if type amixer >/dev/null 2>/dev/null; then
    VOLUME=$(amixer sget Master 2> /dev/null | tail -n1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    if [ $VOLUME == ""]; then
        MAYBE_VOLUME=""
    else
        MAYBE_VOLUME=" vo $VOLUME |"
    fi
else
    MAYBE_VOLUME=""
fi

if type dwm-bar-times-extra >/dev/null 2>/dev/null; then
    TIMES_EXTRA=$(dwm-bar-times-extra)
    MAYBE_TIMES_EXTRA=" $TIMES_EXTRA |"
else
    MAYBE_TIMES_EXTRA=""
fi

echo "$MAYBE_KEYMAP$MAYBE_IP$MAYBE_BACKLIGHT$MAYBE_BATT$MAYBE_VOLUME$MAYBE_TIMES_EXTRA $TIME"
