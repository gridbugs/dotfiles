#!/bin/sh

set -eu

TIME=$(date "+%a %Y-%m-%d %H:%M:%S %Z")

if hash list-ips 2>/dev/null; then
    IP=$(list-ips)
    MAYBE_IP="🖧$IP |"
else
    MAYBE_IP=""
fi

if hash battery 2>/dev/null; then
    BATT=$(battery)
    MAYBE_BATT=" 🗲 $BATT |"
else
    MAYBE_BATT=""
fi

if hash getbacklight 2>/dev/null; then
    BACKLIGHT=$(printf "%.2f" $(getbacklight))
    MAYBE_BACKLIGHT=" ☼ $BACKLIGHT |"
else
    MAYBE_BACKLIGHT=""
fi

if hash amixer 2>/dev/null; then
    VOLUME=$(amixer sget Master | tail -n1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    MAYBE_VOLUME=" 🔊 $VOLUME |"
else
    MAYBE_VOLUME=""
fi

echo "$MAYBE_IP$MAYBE_BACKLIGHT$MAYBE_BATT$MAYBE_VOLUME $TIME"
