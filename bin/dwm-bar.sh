#!/bin/bash

TIME=$(date "+%Y-%m-%d %H:%M:%S")
IP=$(for i in $(ip route); do echo $i; done | grep -A 1 src | tail -n1)
MAYBE_IP="🖧 $IP |"

if hash acpi 2>/dev/null; then
    BATT=$(acpi -b | awk '{ printf "%s%s;", $4, $5}')
    MAYBE_BATT="🗲 $BATT |"
else
    MAYBE_BATT=""
fi

if hash xbacklight 2>/dev/null; then
    BACKLIGHT=$(printf "%.2f" $(xbacklight -get))
    MAYBE_BACKLIGHT="☼ $BACKLIGHT |"
else
    MAYBE_BACKLIGHT=""
fi

if hash amixer 2>/dev/null; then
    VOLUME=$(amixer sget Master | tail -n1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    MAYBE_VOLUME="🔊 $VOLUME |"
else
    MAYBE_VOLUME=""
fi

echo "$MAYBE_IP $MAYBE_BACKLIGHT $MAYBE_BATT$MAYBE_VOLUME $TIME"
