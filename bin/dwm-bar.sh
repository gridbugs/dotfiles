#!/usr/bin/env bash

set -euo pipefail

TIME=$(date "+%a %Y-%m-%d %H:%M:%S %Z")

if hash ip 2>/dev/null; then
    IP=$(for i in $(ip route | grep -v 'default via' | grep -E 'dev (wl|en).*'); do echo $i; done | grep -A 1 src | tail -n1)
    MAYBE_IP="🖧 $IP |"
else
    MAYBE_IP=""
fi

if hash acpi 2>/dev/null; then
    BATT=$(acpi -b | awk '{ printf "%s%s;", $4, $5}')
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
