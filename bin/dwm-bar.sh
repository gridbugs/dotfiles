#!/bin/sh

set -eu

TIME=$(date "+%a %Y-%m-%d %H:%M:%S %Z")

if type list-ips >/dev/null 2>/dev/null; then
    IP=$(list-ips)
    MAYBE_IP="ip$IP |"
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
    VOLUME=$(amixer sget Master | tail -n1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    MAYBE_VOLUME=" vo $VOLUME |"
else
    MAYBE_VOLUME=""
fi

echo "$MAYBE_IP$MAYBE_BACKLIGHT$MAYBE_BATT$MAYBE_VOLUME $TIME"
