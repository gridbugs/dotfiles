#!/bin/sh

set -eu

while true; do
    xsetroot -name "$($HOME/.bin/dwm-bar.sh)"
    sleep 10
    if ! xset q >/dev/null 2>/dev/null; then
        exit
    fi
done &
