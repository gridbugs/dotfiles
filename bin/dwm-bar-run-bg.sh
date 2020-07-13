#!/usr/bin/env bash

set -euo pipefail

while true; do
    xsetroot -name "$($HOME/.bin/dwm-bar.sh)"
    sleep 10
done &
