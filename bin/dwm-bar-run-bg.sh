#!/bin/bash
while true; do
    xsetroot -name "$($HOME/.bin/dwm-bar.sh)"
    sleep 10
done &
