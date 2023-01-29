#!/bin/sh
set -eu
$@
xsetroot -name "$($HOME/.bin/dwm-bar.sh)"
