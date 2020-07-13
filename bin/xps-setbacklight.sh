#!/usr/bin/env bash

set -euo pipefail

dev=/sys/class/backlight/intel_backlight/brightness
step=$1
val=$(($(<$dev) + $(($step * 100))))
val=$((val >= 0 ? val : 0))
sudo bash -c "echo $val > $dev"
