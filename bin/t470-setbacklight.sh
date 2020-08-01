#!/bin/sh

set -eu

dev=/sys/class/backlight/intel_backlight/brightness
step=$1
val=$(($(<$dev) + $step))
val=$((val >= 0 ? val : 0))
sudo bash -c "echo $val > $dev"
