#!/bin/sh

set -eu

dev=/sys/class/backlight/intel_backlight/brightness
cat $dev
