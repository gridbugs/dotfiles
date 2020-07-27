#!/bin/sh

set -euo pipefail

dev=/sys/class/backlight/intel_backlight/brightness
cat $dev
