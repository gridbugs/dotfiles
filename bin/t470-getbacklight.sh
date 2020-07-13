#!/usr/bin/env bash

set -euo pipefail

dev=/sys/class/backlight/intel_backlight/brightness
cat $dev
