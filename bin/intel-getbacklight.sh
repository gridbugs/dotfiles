#!/bin/sh

set -eu

intel_backlight | cut -d' ' -f4 | tr -d %
