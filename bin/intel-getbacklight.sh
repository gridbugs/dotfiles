#!/bin/sh

set -euo pipefail

intel_backlight | cut -d' ' -f4 | tr -d %
