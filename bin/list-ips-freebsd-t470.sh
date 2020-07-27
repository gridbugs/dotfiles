#!/bin/sh

set -euo pipefail

get() {
    ifconfig $1 | awk '/inet / { printf " %s",$2 }'
}

echo "$(get em0)$(get wlan0)"
