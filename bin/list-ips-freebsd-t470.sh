#!/usr/bin/env bash

set -euo pipefail

function get() {
    ifconfig $1 | awk '/inet / { printf " %s",$2 }'
}

echo "$(get em0)$(get wlan0)"
