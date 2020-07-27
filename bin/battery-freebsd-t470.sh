#!/bin/sh

set -euo pipefail

get() {
    acpiconf -i $1 | grep 'Remaining capacity' | cut -f2
}

echo "$(get 0) $(get 1)"
