#!/bin/sh

set -eu

get() {
    acpiconf -i $1 | grep 'Remaining capacity' | cut -f2
}

echo "$(get 0) $(get 1)"
