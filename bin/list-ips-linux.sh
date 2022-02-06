#!/bin/sh

set -eu

ip route | grep -v 'default via' | grep -E 'dev (wl|en).*' | cut -d' ' -f3,9 | xargs echo
