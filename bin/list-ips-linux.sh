#!/bin/sh

set -euo pipefail

for i in $(ip route | grep -v 'default via' | grep -E 'dev (wl|en).*'); do echo " $i"; done | grep -A 1 src | tail -n1
