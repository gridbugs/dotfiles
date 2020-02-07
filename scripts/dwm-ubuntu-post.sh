#!/bin/bash
set -euxo pipefail
apt-get install dwm
cp /usr/share/xsessions/dwm.desktop{,.bak}
apt-get purge dwm
cp /usr/share/xsessions/dwm.desktop{.bak,}
