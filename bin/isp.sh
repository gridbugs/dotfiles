#!/bin/sh
curl --silent --max-time 1 ipinfo.io/org || echo "no isp"
true
