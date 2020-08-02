#!/bin/sh
curl --silent --max-time 2 ipinfo.io/org || echo "no isp"
true
