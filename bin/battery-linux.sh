#!/bin/sh

set -eu

acpi -b | awk '{ printf "%s%s;", $4, $5}'
