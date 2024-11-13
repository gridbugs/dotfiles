#!/bin/sh
tmp=$(mktemp -d)
cp $1 $tmp
open $tmp/$(basename $1) 
