#!/bin/sh
set -e

read -p "Are you sure? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    set -x
    docker container prune --force
    docker image prune --force --all
    docker system prune -a
fi
