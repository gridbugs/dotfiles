#!/usr/bin/env bash
set -eux
DIR="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
home-manager switch --flake ./$(hostname -s)
