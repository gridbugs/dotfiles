#!/usr/bin/env bash
set -eux
home-manager switch --flake $PWD/$(hostname -s) --show-trace
