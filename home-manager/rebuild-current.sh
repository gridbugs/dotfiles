#!/usr/bin/env bash
set -eux
home-manager switch --flake ./$(hostname -s)
