# dwm

## Install default

Uses an overlay to build st with some patches and config applied:
```
nix-env -i dwm
```

## Build with size

Rebuild dwm with a specific fond size:
```
nix-env -i -f common.nix --arg pixelsize 24
```
