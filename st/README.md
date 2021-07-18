# st

## Install default

Uses an overlay to build st with some patches and config applied:
```
nix-env -i st
```

## Build with size

Produce a binary named `st24`:
```
nix-env -i -f sized.nix --arg pixelsize 24
```
