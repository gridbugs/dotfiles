# Nix Notes

## Install Nix

```
curl -L https://nixos.org/nix/install | sh
```

## Update Packages

```
nix-channel --update
nix-env -u
```

## Common Operations

### List installed programs

```
nix-env -q
```

### Start a shell to test a package without installing it
```
nix-shell -p <package>
```
