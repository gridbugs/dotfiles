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

### Install a Package

```
nix-env -i <package>
```

### List installed programs

```
nix-env -q
```

### Start a shell to test a package without installing it
```
nix-shell -p <package>
```

### Repl

To get a repl with packages in scope (useful for debugging while developing a package, say):
```
nix repl '<nixpkgs>'
```

### Build a package from a nixpkgs checkout
```
nix-build -A <nixpkgs> -K
```

### Install a package from a nixpkgs checkout
```
nix-env -f . -i <package>
```
