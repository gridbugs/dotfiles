# Nix Notes

## Install Nix

```
curl -L https://nixos.org/nix/install | sh
```

## Handy packages
```
nix-env -i nix-bash-completions   # bash completions for nix commands
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

### Install a standalone package located at PATH/default.nix
```
nix-env -f PATH -i
```

E.g. [Shopify/comma](https://github.com/Shopify/comma)

## Quirks

### Broken Locales

Some programs will complain about a broken locale in a fresh install.
E.g.
```
$ cowsay hi
perl: warning: Setting locale failed.
perl: warning: Please check that your locale settings:
        LANGUAGE = (unset),
        LC_ALL = (unset),
        LANG = "en_AU.UTF8"
    are supported and installed on your system.
perl: warning: Falling back to the standard locale ("C").
 ____
< hi >
 ----
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

$ higan
hiro: Gtk::Locale not supported by C library.
        Using the fallback 'C' locale.
```

The fix is to install glibc-locales:
```
nix-env -iA nixpkgs.glibcLocales
```
...and set the `LOCALE_ARCHIVE` environment variable:
```
export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
```

### gpg-connect-agent: failed to create temporary file

When switching to a root shell with `su`:

```
gpg-connect-agent: failed to create temporary file '/root/.gnupg/.#lk0x0000000000cfeeb0.iconofsin.1419': No such file or directory
gpg-connect-agent: can't connect to the agent: No such file or directory
gpg-connect-agent: error sending standard options: No agent running

```

The fix is to create a directory `/root/.gnupg`.

Relevant issue: [https://github.com/NixOS/nixpkgs/issues/29331](https://github.com/NixOS/nixpkgs/issues/29331)
