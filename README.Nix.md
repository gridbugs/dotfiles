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

## NixOS

### Graphical login to xsession

```
  services.xserver = {
    enable = true;
    autorun = true;
    layout = "us";
    libinput.enable = true;
    displayManager.startx.enable = false;
    displayManager.lightdm.enable = true;
    displayManager.lightdm.greeters.gtk.enable = true;
    displayManager.defaultSession = "xsession";
    displayManager.session = [
      {
         manage = "desktop";
         name = "xsession";
         start = ''exec $HOME/.xsession'';
      }
    ];
  };
```

## Hacks

### Disabling `dev.i915.perf_stream_paranoid`

Some graphical programs produce a warning:
```
MESA: warning: Performance support disabled, consider sysctl dev.i915.perf_stream_paranoid=0
```

Running the command `sysctl dev.i915.perf_stream_paranoid=0` fixes the problem until the next reboot.

Documentation would suggest that it this can be disabled permanently by adding the following to configuration.nix:
```
  boot.kernel.sysctl = {
    "dev.i915.perf_stream_paranoid" = 0;
  };
```
...but it doesn't seem to have any effect.

My workaround is to run the command (as root) in an X initialilsation script.

### Node Version Manager

Binaries installed with `nvm` don't play nice with the dynamic linker. Building from source produes compatible binaries:
```
nvm install -s stable
```

It probably won't compile from a typical environment, but something like the following will work:
```
nix-shell '<nixpkgs>' -A nodejs --command 'nvm install -s stable'
```

### Python Language Server

The `python-language-server` package in nixpkgs doesn't seem to work with LanguageClientNeovim.
The tool you get when you install the pip package named `python-language-server` is different
and tends to work, but there doesn't seem to be a way to use pip effectively on nixos.

Miniconda can be set up using the nix expression described [here](http://www.jaakkoluttinen.fi/blog/conda-on-nixos/).
Installing `python-language-server` with conda into the base environment.
If vim is opened from this environment, it will be able to start pyls and the IDE will work.