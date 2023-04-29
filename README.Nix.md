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

### Single User Linux
```
nix-channel --update
nix-env -iA nixpkgs.nix nixpkgs.cacert
nix-env -u
```

### Multi User Linux
(as root)
```
nix-channel --update
nix-env -iA nixpkgs.nix nixpkgs.cacert
systemctl daemon-reload
systemctl restart nix-daemon
```

### MacOS
```
sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
```

## System Upgrade

To upgrade to 21.11, say:
```
sudo nix-channel --remove nixos
sudo nix-channel --add https://nixos.org/channels/nixos-21.11 nixos
sudo nix-channel --update
sudo nixos-rebuild switch
```

## Common Operations

### Flake equivalent of `nix-shell -p` that also lets you set the nixpkgs version

This is handly when trying to be compatible with packages installed with home-manager or
system-wide installed packages that were installed with flakes, and the revision of nixpkgs
is known. Supposing the current revision of nixpkgs in the `flake.lock` file is
`f00994e78cd39e6fc966f0c4103f908e63284780`, then you could create a transient shell with some
specified packages by running:

```
nix shell nixpkgs/f00994e78cd39e6fc966f0c4103f908e63284780#hello nixpkgs/f00994e78cd39e6fc966f0c4103f908e63284780#cowsay
```

### Install Home Manager on macOS

```
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
```

### Add the nixpkgs-unstable channel

```
nix-channel --add https://nixos.org/channels/nixpkgs-unstable
```

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

### Clean up unused packages

Remove old profile generations:
```
nix-env --delete-generations old
```

Remove unused packages:
```
nix-store --gc
```

A shorthand for running both of the above:
```
nix-collect-garbage -d
```

Run it as root to clean up old system generations.

### Find out store entry is alive

E.g.
```
nix-store --query --roots /nix/store/h5sn0iaqhjwbp9l2hqmsykb4rp08vcnm-bsnes-hd-10.6-beta/
```

### Delete a store entry unless it is alive

E.g.
```
nix-store --delete /nix/store/h5sn0iaqhjwbp9l2hqmsykb4rp08vcnm-bsnes-hd-10.6-beta
```

### Build a package in sandbox

This is one of the tests required by the PR template for nixpkgs.

E.g. (from a checkout of nixpkgs)
```
nix-build . -A bsnes-hd --option sandbox true
```

### Allow non-free packages:
In /etc/nixos/configuration.nix:
```
nixpkgs.config.allowUnfree = true;
```

### Debug Failing Builds

Keep the build dir by passing `--keep-failed` to `nix-build` or `nix build`.

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

### Patching Binaries

Use the `autoPatchElfHook` package. E.g.

```
$ nix-shell -p alsaLib libudev autoPatchelfHook   # alsaLib and libudev are dependencies of the binary in question
$ autoPatchelf orbital-decay-terminal
automatically fixing dependencies for ELF files
searching for dependencies of orbital-decay-terminal
  libasound.so.2 -> found: /nix/store/rz5pn3gg57wracmqjb756zzhwg4zcn60-alsa-lib-1.2.5.1/lib/libasound.so.2
  libudev.so.1 -> found: /nix/store/27855idyr8dkmh0xrzg7jln7a3fa7viy-systemd-249.4/lib/libudev.so.1
setting RPATH to: /nix/store/rz5pn3gg57wracmqjb756zzhwg4zcn60-alsa-lib-1.2.5.1/lib:/nix/store/27855idyr8dkmh0xrzg7jln7a3fa7viy-systemd-249.4/lib
```

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

## Workflows

### [nix-direnv](https://github.com/nix-community/nix-direnv)

#### Install
```
nix-env -iA nixpkgs.nix-direnv nixpkgs.direnv
```

#### Add to project
```
$ echo "use nix" >> .envrc
$ direnv allow
```


### Python with conda

#### Setup

```
# Install conda-shell
$ nix-env -i conda-shell

# Start conda-shell (ignoring the warning)
$ conda-shell
/etc/profile: line 26: /home/steve/.conda/etc/profile.d/conda.sh: No such file or directory

# Install conda in the current user's home directory
$ conda-install

# This adds some stuff to the shell rc file (don't run this if the shell rc file is set up already)
$ conda init

# Set auto_stack so project environments automatically stack onto the base environment.
# This is so dev tools can be installed in the base environment.
$ conda config --set auto_stack 1

# Install language server and neovim support in base environment
$ conda install -c conda-forge python-language-server neovim
```

#### Usage

```
# Start conda-shell
$ conda-shell

# Activate an environment <foo>
$ conda activate <foo>
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
