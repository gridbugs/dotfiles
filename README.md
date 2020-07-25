# Refind

Updating macos or reinstalling grub can mess up refind. Details for fixing are
[here](http://www.rodsbooks.com/refind/installing.html#osx). Run the `mountesp`
script in the refind package, and then "bless" refind by running
`sudo bless --mount /Volumes/ESP --setBoot --file /Volumes/ESP/efi/refind/refind_x64.efi --shortform`.

# Unicode

The `noto-fonts` archlinux package includes glyphs for some unicode characters.
It's one of the choices of fonts dependencies for firefox, and isn't the default.
The default choice doesn't include all unicode glyphs used in the dwm-bar.sh script,
so if those glyphs are missing, this package might not be installed.

# Grub

```
# generate config
sudo grub-mkconfig -o /boot/grub/grub.cfg

# install grub to efi partition
sudo grub-install --target=x86_64-efi --efi-directory=/boot/ --bootloader-id=arch_grub
```

# Pip

Using YouCompleteMe in neovim requires `pynvim`.

The kb-light.py script depends on `dbus-python`.

# Git

The git global config file (~/.gitconfig) contains the user's name and email, so isn't portable
for inclusion in this repo, however the following can be copied into the file:

```
[color]
	ui = true
[pull]
	rebase = true
	default = current
[push]
	default = current
[alias]
	ls = log
	ci = commit
	st = status
	co = checkout
```

# Language Servers

## Rust

[rust-analyzer](https://rust-analyzer.github.io/manual.html#installation)

```
rustup component add rust-src
git clone https://github.com/rust-analyzer/rust-analyzer.git && cd rust-analyzer
cargo xtask install
```

## Scala

[metals](https://scalameta.org/metals/docs/editors/vim.html)

```
curl -L -o coursier https://git.io/coursier-cli
chmod +x coursier
./coursier bootstrap \
  --java-opt -Xss4m \
  --java-opt -Xms100m \
  --java-opt -Dmetals.client=vim-lsc \
  org.scalameta:metals_2.12:0.9.0 \
  -r bintray:scalacenter/releases \
  -r sonatype:snapshots \
  -o $HOME/.local/bin/metals-vim -f
```

## Python

[pyls](https://github.com/palantir/python-language-server)

```
pip install python-language-server
```

## Javascript/Typescript

[javascript-typescript-langserver](https://github.com/sourcegraph/javascript-typescript-langserver)

```
npm install --global javascript-typescript-langserver --prefix ~/.local/
```

## C

Install clangd with a package manager

# FreeBSD Notes

## Lenovo Thinkpad 470

### Trackpad

Install `xf86-input-libinput`.

```
# /etc/sysctl.conf

kern.evdev.rcpt_mask="12"
```

```
# /boot/loader.conf

hw.psm.synaptics_support=1
```

```
# /etc/rc.conf

moused_enable="NO"
```

```
# /usr/local/etc/X11/xorg.conf.d/input.conf

Section "InputClass"
    Identifier "libinput keyboard catchall"
    MatchIsKeyboard "on"
    MatchDevicePath "/dev/input/event*"
    Driver "libinput"
    Option "XkbRules" "evdev"
EndSection

Section "InputClass"
    Identifier "libinput touchpad catchall"
    MatchIsTouchpad "on"
    MatchDevicePath "/dev/input/event*"
    Driver "libinput"
    Option "NaturalScrolling" "off"
    Option "Tapping" "off"
    Option "DisableWhileTyping" "on"
    Option "AccelSpeed" "0.42"
EndSection
```

### Graphics

Install `drm-kmod`.

```
# /etc/rc.conf

kld_list="/boot/modules/i915kms.ko"
```

```
# /usr/local/etc/X11/xorg.conf.d/driver-intel.conf
Section "Device"
    Identifier "Card0"
    Driver     "intel"
    Option "DRI" "3"
EndSection
```

### Backlight

Install `intel-backlight`, and run `intel_backlight`.

### Notes

The emojis in dwm-bar.sh work if `noto-extra` is installed. Unfortunately this package is huge.

To compile `st`, the `tic` binary is required. It can be obtained by installing the `ncurses` package.
