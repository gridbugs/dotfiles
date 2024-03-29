# Monitor Backlight

The "bin" dir (symlinked to ~/.bin) contains scripts for getting and setting the backlight.
These have names such as "t470-getbacklight" or "t470-setbacklight". These scripts are specific
to models of laptop.
The dwm config in this repo runs "setbacklight" to set the backlight.
The dwm bar script runs "getbacklight" to get the current backlight brightness.

# Keyboard Backlight

The kb-light.py script sets the keyboard backlight on supported laptops.
On archlinux, this script depends on `python-dbus` and `upower`.

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
