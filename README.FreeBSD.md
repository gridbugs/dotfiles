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
hw.psm.trackpoint_support="1"
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
    Option "AccelSpeed" "0.2"
EndSection
```

### Graphics

Install `drm-kmod` and `xf86-video-intel`.

```
# /etc/rc.conf

kld_list="/boot/modules/i915kms.ko"
```

```
# /usr/local/etc/X11/xorg.conf.d/driver-intel.conf
Section "Device"
    Identifier "Card0"
    Driver     "intel"
    Option     "DRI" "3"
EndSection
```

### Backlight

Install `intel-backlight`, and run `intel_backlight`.

The backlight keys don't show up in X, and instead are acpi events, so we can't handle them from dwm like usual.
Instead, add the following to `/etc/devd.conf`:

```
# /etc/devd.conf

notify 10 {
  match "system"    "ACPI";
  match "subsystem" "IBM";
  action "/usr/local/bin/ibm-handle.sh $notify";
};
```
...and put this shell script at `/usr/local/bin/ibm-handle.sh`:
```bash
#!/usr/bin/env bash

set -euo pipefail

# To learn the correspondence between keys and numbers, run `devd -d`
# and watch the output as you press keys.

case $1 in
    0x11)
        intel_backlight decr
        ;;
    0x10)
        intel_backlight incr
        ;;
    *)
        ;;
esac
```

We need to tell the kernel to load the `acpi_ibm` module when it starts. Add the following to /boot/loader.conf:
```
# /boot/loader.conf

acpi_ibm_load="YES"
```

### Suspend/Resume

Amazingly this just worked for me:

```
# /etc/sysctl.conf

hw.acpi.lid_switch_state=S3
```

### Notes

The emojis in dwm-bar.sh work if `noto-extra` is installed. Unfortunately this package is huge.

To compile `st`, the `tic` binary is required. It can be obtained by installing the `ncurses` package.

Run `sysctl dev.acpi_ibm` to list some variables related to the thinkpad hardware.

When mounting partitions, you must specify the driver to use. E.g.

```
$ mount -v -t msdosfs /dev/da0s1 /mnt/
$ mount -t ext2fs /dev/nvd0p2 /mnt/
```

To work with ext filesystems, install `e2fsprogs`.

To work with ntfs file systems, install `fusefs-ntfs`.

To get the `pkg-config` command, install `pkgconf`.

When using `rvm` to install rubies, the binary distribution doesn't seem dynamically link correctly. Building from source seems to fix the issue:

```
$ rvm install 2.7 --disable-binary
```

When using `nvm` to install nodes, it will want to build from source, but needs to be explicitly told what the C and C++ compilers are:
```
$ CC=clang CXX=clang++ nvm install node
```

To have /tmp use tmpfs, add this line to /etf/fstab:
```
tmpfs /tmp tmpfs rw,mode=777 0 0
```

Some pages showing example thinkpad configs which I found helpful:
- https://www.c0ffee.net/blog/freebsd-on-a-laptop/
- https://unrelenting.technology/articles/freebsd-on-the-thinkpad-x240
- https://www.davidschlachter.com/misc/t480-freebsd
