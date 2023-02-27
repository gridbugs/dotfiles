# RAID Notes

## Create a new Linux RAID array in RAID5 configuration with 3 disks

```
mdadm --create --verbose --level=5 --metadata=1.2 --raid-devices=3 /dev/md/storage_raid5 /dev/sda1 /dev/sdb1 /dev/sdc1
mdadm --detail --scan >> /etc/mdadm.conf
mdadm --assemble --scan
```
Note that updating the config (`/etc/mdadm.conf`) is different in nixos.
Instead of appending to the file directly, set the variable `boot.initrd.mdadmConf` in `configuration.nix`.
See example below.

The block device file `/dev/md/storage_raid5` will be created to represent the array.
This will probably be a symlink to `/dev/md127` or similar.

The block device file behaves like a partition.
Format it:
```
mkfs.ext4 -v -L myarray -b 4096 -E stride=128,stripe-width=384 /dev/md/storage_raid5
```
Mount it:
```
mount /dev/md/storage_raid5 /storage
```

Add an entry to /etc/fstab:
```
# RAID array
/dev/md/storage_raid5    /storage    ext4 defaults 0 0
```

Or on nixos, update `configuration.nix`:
```
fileSystems."/storage" =
  { device = "/dev/md/storage_raid5";
    fsType = "ext4";
  };
```

## Stopping

```
umount /storage
mdadm --stop /dev/md/storage_raid5
```

## Starting
```
mdadm --assemble --scan
mount /dev/md/storage_raid5 /storage/
```

## Example NixOS Config
```
  boot.initrd.mdadmConf = ''
    DEVICE partitions
    ARRAY /dev/md/storage_raid5 metadata=1.2 spares=1 name=storage_raid5 UUID=3a13b2bf:c81767ca:202925c2:c00d020f
  '';
```

## Migration

As long as a machine has the appropriate mdadm config, plugging in all the drives and running:
```
mdadm --assemble --scan
```
...should be sufficient for the array's block devices to be created.

## Re-adding a drive that was unplugged or replcaed on the fly

```
mdadm --manage /dev/md127 --add /dev/sda1
```

## Resources
- [[ArchWiki] RAID](https://wiki.archlinux.org/title/RAID)
- [[John Mercier] switching to nixos](http://johnmercier.com/blog/2017/06-19-switching-to-nixos-again.html)
