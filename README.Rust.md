# Rust Notes

## Using llvm's linker

To have cargo link with llvm's linker (faster than gnu's linker), put this in ~/.cargo/config.toml:
```
[build]
rustflags = ["-Clink-arg=-fuse-ld=lld"]
```

On nixos, install the package `llvmPackages.bintools`.
