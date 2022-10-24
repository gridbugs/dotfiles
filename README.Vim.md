# VIM Setup

## Language Servers

### Rust

[rust-analyzer](https://rust-analyzer.github.io/manual.html#installation)

```
rustup component add rust-src
git clone https://github.com/rust-analyzer/rust-analyzer.git && cd rust-analyzer
cargo xtask install
```

### Scala

[metals](https://scalameta.org/metals/docs/editors/vim.html)

Install metals (the language scala server):
```
curl -L -o ~/bin/coursier https://git.io/coursier-cli
chmod +x ~/bin/coursier
coursier install metals --install-dir ~/bin
```

Make a script at ~/bin/metals-vim that invokes metals with custom `JAVA_OPTS`. E.g.:
```bash
#!/bin/sh

JAVA_OPTS=-Dmetals.bloop-port=8123 metals
```
Make it executable:
```
chmod a+x ~/bin/metals-vim
```

### Python

[python-lsp-server](https://github.com/python-lsp/python-lsp-server)

```
pip install python-lsp-server
```

...or on nixos:

```
nix-env -iA nixpkgs.python3Packages.python-lsp-server
```

### Typescript

[typescript-language-server](https://github.com/theia-ide/typescript-language-server)

```
npm install --global typescript-language-server --prefix ~/.local/
```

### C

Install clangd with a package manager

### Ocaml

```
nix-env -iA nixpkgs.ocamlPackages.ocaml-lsp
nix-env -iA nixpkgs.ocamlformat
```

## Issues

### Missing `neovim` python package

An error like this one is displayed when starting vim:
```
[ncm2_core@yarp] Job is dead. cmd=['python3', '-u', '/Users/steve/.vim/plugged/nvim-yarp/pythonx/yarp.py', '/var/folders/q4/fv2jln151qlcxw9hkvm0b6jr0000gq/T/nvimWn9v1U/0', 2, 'ncm2_core']
```

Output of `:messages`:
```
[ncm2_core@yarp] Traceback (most recent call last):
[ncm2_core@yarp]   File "/Users/steve/.vim/plugged/nvim-yarp/pythonx/yarp.py", line 2, in <module>
[ncm2_core@yarp]     from pynvim import attach, setup_logging
[ncm2_core@yarp] ModuleNotFoundError: No module named 'pynvim'
[ncm2_core@yarp] During handling of the above exception, another exception occurred:
[ncm2_core@yarp] Traceback (most recent call last):
[ncm2_core@yarp]   File "/Users/steve/.vim/plugged/nvim-yarp/pythonx/yarp.py", line 4, in <module>
[ncm2_core@yarp]     from neovim import attach, setup_logging
[ncm2_core@yarp] ModuleNotFoundError: No module named 'neovim'
[ncm2_core@yarp] Job is dead. cmd=['python3', '-u', '/Users/steve/.vim/plugged/nvim-yarp/pythonx/yarp.py', '/var/folders/q4/fv2jln151qlcxw9hkvm0b6jr0000gq/T/nvimWn9v1U/0', 2, 'ncm2_core']
```

The fix is to install the `neovim` python package. E.g.
```
pip3 install --user neovim
```
