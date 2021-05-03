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

[pyls](https://github.com/palantir/python-language-server)

```
pip install python-language-server
```

### Typescript

[typescript-language-server](https://github.com/theia-ide/typescript-language-server)

```
npm install --global typescript-language-server --prefix ~/.local/
```

### C

Install clangd with a package manager
