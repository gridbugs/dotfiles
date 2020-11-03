if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . ~/.bashrc
    fi
fi

export PATH="$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$PATH"

# Use neovim, vim, or vi as editor
if type nvim 2>/dev/null >/dev/null; then
    export EDITOR=nvim
    export VISUAL=nvim
    alias vim=nvim
elif type vim 2>/dev/null >/dev/null; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=vi
    export VISUAL=vi
    alias vim=vi
fi

export PATH="$HOME/.cargo/bin:$PATH"

# Set some rust-specific environment variables if rust is installed
if type rustc 2>/dev/null >/dev/null && [[ -d ~/.cargo ]]; then
    export RUST_SRC_PATH=$(rustc --print sysroot)"/lib/rustlib/src/rust/src"
    export CARGO_HOME=$HOME/.cargo
fi

# Set some rust-specific environment variables if rust is installed
if type rustc 2>/dev/null >/dev/null && [[ -d ~/.cargo ]]; then
    export RUST_SRC_PATH=$(rustc --print sysroot)"/lib/rustlib/src/rust/src"
    export CARGO_HOME=$HOME/.cargo
fi

if type rustc 2>/dev/null >/dev/null && [[ -d ~/.cargo ]]; then
    RUST_COMPLETION1=$(rustc --print sysroot)/etc/bash_completion.d/cargo
    RUST_COMPLETION2=$(rustc --print sysroot)/share/bash-completion/completions/cargo
    if [[ -f $RUST_COMPLETION1 ]]; then
        . $RUST_COMPLETION1
    elif [[ -f $RUST_COMPLETION2 ]]; then
        . $RUST_COMPLETION2
    fi
fi

# try to load bash completion from its default location on some systems
[[ $PS1 && -f /usr/local/share/bash-completion/bash_completion.sh ]] && \
    source /usr/local/share/bash-completion/bash_completion.sh
if [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]]; then
    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
    . "/usr/local/etc/profile.d/bash_completion.sh"
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
