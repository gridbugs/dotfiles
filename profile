export PATH="$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:/usr/games:/usr/local/games:$PATH"

# Source extra commands from .profile_extra
[ -f ~/.profile_extra ] && . ~/.profile_extra

if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
elif [ -d "$HOME/.cargo/bin" ]; then
    # fallback for systems that don't have a ~/.cargo/env but on which rust is still installed (e.g. nixos)
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Set some rust-specific environment variables if rust is installed
if type rustc 2>/dev/null >/dev/null && [ -d ~/.cargo ]; then
    export RUST_SRC_PATH=$(rustc --print sysroot)"/lib/rustlib/src/rust/src"
    export CARGO_HOME=$HOME/.cargo
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
[ -d "$HOME/.rvm/bin" ] && export PATH="$PATH:$HOME/.rvm/bin"

# Add NPM bin path to PATH
if [ -d "$HOME/.npm-packages" ]; then
    NPM_PACKAGES=$HOME/.npm-packages
    export PATH="$NPM_PACKAGES/bin:$PATH"
fi

# Set up homebrew env if available
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ -f /opt/homebrew/bin/brew ]; then
    export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
fi

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . ~/.bashrc
    fi
elif [ "$SHELL" == "/bin/ksh" ]; then
    if [ -f "$HOME/.kshrc" ]; then
        export ENV=$HOME/.kshrc
    fi
fi

# opam configuration
test -r $HOME/.opam/opam-init/init.sh && . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export PATH="$HOME/bin:$PATH"
