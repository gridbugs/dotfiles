# Only execute this file once per shell.
if [ -n "$__USER_PROFILE_SOURCED" ]; then return; fi
export __USER_PROFILE_SOURCED=1

export PATH="$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:/usr/games:/usr/local/games:$PATH"

# Source extra commands from .profile_extra
[ -f ~/.profile_extra ] && . ~/.profile_extra

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$HOME/.conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/.conda/etc/profile.d/conda.sh" ]; then
        . "$HOME/.conda/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/.conda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# opam configuration
if type opam 2>/dev/null >/dev/null; then
    eval $(opam env --revert)
    OPAMROOT=${OPAMROOT:-$HOME/.opam}
    test -r $OPAMROOT/opam-init/init.sh && . $OPAMROOT/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

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

export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels

export PATH="$HOME/bin:$PATH"
