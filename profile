export PATH="$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:/usr/games:/usr/local/games:$PATH"

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

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . ~/.bashrc
    fi
fi

# Start nix environment if present
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh
  export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
fi

# >>> conda initialize >>>
#
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('~/.conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "~/.conda/etc/profile.d/conda.sh" ]; then
        . "~/.conda/etc/profile.d/conda.sh"
    else
        export PATH="~/.conda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Use neovim, vim, or vi as editor
if type nvim 2>/dev/null >/dev/null; then
    export EDITOR=nvim
    export VISUAL=nvim
elif type vim 2>/dev/null >/dev/null; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=vi
    export VISUAL=vi
fi
