#!/bin/sh

# Source the shell config file. This operation is supposed to be idempotent.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
elif [ "$SHELL" = "/bin/ksh" ]; then
    if [ -f "$HOME/.kshrc" ]; then
        export ENV="$HOME/.kshrc"
    fi
fi

# Only execute the remainder of this file once per shell session. There are
# several different ways that .profile gets run across various systems and use
# cases. Sourcing .profile isn't usually idempotent so this line protects the
# environment from having .profile's effects applied multiple times.
#
# Note that if X is started with xinitrc/xsession then the entire graphical
# session takes place inside an environment where .profile has been sourced.
# New terminal windows will not execute login shells.
if [ -n "$__USER_PROFILE_SOURCED" ]; then return; fi
export __USER_PROFILE_SOURCED=1

if ! (test -f /etc/issue && grep -i nixos /etc/issue > /dev/null); then
    # These paths seem to be missing from the default environment on some
    # systems that install programs to these paths nonetheless.

    __prepend_path() {
        if test -d "$1"; then
            case :"$PATH": in
                *:"$1":*)
                  ;;
                *)
                  export PATH="$1:$PATH"
                  ;;
            esac
        fi
    }

    __prepend_path "/usr/games"
    __prepend_path "/usr/local/games"
fi

export PATH="$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$PATH"

# Set up homebrew env if available
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ -f /opt/homebrew/bin/brew ]; then
    export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
fi

# Set up homebrew library path
if type brew 2>/dev/null >/dev/null; then
    LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
    export LIBRARY_PATH
fi

# Source extra commands from .profile_extra
[ -f "$HOME/.profile_extra" ] && . "$HOME/.profile_extra"

# Add the cargo bin dir to PATH if it exists
if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
elif [ -d "$HOME/.cargo/bin" ]; then
    # fallback for systems that don't have a ~/.cargo/env but on which rust is still installed (e.g. nixos)
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Set some rust-specific environment variables if rust is installed
if type rustc 2>/dev/null >/dev/null && [ -d "$HOME/.cargo" ]; then
    RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
    CARGO_HOME="$HOME/.cargo"
    export RUST_SRC_PATH
    export CARGO_HOME
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
[ -d "$HOME/.rvm/bin" ] && export PATH="$PATH:$HOME/.rvm/bin"

# Add NPM bin path to PATH
if [ -d "$HOME/.npm-packages" ]; then
    NPM_PACKAGES=$HOME/.npm-packages
    export PATH="$NPM_PACKAGES/bin:$PATH"
fi

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
if type opam 2>/dev/null >/dev/null; then
    if test -r "$HOME/.opam/opam-init/init.sh"; then
        . "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null
    fi
fi
# END opam configuration

# BEGIN configuration from Dune installer
# This configuration must be placed after any opam configuration in your shell config file.
# This performs several tasks to configure your shell for Dune:
#   - makes sure the dune executable is available in your $PATH
#   - registers shell completions for dune if completions are available for your shell
#   - removes opam's pre-command hook because it would override Dune's shell configuration
if [ -f "$HOME/.dune/share/dune/env/env.bash" ]; then
    . "$HOME/.dune/share/dune/env/env.bash"
    __dune_env "$HOME/.dune"
    PROMPT_COMMAND="$(echo "$PROMPT_COMMAND" | tr ';' '\n' | grep -v _opam_env_hook | paste -sd ';' -)" # remove opam's pre-command hook
fi
# END configuration from Dune installer

# Make sure that my bin directory is the first entry in PATH
export PATH="$HOME/bin:$PATH"

# In some systems SHELL is shell variable by default. Force it to be an environment variable.
export SHELL
