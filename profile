# Only execute this file once per shell.
if [ -n "$__USER_PROFILE_SOURCED" ]; then return; fi
export __USER_PROFILE_SOURCED=1

export PATH="$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:/usr/games:/usr/local/games:$PATH"


# Set up homebrew env if available
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ -f /opt/homebrew/bin/brew ]; then
    export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
fi

# Set up homebrew library path
if type brew 2>/dev/null >/dev/null; then
    export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
fi

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

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . ~/.bashrc
    fi
elif [ "$SHELL" = "/bin/ksh" ]; then
    if [ -f "$HOME/.kshrc" ]; then
        export ENV=$HOME/.kshrc
    fi
fi

# In some systems SHELL is shell variable by default. Force it to be an environment variable.
export SHELL

export PATH="$HOME/bin:$PATH"

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/s/.opam/opam-init/init.sh' && . '/home/s/.opam/opam-init/init.sh' > /dev/null 2> /dev/null || true
# END opam configuration

# BEGIN configuration from Dune installer
# This configuration must be placed after any opam configuration in your shell config file.
# This performs several tasks to configure your shell for Dune:
#   - makes sure the dune executable is available in your $PATH
#   - registers shell completions for dune if completions are available for your shell
#   - removes opam's pre-command hook because it would override Dune's shell configuration
source $HOME/.dune/share/dune/env/env.bash
__dune_env $HOME/.dune
PROMPT_COMMAND="$(echo "$PROMPT_COMMAND" | tr ';' '\n' | grep -v _opam_env_hook | paste -sd ';' -)" # remove opam's pre-command hook
# END configuration from Dune installer
