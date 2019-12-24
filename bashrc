#!/bin/bash

# Only run if the shell is interactive
if [[ $- == *i* ]]; then

    export PATH="$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/.cargo/bin:$HOME:$PATH"

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

    # Allow aliases in sudo
    alias sudo='sudo '

    # Some handy aliases
    alias tmp='pushd $(mktemp -d)'
    alias rebash='source $HOME/.bashrc'

    # Archlinux-specific pacman helpers
    if type pacman 2>/dev/null >/dev/null; then
        alias explicit-non-default-packages='comm -23 <(pacman -Qqe | sort) <(pacman -Qqg base base-devel | sort)'
        alias orphaned-packages='pacman -Qdtq'
    fi

    # Set colours for `ls` to use. If gnu-ls is installed, prefer it over ls.
    export LS_COLORS='di=1;34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=1;30;42:ow=1;30;43'
    if type gls 2>/dev/null >/dev/null; then
        alias ls='gls --color --human-readable --group-directories-first'
    elif ls --color 2>/dev/null >/dev/null; then
        alias ls='ls --color --human-readable --group-directories-first'
    else
        # this case will be hit on macos and bsd without gls installed
        alias ls='ls -h'
    fi

    # Start keychain if it is installed
    if type keychain 2>/dev/null >/dev/null && [[ "$SHELL" == "/bin/bash" ]] && [[ -e ~/.ssh/id_rsa ]]; then
       eval $(keychain --quiet --agents ssh id_rsa --eval)
    fi

    # Set some rust-specific environment variables if rust is installed
    if type rustc 2>/dev/null >/dev/null; then
        export RUST_SRC_PATH=$(rustc --print sysroot)"/lib/rustlib/src/rust/src"
        export CARGO_HOME=$HOME/.cargo
    fi

    # FZF
    [[ -f ~/.fzf.bash ]] && [[ "$SHELL" == "/bin/bash" ]] && source ~/.fzf.bash

    # Lazily load NVM
    function nvm() {
        export NVM_DIR="$HOME/.nvm"
        [[ -s "$NVM_DIR/nvm.sh" ]] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
        [[ -s "$NVM_DIR/bash_completion" ]] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
        nvm $@
    }

    # Prompt
    function set_prompt() {
        if [[ $(id -u) == "0" ]]; then
            local PROMPT_COLOUR=31
        else
            local PROMPT_COLOUR=34
        fi
        local EXIT_CODE_MESSAGE="\$(EXIT=\$?; if [[ \$EXIT != 0 ]]; then echo \"\[\e[0;31m\]\$EXIT\[\e[0m\] \"; fi)"
        PS1="\u\[\e[1;${PROMPT_COLOUR}m\]@\[\e[0m\]\h \[\e[1;${PROMPT_COLOUR}m\]\w\[\e[0m\] $EXIT_CODE_MESSAGE$ "
    }
    set_prompt

fi
