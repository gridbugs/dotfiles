#!/usr/bin/env bash

# Only run if the shell is interactive
if [[ $- == *i* ]]; then

    export PATH="$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/.cargo/bin:$PATH"

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

    # Tmux unicode and 256 colour support
    alias tmux='tmux -u -2'

    # Allow aliases in sudo
    alias sudo='sudo '

    # Some handy aliases
    alias tmp='pushd $(mktemp -d)'
    alias rebash='source $HOME/.bashrc'
    alias AOEU='aoeu'
    alias ASDF='asdf'
    alias nix-shell='nix-shell --command "$(declare -p PS1); return"'

    # Archlinux-specific pacman helpers
    if type pacman 2>/dev/null >/dev/null; then
        alias explicit-non-default-packages='comm -23 <(pacman -Qqe | sort) <(pacman -Qqg base base-devel | sort)'
        alias orphaned-packages='pacman -Qdtq'
    fi

    # Set colours for `ls` to use. If gnu-ls is installed, prefer it over ls.
    export LS_COLORS='di=1;34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=1;30;42:ow=1;30;43'
    if type gls 2>/dev/null >/dev/null; then
        alias ls='gls --color --human-readable --group-directories-first'
    elif type colorls 2>/dev/null >/dev/null; then
        alias ls='colorls -G'
    elif ls --color --group-directories-first 2>/dev/null >/dev/null; then
        alias ls='ls --color -h --group-directories-first'
    elif ls --color 2>/dev/null >/dev/null; then
        # freebsd appears to understand --color, but not --group-directories-first
        alias ls='ls --color -h'
    else
        # this case will be hit on macos and bsd without gls installed
        alias ls='ls -h'
    fi

    # Customize bash history behaviour
    export HISTCONTROL=ignoredups:erasedups
    export HISTSIZE=100000
    export HISTFILESIZE=100000
    shopt -s histappend

    # Start keychain if it is installed
    if type keychain 2>/dev/null >/dev/null && [[ $(basename "$SHELL") == "bash" ]] && [[ -e ~/.ssh/id_rsa ]]; then
       eval $(keychain --quiet --agents ssh id_rsa --eval)
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


    # opam configuration
    test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

    # FZF
    [[ -f ~/.fzf.bash ]] && [[ "$SHELL" == "/bin/bash" ]] && source ~/.fzf.bash

    # Lazily load NVM
    nvm() {
        if [[ -d ~/.nvm ]]; then
            if type realpath 2>/dev/null >/dev/null; then
                export NVM_DIR=$(realpath "$HOME/.nvm")
            else
                export NVM_DIR="$HOME/.nvm"
            fi
            [[ -s "$NVM_DIR/nvm.sh" ]] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
            [[ -s "$NVM_DIR/bash_completion" ]] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
            nvm $@
        else
            echo nvm is not installed
            echo install with:
            echo "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash"
        fi
    }

    # RVM wants to be at the end of PATH
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" && export PATH="$PATH:$HOME/.rvm/bin" 2> /dev/null

    man() {
        LESS_TERMCAP_md=$'\e[01;31m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[01;44;33m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[01;32m' \
        command man "$@"
    }

    # Git Prompt
    [[ -f ~/.git-prompt.sh ]] && source ~/.git-prompt.sh

    __colour_by_command_output() {
        # change STYLE_SUFFIX until you like the colours of your username/hostname as you'll see them a lot!
        STYLE_SUFFIX=ba
        if type cksum 2>/dev/null >/dev/null && type cut 2>/dev/null >/dev/null; then
            NUM_COLOURS=12
            NUM_COLOURS_NORMAL=6
            BASE_COLOUR_NORMAL=31
            BASE_COLOUR_BRIGHT=91
            TO_HASH=$($@)$STYLE_SUFFIX
            HASH_SIGNED=$((16#$(cksum <(echo $TO_HASH) | cut -d' ' -f1)))
            HASH_POSITIVE=${HASH_SIGNED#-}
            INDEX=$(($HASH_POSITIVE % $NUM_COLOURS))
            if [ $INDEX -lt $NUM_COLOURS_NORMAL ]; then
                RET=$(($BASE_COLOUR_NORMAL + $INDEX))
            else
                RET=$(($BASE_COLOUR_NORMAL + $INDEX - $NUM_COLOURS_NORMAL))
            fi
        else
            RET=0
        fi
        echo $RET
    }

    __prompt_command() {
        local EXIT="$?";

        if [[ $(id -u) == '0' ]]; then
            local TERMINATOR="#"
        else
            local TERMINATOR="$"
        fi

        if type __git_ps1 2>/dev/null >/dev/null; then
            local GIT_MESSAGE="$(GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWUPSTREAM=auto __git_ps1) "
        else
            local GIT_MESSAGE=" "
        fi

        if [[ $EXIT != 0 ]]; then
            local RED=31
            local DEFAULT_COLOUR=39
            local EXIT_CODE_MESSAGE="\[\e[${RED}\]m$EXIT\[\e[${DEFAULT_COLOUR}m\] "
        else
            local EXIT_CODE_MESSAGE=""
        fi

        PS1="\[\e[1m\]\u@\h \w$GIT_MESSAGE$EXIT_CODE_MESSAGE$TERMINATOR\[\e[0m\] "
    }

    PROMPT_COMMAND=__prompt_command

    # source extra commands from .bashrc_extra
    [[ -f ~/.bashrc_extra ]] && source ~/.bashrc_extra

    true
fi
