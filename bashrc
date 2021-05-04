#!/usr/bin/env bash

# Only run if the shell is interactive
if [[ $- == *i* ]]; then

    # Tmux unicode and 256 colour support
    alias tmux='tmux -u -2'

    # Allow aliases in sudo
    alias sudo='sudo '

    # Some handy aliases
    alias tmp='pushd $(mktemp -d)'
    alias rebash='source $HOME/.bashrc'

    # irb gets confused by my readline config
    alias irb='INPUTRC=/dev/null irb'

    # Archlinux-specific pacman helpers
    if type pacman 2>/dev/null >/dev/null; then
        alias explicit-non-default-packages='comm -23 <(pacman -Qqe | sort) <(pacman -Qqg base base-devel | sort)'
        alias orphaned-packages='pacman -Qdtq'
        alias which-package-owns-file='pacman -Qo'
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

    # opam configuration
    test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

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
            if type npm 2>/dev/null >/dev/null; then
                 source <(npm completion)
            fi
            nvm $@
        else
            echo nvm is not installed
            echo install with:
            echo "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash"
        fi
    }

    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

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
    if [[ -r /usr/local/share/bash-completion/bash_completion.sh ]]; then
        export BASH_COMPLETION_DIR=/usr/local/share/bash-completion/completions
        . /usr/local/share/bash-completion/bash_completion.sh
    elif [[ -r /usr/local/share/bash-completion/bash_completion ]]; then
        export BASH_COMPLETION_DIR=/usr/local/share/bash-completion/completions
        . /usr/local/share/bash-completion/bash_completion
    elif [[ -r /usr/share/bash-completion/bash_completion ]]; then
        export BASH_COMPLETION_DIR=/usr/share/bash-completion/completions
        . /usr/share/bash-completion/bash_completion
    elif [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]]; then
        export BASH_COMPLETION_DIR="/usr/local/etc/bash_completion.d"
        export BASH_COMPLETION_COMPAT_DIR=$BASH_COMPLETION_DIR
        . "/usr/local/etc/profile.d/bash_completion.sh"
    fi

    # fall back to completions from dotfiles for certain programs
    if [[ -d ~/.completions ]]; then
        if [[ ! -r $BASH_COMPLETION_DIR/git ]]; then
            . ~/.completions/git
        fi
    fi

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
            local EXIT_CODE_MESSAGE="\[\033[01;31m\]$EXIT\[\033[01;39m\] "
        else
            local EXIT_CODE_MESSAGE=""
        fi

        PS1="\[\033[01;1m\]\h \w$GIT_MESSAGE$EXIT_CODE_MESSAGE$TERMINATOR\[\033[01;0m\] "
    }

    PROMPT_COMMAND=__prompt_command

    # source extra commands from .bashrc_extra
    [[ -f ~/.bashrc_extra ]] && source ~/.bashrc_extra

    true
fi
