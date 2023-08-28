#!/usr/bin/env bash

# Only run if the shell is interactive
if [[ $- == *i* ]]; then

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

    # Tmux unicode and 256 colour support
    alias tmux='tmux -u -2'

    # Allow aliases in sudo
    alias sudo='sudo '

    # Some handy aliases
    alias tmp='pushd $(mktemp -d)'
    alias rebash='. $HOME/.bashrc'

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
        alias ls='ls --color --group-directories-first'
    elif ls --color 2>/dev/null >/dev/null; then
        # freebsd appears to understand --color, but not --group-directories-first
        alias ls='ls --color'
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

    # Lazily load NVM to speed up bashrc
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
                 . <(npm completion)
            fi
            nvm $@
        else
            echo nvm is not installed
            echo Installation instructions: https://github.com/nvm-sh/nvm#installing-and-updating
        fi
    }

    # Start nix environment if present
    if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
    fi
    if [ -e ~/.nix-profile/lib/locale ]; then
        export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
    fi
    if [ -e ~/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then
        . ~/.nix-profile/etc/profile.d/hm-session-vars.sh
    fi

    if [[ ! -n "$IN_NIX_SHELL" ]]; then
        # Don't start rvm in nix-shell. It's assumed that if we're in a nix-shell then we want nix to manage our ruby.
        [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
    fi

    if type rustc 2>/dev/null >/dev/null && [[ -d ~/.cargo ]]; then
        find_rust_completions() {
            local C1 C2 C3 C4 RUST_COMPLETION
            C1=$(rustc --print sysroot)/etc/bash_completion.d/cargo
            C2=$(rustc --print sysroot)/share/bash-completion/completions/cargo
            C3=$(rustc --print sysroot)/etc/bash_completion.d/cargo.bashcomp.sh
            C4=$(rustc --print sysroot)/share/bash-completion/completions/cargo.bashcomp.sh
            RUST_COMPLETION=/dev/null
            for C in $C1 $C2 $C3 $C4; do
                if [[ -f $C ]]; then
                    RUST_COMPLETION=$C
                    break
                fi
            done
            echo $RUST_COMPLETION
        }
        RUST_COMPLETION=$(find_rust_completions)
        if [[ -f $RUST_COMPLETION ]]; then
            . $RUST_COMPLETION
        fi
    fi

    # Enable extended globbing. This feature is needed by nix bash completions
    shopt -s extglob

    if [[ -f ~/.nix-profile/share/bash-completion/completions/_nix ]]; then
        . ~/.nix-profile/share/bash-completion/completions/_nix
    fi

    # try to load bash completion from its default location on some systems
    if [[ -r ~/.nix-profile/share/bash-completion/bash_completion ]]; then
        export BASH_COMPLETION_DIR=$HOME/.nix-profile/share/bash-completion/completions
        . ~/.nix-profile/share/bash-completion/bash_completion
    elif [[ -r /usr/local/share/bash-completion/bash_completion.sh ]]; then
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
        if type docker 2>/dev/null >/dev/null; then
            if [[ ! -r $BASH_COMPLETION_DIR/docker ]]; then
                . ~/.completions/docker
            fi
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
    [[ -f ~/.git-prompt.sh ]] && . ~/.git-prompt.sh

    # Prevent python virtualenv from modifying the prompt
    export VIRTUAL_ENV_DISABLE_PROMPT=1

    __prompt_command() {
        local EXIT="$?";

        local PROMPT_COLOUR="\[\033[01;35m\]"
        local PROMPT_COLOUR="\[\033[01;35m\]"
        local NORMAL_COLOUR="\[\033[01;0m\]"
        local ERROR_COLOUR="\[\033[01;31m\]"
        local GIT_COLOUR="\[\033[01;32m\]"
        local VENV_COLOUR="\[\033[01;34m\]"
        local DOLLAR_COLOUR=$PROMPT_COLOUR
        local LAMBDA_COLOUR="\[\033[01;36m\]"

        if type __git_ps1 2>/dev/null >/dev/null; then
            local GIT_MESSAGE="$GIT_COLOUR$(GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWUPSTREAM=auto __git_ps1)$PROMPT_COLOUR "
        else
            local GIT_MESSAGE=" "
        fi

        if [[ $EXIT != 0 ]]; then
            local EXIT_CODE_MESSAGE="$ERROR_COLOUR$EXIT$PROMPT_COLOUR "
        else
            local EXIT_CODE_MESSAGE=""
        fi

        if [[ -n "$IN_NIX_SHELL" ]]; then
            local TERMINATOR="$NORMAL_COLOUR\n$LAMBDA_COLOUR λ$PROMPT_COLOUR"
        else
            local TERMINATOR="$NORMAL_COLOUR\n$DOLLAR_COLOUR \\\$$PROMPT_COLOUR"
        fi

        if [[ -n "$VIRTUAL_ENV" ]]; then
            local VENV_MESSAGE="$VENV_COLOUR(${VIRTUAL_ENV##*/})$PROMPT_COLOUR "
        else
            local VENV_MESSAGE=""
        fi

        PS1="\n$PROMPT_COLOUR\u@\H \w$GIT_MESSAGE$VENV_MESSAGE$EXIT_CODE_MESSAGE$TERMINATOR$NORMAL_COLOUR "
    }

    PROMPT_COMMAND=__prompt_command

    # source extra commands from .bashrc_extra
    [[ -f ~/.bashrc_extra ]] && . ~/.bashrc_extra

    if type direnv 2>/dev/null >/dev/null && [[ $(basename "$SHELL") == "bash" ]] ; then
        eval "$(direnv hook bash)"
    fi
fi

true
