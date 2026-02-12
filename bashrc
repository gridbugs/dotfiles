#!/usr/bin/env bash

# Only run if the shell is interactive
if [[ $- == *i* ]]; then

    # Start with an empty PROMPT_COMMAND so we don't end up with duplicates when things add to it.
    PROMPT_COMMAND=""

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

    # Allow aliases in sudo and doas
    alias sudo='sudo '
    alias doas='doas '

    # Allow bash completion of commands through doas
    complete -F _command doas

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
    if type eza 2>/dev/null >/dev/null; then
        alias ls='eza --group-directories-first --git'
        alias l='eza --group-directories-first --git --icons=auto'
        alias ll='eza --group-directories-first --git --icons=auto -l'
    elif type gls 2>/dev/null >/dev/null; then
        alias ls='gls --color --human-readable --group-directories-first'
    elif type colorls 2>/dev/null >/dev/null; then
        alias ls='colorls -G'
    elif ls --color --group-directories-first 2>/dev/null >/dev/null; then
        alias ls='ls --color --group-directories-first'
    elif ls --color 2>/dev/null >/dev/null; then
        # freebsd appears to understand --color, but not --group-directories-first
        alias ls='ls --color'
    fi

    export RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"

    # Customize bash history behaviour
    export HISTCONTROL=ignoredups:erasedups
    export HISTSIZE=100000
    export HISTFILESIZE=100000
    shopt -s histappend

    # Start keychain if it is installed
    if type keychain 2>/dev/null >/dev/null && [[ $(basename "$SHELL") == "bash" ]] && [[ -e "$HOME/.ssh/id_rsa" ]]; then
       eval "$(keychain --quiet --agents ssh id_rsa --eval)"
    fi

    # Lazily load NVM to speed up bashrc
    nvm() {
        if [[ -d "$HOME/.nvm" ]]; then
            if type realpath 2>/dev/null >/dev/null; then
                NVM_DIR="$(realpath "$HOME/.nvm")"
            else
                NVM_DIR="$HOME/.nvm"
            fi
            export NVM_DIR
            [[ -s "$NVM_DIR/nvm.sh" ]] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
            [[ -s "$NVM_DIR/bash_completion" ]] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
            nvm "$@"
        else
            echo "nvm is not installed"
            echo "Installation instructions: https://github.com/nvm-sh/nvm#installing-and-updating"
        fi
    }

    # Start nix environment if present
    if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    fi
    if [ -e "$HOME/.nix-profile/lib/locale" ]; then
        LOCALE_ARCHIVE="$(readlink "$HOME/.nix-profile/lib/locale")/locale-archive"
        export LOCALE_ARCHIVE
    fi
    if [ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    fi

    if [[ -z "$IN_NIX_SHELL" ]]; then
        # Don't start rvm in nix-shell. It's assumed that if we're in a nix-shell then we want nix to manage our ruby.
        [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
    fi

    if type rustc 2>/dev/null >/dev/null && [[ -d "$HOME/.cargo" ]]; then
        # try to guess the rust toolchain to avoid running `rustc --print sysroot`
        for x in \
            $HOME/.rustup/toolchains/stable-aarch64-apple-darwin
        do
            if [[ -d $x ]]; then
                __RUST_SYSROOT=$x
                break
            fi
        done
        if [[ -z "${__RUST_SYSROOT+set}" ]]; then
            __RUST_SYSROOT="$(rustc --print sysroot)"
        fi
        __RUST_SYSROOT="$(rustc --print sysroot)"
        for x in \
            $__RUST_SYSROOT/etc/bash_completion.d/cargo \
            $__RUST_SYSROOT/share/bash-completion/completions/cargo \
            $__RUST_SYSROOT/etc/bash_completion.d/cargo.bashcomp.sh \
            $__RUST_SYSROOT/share/bash-completion/completions/cargo.bashcomp.sh
        do
            if [[ -f $x ]]; then
                . "$x"
                break
            fi
        done
    fi

    # Bash completion for Dune
    if [ -f "$HOME/.dune/share/bash-completion/completions/dune" ]; then
        . "$HOME/.dune/share/bash-completion/completions/dune"
    fi

    # Enable extended globbing. This feature is needed by nix bash completions
    shopt -s extglob

    # Load bash completions for programs installed with nix
    if [[ -f "$HOME/.nix-profile/share/bash-completion/completions/_nix" ]]; then
        . "$HOME/.nix-profile/share/bash-completion/completions/_nix"
    fi

    # Load bash completions for programs installed with homebrew
    [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

    # try to load bash completion from its default location on some systems
    if [[ -z ${BASH_COMPLETION_DIR+x} ]]; then
        if [[ -r "$HOME/.nix-profile/share/bash-completion/bash_completion" ]]; then
            export BASH_COMPLETION_DIR=$HOME/.nix-profile/share/bash-completion/completions
            . "$HOME/.nix-profile/share/bash-completion/bash_completion"
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
    fi

    # fall back to completions from dotfiles for certain programs
    if [[ -d "$HOME/.completions" ]]; then
        if [[ ! -r "$BASH_COMPLETION_DIR/git" ]]; then
            . "$HOME/.completions/git"
        fi
        if [[ ! -r "$BASH_COMPLETION_DIR/zola" ]]; then
            . "$HOME/.completions/zola"
        fi
        if [[ ! -r "$BASH_COMPLETION_DIR/jj" ]]; then
            . "$HOME/.completions/jj"
        fi
        if [[ ! -r "$BASH_COMPLETION_DIR/dune" ]]; then
            . "$HOME/.completions/dune"
        fi
        if type docker 2>/dev/null >/dev/null; then
            if [[ ! -r "$BASH_COMPLETION_DIR/docker" ]]; then
                . "$HOME/.completions/docker"
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
    [[ -f "$HOME/.git-prompt.sh" ]] && . "$HOME/.git-prompt.sh"

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
        local SSH_COLOUR="\[\033[01;33m\]"
        local DOLLAR_COLOUR=$PROMPT_COLOUR
        local LAMBDA_COLOUR="\[\033[01;36m\]"

        local GIT_MESSAGE EXIT_CODE_MESSAGE TERMINATOR VENV_MESSAGE SSH_MESSAGE

        if type __git_ps1 2>/dev/null >/dev/null; then
            GIT_MESSAGE="$GIT_COLOUR$(GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWUPSTREAM=auto __git_ps1)$PROMPT_COLOUR "
        else
            GIT_MESSAGE=" "
        fi

        if [[ $EXIT != 0 ]]; then
            EXIT_CODE_MESSAGE="$ERROR_COLOUR$EXIT$PROMPT_COLOUR "
        else
            EXIT_CODE_MESSAGE=""
        fi

        if [[ -n "$IN_NIX_SHELL" ]]; then
            TERMINATOR="$NORMAL_COLOUR\n$LAMBDA_COLOUR λ$PROMPT_COLOUR"
        else
            TERMINATOR="$NORMAL_COLOUR\n$DOLLAR_COLOUR \\\$$PROMPT_COLOUR"
        fi

        if [[ -n "$VIRTUAL_ENV" ]]; then
            VENV_MESSAGE="$VENV_COLOUR(${VIRTUAL_ENV##*/})$PROMPT_COLOUR "
        else
            VENV_MESSAGE=""
        fi

        if [[ -n "$SSH_CONNECTION" ]]; then
            SSH_MESSAGE="$SSH_COLOUR(ssh)$PROMPT_COLOUR "
        else
            SSH_MESSAGE=""
        fi

        PS1="\n\# $PROMPT_COLOUR\u@\H \w$GIT_MESSAGE$VENV_MESSAGE$SSH_MESSAGE$EXIT_CODE_MESSAGE$TERMINATOR$NORMAL_COLOUR "
    }

    PROMPT_COMMAND="__prompt_command;$PROMPT_COMMAND"

    # source extra commands from .bashrc_extra
    [[ -f "$HOME/.bashrc_extra" ]] && . "$HOME/.bashrc_extra"

    if type direnv 2>/dev/null >/dev/null && [[ $(basename "$SHELL") == "bash" ]] ; then
        eval "$(direnv hook bash)"
    fi

    if test -f $HOME/.config/broot/launcher/bash/br; then
        . $HOME/.config/broot/launcher/bash/br
    fi
fi

true
