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
    alias AOEU='aoeu'
    alias ASDF='asdf'

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

    if type inotifywait 2>/dev/null >/dev/null; then
      function do-on-every-file-change {
         while true; do
           bash -c "$@"
           inotifywait -qre close_write .
         done
      }
    fi

    if type sbt 2>/dev/null >/dev/null; then
        function sbt-new {
            name=$1
            sbt new sbt/scala-seed.g8 --name=$name
        }
    fi

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
        source $(rustc --print sysroot)/etc/bash_completion.d/cargo
    fi

    # opam configuration
    test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

    # FZF
    [[ -f ~/.fzf.bash ]] && [[ "$SHELL" == "/bin/bash" ]] && source ~/.fzf.bash

    # Lazily load NVM
    function nvm() {
        if [[ -d ~/.nvm ]]; then
            export NVM_DIR="$HOME/.nvm"
            [[ -s "$NVM_DIR/nvm.sh" ]] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
            [[ -s "$NVM_DIR/bash_completion" ]] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
            nvm $@
        else
            echo nvm is not installed
            echo install with:
            echo "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash"
        fi
    }

    # RVM
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 2> /dev/null

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

    function __colour_by_command_output {
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

    # Prompt
    PS1="\$(
        EXIT=\$?;
        if [[ \$(id -u) == '0' ]]; then
            PROMPT_COLOUR=97
            PROMPT_TERMINATOR=\"\[\e[1;\${PROMPT_COLOUR}m\]#\[\e[0m\]\"
        else
            PROMPT_COLOUR=97
            PROMPT_TERMINATOR=\"\[\e[1;\${PROMPT_COLOUR}m\]$\[\e[0m\]\"
        fi
        if [[ \$EXIT != 0 ]]; then
            RED=31
            EXIT_CODE_MESSAGE=\"\[\e[0;\${RED}m\]\$EXIT\[\e[0m\] \"
        else
            EXIT_CODE_MESSAGE=''
        fi
        if type __git_ps1 2>/dev/null >/dev/null; then
            GIT_MESSAGE=\"\$(GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWUPSTREAM=auto __git_ps1) \"
        else
            GIT_MESSAGE=\" \"
        fi
        if type git 2>/dev/null >/dev/null && git rev-parse --is-inside-work-tree 2>/dev/null >/dev/null; then
            GIT_COLOUR=\$(__colour_by_command_output git rev-parse HEAD 2> /dev/null)
        else
            GIT_COLOUR=0
        fi
        if type hostname 2>/dev/null >/dev/null; then
            HOSTNAME_COLOUR=\$(__colour_by_command_output hostname)
        elif [[ -f /etc/hostname ]]; then
            HOSTNAME_COLOUR=\$(__colour_by_command_output cat /etc/hostname)
        else
            HOSTNAME_COLOUR=0
        fi
        if test whoami 2>/dev/null; then
            USERNAME_COLOUR=\$(__colour_by_command_output whoami)
        else
            USERNAME_COLOUR=0
        fi
        if test pwd 2>/dev/null; then
            PWD_COLOUR=\$(__colour_by_command_output pwd)
        else
            PWD_COLOUR=0
        fi
        BASE_PROMPT=\"\[\e[0;\${USERNAME_COLOUR}m\]\u\[\e[0m\]\[\e[1;\${PROMPT_COLOUR}m\]@\[\e[0m\]\[\e[0;\${HOSTNAME_COLOUR}m\]\h\[\e[0m\] \[\e[0;\${PWD_COLOUR}m\]\w\[\e[0m\]\"
        PROMPT=\"\$BASE_PROMPT\[\e[0;\${GIT_COLOUR}m\]\$GIT_MESSAGE\[\e[0m\]\$EXIT_CODE_MESSAGE\$PROMPT_TERMINATOR \"
        echo \"\$PROMPT\"
    )"

    # source extra commands from .bashrc_extra
    [[ -f ~/.bashrc_extra ]] && source ~/.bashrc_extra

    true
fi
