# If not running interactively, don't do anything
[[ $- != *i* ]] && return

set -o vi

export PATH="$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/.cargo/bin:$HOME/.rvm/bin:$PATH"
export EDITOR=nvim
export VISUAL=nvim
export PS1="\h \e[1m\e[36m\w\e[0m \e[1m\$\e[0m "

alias tmp='pushd (mktemp -d)'
alias vim=nvim
alias vmi=nvim
alias explicit-non-default-packages='comm -23 <(pacman -Qqe | sort) <(pacman -Qqg base base-devel | sort)'
alias orphaned-packages='pacman -Qdtq'
alias rebash="source $HOME/.bashrc"

if hash gls 2>/dev/null; then
    alias ls='gls --color --human-readable --group-directories-first'
else
    alias ls='ls --color --human-readable --group-directories-first'
fi

export LS_COLORS='di=1;34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=1;30;42:ow=1;30;43'

if hash keychain 2>/dev/null; then
    eval $(keychain --quiet --agents ssh id_rsa --eval)
fi

if hash rustc 2>/dev/null; then
    export RUST_SRC_PATH=$(rustc --print sysroot)"/lib/rustlib/src/rust/src"
fi

[[ -f ~/.fzf.bash ]] && source ~/.fzf.bash
