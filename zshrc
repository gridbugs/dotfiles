export ZSH=$HOME/.oh-my-zsh

ZSH_CUSTOM="$HOME/.zsh-custom"
ZSH_THEME="stevebob"

plugins=(git cargo rust archlinux z)

export PATH="$HOME/bin:$HOME/.cargo/bin:$HOME/.local/bin:$PATH"
export EDITOR=vim

alias tmp='pushd `mktemp -d`'

if hash keychain 2>/dev/null; then
    eval `keychain --quiet --eval --agents ssh id_rsa`
fi

source $ZSH/oh-my-zsh.sh

if test -f $HOME/.profile; then
    source $HOME/.profile
fi
