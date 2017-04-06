export ZSH=$HOME/.oh-my-zsh

ZSH_CUSTOM="$HOME/.zsh-custom"
ZSH_THEME="stevebob"

plugins=(git cargo rust archlinux z)

export PATH="$HOME/bin:$HOME/.cargo/bin:$HOME/.local/bin:$PATH"

alias tmp='pushd `mktemp -d`'

eval `keychain --quiet --eval --agents ssh id_rsa`

source $ZSH/oh-my-zsh.sh

if test -f $HOME/.profile; then
    source $HOME/.profile
fi
