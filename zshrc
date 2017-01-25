export ZSH=$HOME/.oh-my-zsh

ZSH_CUSTOM="$HOME/.zsh-custom"
ZSH_THEME="stevebob"

plugins=(git cargo rust archlinux z)

export PATH="$HOME/bin:$HOME/.cargo/bin:$PATH"

alias ls='ls -Fp --group-directories-first --color=auto'

eval `keychain --quiet --eval --agents ssh id_rsa`

source $ZSH/oh-my-zsh.sh
