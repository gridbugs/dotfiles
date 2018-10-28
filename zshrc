export ZSH=$HOME/.oh-my-zsh

ZSH_CUSTOM="$HOME/.zsh-custom"
ZSH_THEME="stevebob"

plugins=(git cargo rust archlinux z nvm gem npm)

export PATH="$HOME/bin:$HOME/.bin:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.local/sbin:$PATH"
export EDITOR=vim
export VISUAL=vim

alias tmp='pushd `mktemp -d`'
alias tmux='tmux -2'
alias portscan='nmap -sP' # follow this with e.g. 192.168.1.1-255

# typos
alias vmi='vim'

if hash keychain 2>/dev/null; then
    eval `keychain --quiet --eval --agents ssh id_rsa`
fi

fpath+=~/.zfunc

source $ZSH/oh-my-zsh.sh

export I3_MOD_KEY=Mod4

if test -f $HOME/.profile; then
    source $HOME/.profile
fi

export CARGO_INCREMENTAL=0

OPAM_ZSH=$HOME/.opam/opam-init/init.zsh
if test -f $OPAM_ZSH; then
    # OPAM configuration
    . $OPAM_ZSH > /dev/null 2> /dev/null || true
fi

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

if hash gem 2>/dev/null; then
    GEM_BIN=$(gem environment gempath | awk -v RS=: '{print}' | grep $HOME)
    PATH=$GEM_BIN:$PATH
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey -M viins '^[[3~' delete-char
bindkey -a '^[[3~' delete-char

bindkey -v
export KEYTIMEOUT=1
bindkey -a 'h' backward-char
bindkey -a 's' forward-char
bindkey -a 'H' backward-word
bindkey -a 'S' forward-word
bindkey -a 'n' up-line-or-beginning-search
bindkey -a 't' down-line-or-beginning-search

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search

export LS_COLORS="di=1;34:ln=35;40:so=32;40:pi=33;40:ex=31:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=1;42;40:ow=33;40:"
alias ls="ls --color --group-directories-first"

alias aoeu='setxkbmap en_US'
alias asdf='setxkbmap dvorak'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
