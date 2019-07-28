set --export PATH "$HOME/bin:$HOME/.bin:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.local/sbin:$PATH"

alias vim nvim
alias refish "source $HOME/.config/fish/config.fish"
fish_vi_key_bindings
bind --mode insert \ce end-of-line
bind --mode insert \ca beginning-of-line
bind --mode default n up-or-search
bind --mode default t down-or-search
bind --mode default h backward-char
bind --mode default s forward-char
bind --mode default H backward-word
bind --mode default S forward-word
