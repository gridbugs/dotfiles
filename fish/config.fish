set --export PATH "$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/.cargo/bin:$HOME/.rvm/bin:$PATH"
set --export EDITOR nvim
set --export VISUAL nvim

alias vim nvim
alias vmi nvim

alias ls 'ls --color -h --group-directories-first'

if type keychain ^/dev/null >/dev/null
    eval (keychain --quiet --agents ssh id_rsa --eval)
end

set --export RUST_SRC_PATH (rustc --print sysroot)"/lib/rustlib/src/rust/src"

fish_vi_key_bindings
bind --mode insert \ce end-of-line
bind --mode insert \ca beginning-of-line
bind --mode default n up-or-search
bind --mode default t down-or-search
bind --mode default h backward-char
bind --mode default s forward-char
bind --mode default H backward-word
bind --mode default S forward-word

alias refish "source $HOME/.config/fish/config.fish"
