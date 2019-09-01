set --export PATH "$HOME/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/.cargo/bin:$HOME/.rvm/bin:$PATH"
set --export EDITOR nvim
set --export VISUAL nvim

alias tmp 'pushd (mktemp -d)'
alias vim nvim
alias vmi nvim
alias explicit-non-default-packages "bash -c 'comm -23 <(pacman -Qqe | sort) <(pacman -Qqg base base-devel | sort)'"
alias orphaned-packages "pacman -Qdtq"

if type gls ^/dev/null >/dev/null
    alias ls 'gls --color --human-readable --group-directories-first'
else
    alias ls 'ls --color --human-readable --group-directories-first'
end

set --export LS_COLORS "di=1;34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=1;30;42:ow=1;30;43"

if type keychain ^/dev/null >/dev/null
    eval (keychain --quiet --agents ssh id_rsa --eval)
end

if type rustc ^/dev/null >/dev/null
    set --export RUST_SRC_PATH (rustc --print sysroot)"/lib/rustlib/src/rust/src"
end

fish_vi_key_bindings
bind --mode insert \ce end-of-line
bind --mode insert \ca beginning-of-line
bind --mode default n up-or-search
bind --mode default t down-or-search
bind --mode default h backward-char
bind --mode default s forward-char
bind --mode default H backward-word
bind --mode default S forward-word
bind --mode insert \e\[1\;5C forward-word
bind --mode insert \e\[1\;5D backward-word
bind --mode normal \e\[1\;5C forward-word
bind --mode normal \e\[1\;5D backward-word

alias refish "source $HOME/.config/fish/config.fish"
