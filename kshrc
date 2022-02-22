if [ -f "/etc/ksh.kshrc" ]; then
    . /etc/ksh.kshrc
fi
export VISUAL="vi"

if [ $(id -u) == '0' ]; then
    TERMINATOR="#"
else
    TERMINATOR="$"
fi
BOLD="$(tput bold)"
NORMAL="$(tput sgr0)"
PS1="$BOLD$(whoami)@$(hostname -s):\$(basename \$PWD) $TERMINATOR$NORMAL "
