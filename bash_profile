if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . ~/.bashrc
    fi
fi

export PATH="$HOME/.cargo/bin:$PATH"
