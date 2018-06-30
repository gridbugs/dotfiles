# Setup fzf
# ---------
if [[ ! "$PATH" == */home/steve/.fzf/bin* ]]; then
  export PATH="$PATH:/home/steve/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/steve/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/steve/.fzf/shell/key-bindings.zsh"

