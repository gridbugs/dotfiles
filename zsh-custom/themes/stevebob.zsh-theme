USER=%{$fg_bold[blue]%}%n%{$reset_color%}
MACHINE=%{$fg_bold[red]%}%m%{$reset_color%}
DIR=%{$fg_bold[green]%}%d%{$reset_color%}
AT=%{$fg_bold[white]%}@%{$reset_color%}

TOP='$DIR $USER$AT$MACHINE $(git_prompt_info)'
PROMPT_PREFIX='%{$fg_bold[white]%}$%{$reset_color%} '
VIM_PROMPT_PREFIX='%{$fg_bold[red]%}$%{$reset_color%} '

TIME="%D{%H:%M:%S}"
RPROMPT="%{$fg_bold[white]%}$TIME%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

function zle-line-init zle-keymap-select {
    PROMPT="
$TOP
${${KEYMAP/vicmd/$VIM_PROMPT_PREFIX}/(main|viins)/$PROMPT_PREFIX}"

    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
