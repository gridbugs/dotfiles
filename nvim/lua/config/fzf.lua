require("fzf-lua").setup({
  files = {
    fd_opts = [[--color=never --type f --type l --exclude .git --hidden --no-ignore --follow]],
  },
  grep = {
    rg_opts = [[--column --line-number --no-heading --color=always --smart-case --max-columns=4096 -e --hidden --no-ignore --follow]],
  }
})
