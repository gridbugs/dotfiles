lua <<EOF
require("catppuccin").setup({
    flavour = "macchiato",
    color_overrides = {
        macchiato = {
        },
    },
    highlight_overrides = {
        all = function(colors)
            return {
                MatchParen = { bg = colors.surface2 },
                LineNr = { fg = colors.overlay1 },
                Comment = { fg = colors.overlay1 },
            }
        end,
    },
})
vim.cmd.colorscheme "catppuccin"
EOF
   source ~/.vim/catppuccin-lightline.vim
   let g:lightline = {
      \ 'colorscheme': 'catppuccin',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'relativepath', 'modified' ] ]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'readonly', 'relativepath', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

    " Don't display the mode again in the statusbar
    set noshowmode

