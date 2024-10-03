require("catppuccin").setup({
  flavour = "macchiato",
  color_overrides = {
    macchiato = {},
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
