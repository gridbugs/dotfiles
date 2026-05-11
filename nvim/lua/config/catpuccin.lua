-- Configure the catpuccin colour scheme

require("catppuccin").setup({
  flavour = "macchiato",
  highlight_overrides = {
    all = function(colors)
      return {
        -- Markdown headings (treesitter)
        ["@markup.heading.1.markdown"] = { fg = colors.blue,    style = { "bold" } },
        ["@markup.heading.2.markdown"] = { fg = colors.mauve,   style = { "bold" } },
        ["@markup.heading.3.markdown"] = { fg = colors.green,   style = { "bold" } },
        ["@markup.heading.4.markdown"] = { fg = colors.peach,   style = { "bold" } },
        ["@markup.heading.5.markdown"] = { fg = colors.pink,    style = { "bold" } },
        ["@markup.heading.6.markdown"] = { fg = colors.flamingo, style = { "bold" } },

        -- Markdown bold text itself
        ["@markup.strong"] = { style = { "bold" } },

        -- Other commonly-bolded things
        ["@markup.heading"] = { style = { "bold" } },  -- fallback for all headings
      }
    end,
  },
})
vim.cmd.colorscheme "catppuccin"
