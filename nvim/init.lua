require("config.lazy")
require("config.catpuccin")
require("config.neotree")
require("config.lualine")
require("config.formatter")
require("config.keys")
require("config.treesitter")
require("lsp")

require('cmp').setup({
  sources = {
    { name = 'nvim_lsp' }, -- enable autocompletion based on lsp
    { name = 'tags' },  -- enable autocompletion based on ctags
  }
})

-- Load the common vim configuration
local vimrc = vim.fn.expand("~/.vimrc")
if vim.loop.fs_stat(vimrc) ~= nil then
  vim.cmd("source " .. vimrc)
end
