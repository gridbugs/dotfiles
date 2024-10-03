require("config.lazy")
require("config.catpuccin")
require("config.neotree")
require("config.lualine")
require("config.keys")

require("lsp")

-- Load the common vim configuration
local vimrc = vim.fn.expand("~/.vimrc")
if vim.loop.fs_stat(vimrc) ~= nil then
  vim.cmd("source " .. vimrc)
end
