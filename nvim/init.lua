-- General configuration
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.pack.add(require("plugins"))
require("config.lsp")
require("config.commands")
require("config.completion")
require("config.catpuccin")
require("config.lualine")
require("config.formatter")
require("config.fzf")
require("config.keys")
require("config.neotree")

-- Load the common vim configuration
local vimrc = vim.fn.expand("~/.vimrc")
if vim.loop.fs_stat(vimrc) ~= nil then
  vim.cmd("source " .. vimrc)
end
