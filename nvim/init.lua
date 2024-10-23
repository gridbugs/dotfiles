require("config.lazy")
require("config.catpuccin")
require("config.neotree")
require("config.lualine")
require("config.keys")

require("lsp")

local initial_cwd = vim.fn.getcwd()
function get_initial_cwd()
  return initial_cwd
end

-- Load the common vim configuration
local vimrc = vim.fn.expand("~/.vimrc")
if vim.loop.fs_stat(vimrc) ~= nil then
  vim.cmd("source " .. vimrc)
end
