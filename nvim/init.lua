require("config.lazy")

require("config/catpuccin")
require("config/neotree")
require("config/lualine")



require("lsp")

-- Keybinding to toggle between .ml and .mli file when editing ocaml
vim.cmd("au FileType ocaml map <leader>m :e %:p:s,.mli$,.X123X,:s,.ml$,.mli,:s,.X123X$,.ml,<CR>")

-- Load the common vim configuration
local vimrc = vim.fn.expand("~/.vimrc")
if vim.loop.fs_stat(vimrc) ~= nil then
  vim.cmd("source " .. vimrc)
end
