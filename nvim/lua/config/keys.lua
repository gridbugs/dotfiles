-- open neotree in directory of current file or close it if it's already open
vim.keymap.set("n", "<leader>d", ":Neotree dir=%:p:h<CR>")
vim.keymap.set("n", "<leader>D", ":Neotree toggle dir=%:p:h<CR>")

-- toggle between .ml and .mli file when editing ocaml
vim.cmd("au FileType ocaml map <leader>m :e %:p:s,.mli$,.X123X,:s,.ml$,.mli,:s,.X123X$,.ml,<CR>")

-- open a fuzzy file searching window
vim.keymap.set("n", "<leader>ff", ":FzfLua files<CR>")
vim.keymap.set("n", "<leader>fo", ":FzfLua oldfiles<CR>")
vim.keymap.set("n", "<leader>fb", ":FzfLua buffers<CR>")
vim.keymap.set("n", "<leader>fd", ":FzfLua diagnostics_workspace<CR>")
vim.keymap.set("n", "<leader>fg", ":FzfLua grep_visual<CR>")
vim.keymap.set("n", "<leader>fc", ":FzfLua git_commits<CR>")

-- navigate between errors reported by lsp
vim.keymap.set('n', '<leader>dn', '<cmd>lua vim.diagnostic.goto_next()<CR>')
vim.keymap.set('n', '<leader>dp', '<cmd>lua vim.diagnostic.goto_prev()<CR>')

vim.keymap.set('n', '<leader>bn', ':bnext<CR>')
vim.keymap.set('n', '<leader>bp', ':bprevious<CR>')
vim.keymap.set('n', '<leader>bd', ':bdelete<CR>')
