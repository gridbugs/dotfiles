-- open neotree in directory of current file or close it if it's already open
vim.keymap.set("n", "<leader>d", ":Neotree dir=%:p:h<CR>")
vim.keymap.set("n", "<leader>D", ":Neotree toggle dir=%:p:h<CR>")

-- toggle between .ml and .mli file when editing ocaml
vim.cmd("au FileType ocaml map <leader>m :e %:p:s,.mli$,.X123X,:s,.ml$,.mli,:s,.X123X$,.ml,<CR>")

-- open a fuzzy file searching window
vim.keymap.set("n", "<leader>f", ":FzfLua files<CR>")
