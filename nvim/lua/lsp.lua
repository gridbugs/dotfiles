-- Keybindings for lsp
vim.keymap.set("n", "<leader>d", '<cmd>lua vim.lsp.buf.definition()<CR>')
vim.keymap.set("n", "<leader>t", '<cmd>lua vim.lsp.buf.hover()<CR>')

-- Always autoformat on save
vim.cmd("autocmd BufWritePre * lua vim.lsp.buf.format({ async = false })")

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'ocaml',
  callback = function(args)
    vim.lsp.start({
      name = 'ocaml-lsp-server',
      cmd = {'opam', 'exec', 'ocamllsp', '--'},
      root_dir = vim.fs.root(args.buf, {'dune-workspace', 'dune-project'}),
    })
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'rust',
  callback = function(args)
    vim.lsp.start({
      name = 'rust-analyzer',
      cmd = {'rust-analyzer'},
      root_dir = vim.fs.root(args.buf, {'Cargo.toml'}),
    })
  end,
})
