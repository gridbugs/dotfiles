local nvim_lsp = require("lspconfig")

-- Always autoformat on save
vim.cmd("autocmd BufWritePre *.ml,*.mli,*.rs,*.nix lua vim.lsp.buf.format({ async = false })")

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'ocaml',
  callback = function(args)
    vim.lsp.start({
      name = 'ocaml-lsp-server',
      cmd = {'ocamllsp.sh'},
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
      root_dir = vim.fs.root(args.buf, {'Cargo.lock'}),
    })
  end,
})

nvim_lsp.nil_ls.setup({
   settings = {
      ['nil'] = {
         formatting = {
            command = { "nixfmt" },
         },
      },
   },
})
