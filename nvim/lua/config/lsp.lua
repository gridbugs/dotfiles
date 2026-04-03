-- Configure each language for LSP with the nvim-lspconfig plugin

-- Always autoformat on save for these types of files
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.ml", "*.mli", "*.rs" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

-- Allow inline diagnostic messages
vim.diagnostic.config({
  virtual_text = true,
})

-- OCaml
vim.lsp.enable('ocamllsp')
vim.lsp.config('ocamllsp', {
  cmd = { 'ocamllsp', '--fallback-read-dot-merlin' },
})

-- Rust
vim.lsp.enable('rust_analyzer')
vim.lsp.config('rust_analyzer', {
  capabilities = capabilies,
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = true,
    },
  },
})

-- Nix
vim.lsp.enable('nil_ls')

-- Typescript
vim.lsp.enable('ts_ls')

-- Python
vim.lsp.enable('pylsp')

-- Elixir
vim.lsp.enable('elixirls', {
  cmd = { "elixir-ls" },
})
