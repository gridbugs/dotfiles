local cmp = require("cmp")
cmp.setup {
  sources = {
    { name = 'nvim_lsp' }
  },
  mapping = cmp.mapping.preset.insert({
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Confirm the selected item
  }),
}

local capabilities = require('cmp_nvim_lsp').default_capabilities()
capabilities.textDocument.completion.snippetSupport = true

-- Always autoformat on save for these types of files
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.ml", "*.mli", "*.rs", "*.nix", "*.go" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

-- Allow inline diagnostic messages
vim.diagnostic.config({
  virtual_text = true,
})

vim.lsp.enable('ocamllsp')
vim.lsp.config('ocamllsp', {
  cmd = { 'ocamllsp', '--fallback-read-dot-merlin' },
})

vim.lsp.enable('rust_analyzer')
vim.lsp.config('rust_analyzer', {
  capabilities = capabilies,
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = true,
    },
  },
})

vim.lsp.enable('nil_ls')
vim.lsp.config('nil_ls', {
   settings = {
      ['nil'] = {
         formatting = {
            command = { "nixfmt" },
         },
      },
   },
})

vim.lsp.enable('ts_ls')
vim.lsp.enable('pylsp')
