local nvim_lsp = require("lspconfig")
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
  pattern = { "*.ml", "*.mli", "*.rs", "*.nix" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

nvim_lsp.ocamllsp.setup {}

nvim_lsp.rust_analyzer.setup {
  capabilities = capabilities,
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = {
        command = "clippy",
      },
    },
  },
}

nvim_lsp.nil_ls.setup {
   settings = {
      ['nil'] = {
         formatting = {
            command = { "nixfmt" },
         },
      },
   },
}

nvim_lsp.ts_ls.setup {
  on_attach = on_attach,
  filetypes = { "javascript" },
  cmd = { "npx", "typescript-language-server", "--stdio" }
}

nvim_lsp.pylsp.setup{}
