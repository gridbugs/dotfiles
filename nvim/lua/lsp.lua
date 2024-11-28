local nvim_lsp = require("lspconfig")

-- Always autoformat on save for these types of files
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.ml", "*.mli", "*.rs", "*.nix" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

nvim_lsp.ocamllsp.setup {}

nvim_lsp.rust_analyzer.setup {
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
