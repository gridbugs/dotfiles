-- Configure code completion with LSP

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
