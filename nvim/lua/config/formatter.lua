-- Provides the Format, FormatWrite, FormatLock, and FormatWriteLock commands
require("formatter").setup {
  -- Enable or disable logging
  logging = true,
  -- Set the log level
  log_level = vim.log.levels.WARN,
  -- All formatter configurations are opt-in
  filetype = {
    dune = {
      function()
        return {
          exe = "dune",
          args = { "format-dune-file" },
          stdin = true,
        }
      end
    },
    python = {
      function()
        return {
          exe = "black",
          args = {},
          stdin = false,
        }
      end
    }

  }
}
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
augroup("__formatter__", { clear = true })
autocmd("BufWritePost", {
  group = "__formatter__",
  command = ":FormatWrite",
})
