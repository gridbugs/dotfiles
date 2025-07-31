function file_exists(name)
  local f = io.open(name, "r")
  if f == nil then
    return false
  else
    io.close(f)
    return true
  end
end

function has_noformat()
  return file_exists(".noformat")
end

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
        if has_noformat() then
          return nil
        else
          return {
            exe = "dune",
            args = { "format-dune-file" },
            stdin = true,
          }
        end
      end
    },
    python = {
      function()
        if has_noformat() then
          return nil
        else
          return {
            exe = "black",
            args = {},
            stdin = false,
          }
        end
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
