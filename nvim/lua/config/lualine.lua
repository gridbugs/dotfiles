-- Configure the status line

require('lualine').setup {
  sections = {
    lualine_a = {},
    lualine_c = {
      {
        'filename',
        path = 1,
      }
    }
  },
  inactive_sections = {
    lualine_c = {
      {
        'filename',
        path = 1,
      }
    }
  },
}
