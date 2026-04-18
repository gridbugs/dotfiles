return {
  -- Set of LSP configurations for various languages
  "https://github.com/neovim/nvim-lspconfig",

  -- Autocomplete with LSP
  "https://github.com/hrsh7th/cmp-nvim-lsp",
  "https://github.com/hrsh7th/nvim-cmp",

  -- Autocompletion based on ctags
  "https://github.com/quangnguyen30192/cmp-nvim-tags",

  -- Alternative formatting from LSP
  "https://github.com/mhartington/formatter.nvim",

  -- Catppuccin colour scheme
  "https://github.com/catppuccin/nvim",

  -- Multiple cursors
  "https://github.com/mg979/vim-visual-multi",

  -- Dependency for many plugins that display icons
  "https://github.com/nvim-tree/nvim-web-devicons",

  -- Status line
  "https://github.com/nvim-lualine/lualine.nvim",

  -- Fuzzyfind
  "https://github.com/ibhagwan/fzf-lua",

  -- Git
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/airblade/vim-gitgutter",

  -- File tree ui
  {
    src = 'https://github.com/nvim-neo-tree/neo-tree.nvim',
    version = vim.version.range('3')
  },
  -- ...and extra dependencies for neo-tree
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/MunifTanjim/nui.nvim",

  -- Treesitter
  "https://github.com/neovim-treesitter/nvim-treesitter",

  -- Git integration
  "https://github.com/airblade/vim-gitgutter",
  "https://github.com/tpope/vim-fugitive",

  -- Plugins for specific languages
  "https://github.com/rust-lang/rust.vim",
  "https://github.com/ledger/vim-ledger",
  "https://github.com/LnL7/vim-nix",
  "https://github.com/imsnif/kdl.vim",
}
