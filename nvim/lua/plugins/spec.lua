return {
    -- Plugins for integrating git into vim
    { "tpope/vim-fugitive" },  -- git commands
    { "airblade/vim-gitgutter" }, -- display changed lines in gutter

    -- Tree file explorer
    {
      "nvim-neo-tree/neo-tree.nvim",
      branch = "v3.x",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        "MunifTanjim/nui.nvim",
      }
    },

    -- Status line
    {
      'nvim-lualine/lualine.nvim',
      dependencies = { 'nvim-tree/nvim-web-devicons' }
    },

    -- Colour scheme
    { "catppuccin/nvim", name = "catppuccin", priority = 1000 },

    -- Fuzzy find
    {
      "ibhagwan/fzf-lua",
      -- optional for icon support
      dependencies = { "nvim-tree/nvim-web-devicons" },
      config = function()
        -- calling `setup` is optional for customization
        require("fzf-lua").setup({})
      end
    },

    -- Better rust support
    { "rust-lang/rust.vim" },

    -- Cram tests (used for dune tests)
    { "gridbugs/vim-cram" },

    -- Version info in Cargo.toml
    {
      'saecki/crates.nvim',
      tag = 'stable',
      config = function()
        require('crates').setup()
      end,
    },

    -- Better nix support
    { "LnL7/vim-nix" },

    -- Better ledger support
    { "ledger/vim-ledger" },

    -- Multiple cursors
    { "mg979/vim-visual-multi" },
  }
