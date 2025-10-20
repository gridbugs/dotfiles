return {
    -- General language support
    { "neovim/nvim-lspconfig" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/nvim-cmp" },
    { "nvim-treesitter/nvim-treesitter" },

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

    -- Better ocaml support
    { "ocaml/vim-ocaml" },
    { "tarides/ocaml.nvim",
      lazy = false,
      config = function()
        require("ocaml")
      end
    },
    { "ocaml-mlx/ocaml_mlx.nvim" },

    -- Cram tests (used for dune tests)
    { "gridbugs/vim-cram" },

    -- Better nix support
    { "LnL7/vim-nix" },

    -- Better ledger support
    { "ledger/vim-ledger" },

    -- Better go support
    { "ray-x/go.nvim" },

    -- Better LaTeX support
    { "lervag/vimtex" },

    -- Multiple cursors
    { "mg979/vim-visual-multi" },

    -- Formatting code without an lsp server
    { "mhartington/formatter.nvim" },
  }
