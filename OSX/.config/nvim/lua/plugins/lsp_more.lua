return {
  -- add matlab_ls to lspconfig
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        matlab_ls = {
          single_file_support = true,
          settings = {
            MATLAB = {
              indexWorkspace = true,
              installPath = "/Applications/MATLAB_R2022b.app",
            },
          },
        },
      },
    },
  },

  -- fix telescope symbol bug
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      -- fix bug for lsp symbols
      -- bug fix: https://github.com/nvim-telescope/telescope.nvim/issues/964#issuecomment-1743933261
      { "<leader>sS", false },
      { "<leader>sS", "<cmd>Telescope lsp_workspace_symbols query=S<CR>", desc = "Goto Symbol (Workspace)" },
    },
  },

  -- add symbols-outline
  {
    "simrat39/symbols-outline.nvim",
    cmd = "SymbolsOutline",
    keys = { { "<leader>cs", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
    config = true,
  },
}
