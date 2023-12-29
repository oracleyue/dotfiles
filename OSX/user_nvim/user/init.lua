return {
  -- Set global colorscheme to use
  -- colorscheme = "ayu",  -- require plugin "ayu-theme/ayu-vim"

  -- LSP
  lsp = {
    servers = { 
      'matlab_ls', 
    },
    config = {
      matlab_ls = {
        single_file_support = false;
        indexWorkspace = true,
        installPath = '/Applications/MATLAB_R2022b.app',
      },
    },
  },

  -- Advanced configs for Neovide and tty
  polish = function()
    if vim.g.neovide then
      -- vim.cmd[[colorscheme astromars]]
      vim.cmd[[colorscheme ayu]]
    end
  end,
}
