return {
  -- Set colorscheme 
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

  -- Polish for advanced configurations
  polish = function()
    -- configs only for neovide
    if vim.g.neovide then
      -- vim.cmd[[colorscheme astrodark]]
      vim.cmd[[colorscheme ayu]]
    end 

    -- manage autocommands
    vim.api.nvim_create_augroup("exec_code", { clear = true })
    vim.api.nvim_create_autocmd("FileType", {
      desc = "Run Python script",
      group = "exec_code",
      pattern = "python",
      callback = function(opts)
        vim.keymap.set("n", "<leader>r", 
                       ":sp<CR> :term python %<CR> :startinsert<CR>",
                       { desc="Run script" })
      end
    })
  end,
}
