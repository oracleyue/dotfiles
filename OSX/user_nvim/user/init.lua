return {
  -- Set global colorscheme to use
  -- colorscheme = "ayu",

  -- Advanced configs for Neovide and tty
  polish = function()
    if vim.g.neovide then
      vim.cmd[[colorscheme ayu]]  -- require plugin "ayu-theme/ayu-vim"
    else
      vim.cmd[[colorscheme astromars]]
    end
  end,
}
