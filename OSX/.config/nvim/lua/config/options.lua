-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Face
vim.o.guifont = "JetBrainsMono Nerd Font:h15"
-- vim.o.guifont = "FiraCode Nerd Font:h15"
-- vim.o.guifont = "RobotoMono Nerd Font:h15"

-- Basics
vim.o.autochdir = true
vim.o.shell = "/usr/local/bin/bash"

-- TAB
vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.softtabstop = 4
-- vim.o.shiftwidth = 4

-- Autoformat
vim.g.autoformat = false
