-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Terminal: "C-/" or "<Leader>ft" or "<Leader>fT"

-- Which-key: search keymaps "<Leader>sk"

-- Escape
vim.keymap.set("i", "jj", "<ESC>", { silent = true })
vim.keymap.set("i", "jk", "<ESC>", { silent = true })

-- Disable keymaps
-- vim.keymap.del("n", "<leader>L")

-- Snacks
vim.keymap.set("n", "<Leader>snH", ":lua Snacks.notifier.show_history()<CR>", { desc = "Notifier history"})

-- Zoom in/out (increase/decrease font size) 
for ft, fs in string.gmatch(vim.o.guifont, "([%w%s]+):h(%d+)") do
  vim.g.gui_font_face = ft
  vim.g.gui_font_default_size = fs
end
vim.g.gui_font_size = vim.g.gui_font_default_size

RefreshGuiFont = function()
  vim.opt.guifont = string.format("%s:h%s",vim.g.gui_font_face, vim.g.gui_font_size)
end

ResizeGuiFont = function(delta)
  vim.g.gui_font_size = vim.g.gui_font_size + delta
  RefreshGuiFont()
end

ResetGuiFont = function ()
  vim.g.gui_font_size = vim.g.gui_font_default_size
  RefreshGuiFont()
end

local opts = { noremap = true, silent = true }
vim.keymap.set({'n', 'i'}, "<C-=>", function() ResizeGuiFont(1)  end, opts)
vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeGuiFont(-1) end, opts)
vim.keymap.set({'n', 'i'}, "<C-0>", function() ResetGuiFont() end, opts)
