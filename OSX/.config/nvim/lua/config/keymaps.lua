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

