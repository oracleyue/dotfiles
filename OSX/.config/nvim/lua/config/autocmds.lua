-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- running codes
vim.api.nvim_create_augroup("exec_code", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  desc = "Run Python script",
  group = "exec_code",
  pattern = "python",
  callback = function()
    vim.keymap.set("n", "<leader>rr", ":sp<CR> :term python %<CR> :startinsert<CR>", { desc = "Run script in shell" })
    -- vim.keymap.set("n", "<leader>r", ":sp<CR> :term python %<CR>", { desc = "Run script" })
  end,
})
