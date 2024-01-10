-- Mapping data with "desc" stored directly by vim.keymap.set().
 
return {
    -- setting new keymaps
    n = {
        ["<leader>y"] = {"\"+y", desc="Copy to clipboard"},
        ["<leader>p"] = {"\"+p", desc="Paste to clipboard"},
    },

    -- change keymaps
    t = {
        -- ["<esc>"] = false,  -- setting to false to disable keymaps
        ["<esc>"] = {"<C-\\><C-n>", desc="Exit term"},
    },
}
