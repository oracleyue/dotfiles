return {
  -- Add a key group for code runner
  {
    "folke/which-key.nvim",
    opts = {
      defaults = {
        ["<leader>r"] = { name = "+runner" },
      },
    },
  },
  -- Evaluate python codes in Iron
  {
    "Vigemus/iron.nvim",
    config = function(plugins, opts)
      local iron = require("iron.core")
      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            python = {
              command = { "python" },
            },
          },
          repl_open_cmd = require("iron.view").split("40%"),
        },
        -- if the highlight is on, you can change how it looks. For the available options, check nvim_set_hl
        highlight = {
          italic = true,
        },
        ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
      })

      -- list of keymaps, see :h iron-commands for all available commands
      vim.keymap.set("n", "<leader>rl", iron.send_line, { desc = "Iron: send line" })
      vim.keymap.set("n", "<leader>rf", iron.send_file, { desc = "Iron: send file" })
      vim.keymap.set("n", "<leader>ru", iron.send_until_cursor, { desc = "Iron: send until cursor" })
      vim.keymap.set("v", "<leader>rc", iron.visual_send, { desc = "Iron: send region" })

      vim.keymap.set("n", "<leader>rI", function() iron.send(nil, string.char(03)) end, { desc = "Iron: interrupt" })
      vim.keymap.set("n", "<leader>rX", function() iron.send(nil, string.char(12)) end, { desc = "Iron: clear" })
      vim.keymap.set("n", "<leader>rQ", iron.close_repl, { desc = "Iron: exit" })

      vim.keymap.set("n", "<leader>rs", "<cmd>IronRepl<cr>")
      vim.keymap.set("n", "<leader>rR", "<cmd>IronRestart<cr>")
      vim.keymap.set("n", "<leader>rF", "<cmd>IronFocus<cr>")
      vim.keymap.set("n", "<leader>rH", "<cmd>IronHide<cr>")

      vim.keymap.set({ "n", "v" }, "<leader>r<space>", function() end, { desc = "<Nop>" })
    end,
  },
}
