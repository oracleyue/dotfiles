return {
  -- add more lsp langservers
  {
    "neovim/nvim-lspconfig",
    -- opts = function()
    --   require("lspconfig").matlab_ls.setup({})
    -- end,
    opts = {
      diagnostics = {
        virtual_text = false, -- remove inline error messages
      },
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

  -- telescope: fix symbol bug
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

  -- interactive REPLs (bash, python)
  {
    "Vigemus/iron.nvim",
    config = function(plugins, opts)
      local iron = require("iron.core")
      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            sh = {
              command = {"bash"}
            },
            python = {
              command = { "python" },  -- or { "ipython", "--no-autoindent" }
              format = require("iron.fts.common").bracketed_paste_python
            },
          },
          repl_open_cmd = require("iron.view").split("40%"),
        },
        -- if the highlight is on, you can change how it looks. For the available options, check nvim_set_hl
        highlight = { italic = true },
        ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
      })

      -- list of keymaps, see :h iron-commands for all available commands
      vim.keymap.set("n", "<leader>rl", iron.send_line, { desc = "Iron: send line" })
      vim.keymap.set("n", "<leader>rf", iron.send_file, { desc = "Iron: send file" })
      vim.keymap.set("n", "<leader>ru", iron.send_until_cursor, { desc = "Iron: send until cursor" })
      vim.keymap.set("v", "<leader>rc", iron.visual_send, { desc = "Iron: send region" })

      vim.keymap.set("n", "<leader>rI", function()
        iron.send(nil, string.char(03))
      end, { desc = "Iron: interrupt" })
      vim.keymap.set("n", "<leader>rX", function()
        iron.send(nil, string.char(12))
      end, { desc = "Iron: clear" })
      vim.keymap.set("n", "<leader>rQ", iron.close_repl, { desc = "Iron: exit" })

      vim.keymap.set("n", "<leader>rs", "<cmd>IronRepl<cr>")
      vim.keymap.set("n", "<leader>rR", "<cmd>IronRestart<cr>")
      vim.keymap.set("n", "<leader>rF", "<cmd>IronFocus<cr>")
      vim.keymap.set("n", "<leader>rH", "<cmd>IronHide<cr>")

      vim.keymap.set({ "n", "v" }, "<leader>r<space>", function() end, { desc = "<Nop>" })
    end,
  },

  -- add REPL keybindings in which-key 
  {
    "folke/which-key.nvim",
    opts = {
      spec = {
        { "<leader>r", group = "run", icon = " " },
      },
    },
  },

}
