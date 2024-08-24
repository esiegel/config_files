-- local api = require("nvim-tree.api")
-- api.tree.toggle()
-- "<cmd>NvimTreeFindFileToggle!<cr>",

return {
  -- nvim-tree
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader>n", "<cmd>NvimTreeToggle<cr>", desc = "NvimTree Toggle" },
      {
        "<C-d>",
        function()
          local api = require("nvim-tree.api")
          api.tree.open({ path = nil, find_file = true, update_root = true })
          api.tree.focus()
        end,
        desc = "NvimTree Toggle open file",
      },
    },
    config = function()
      require("nvim-tree").setup({
        actions = {
          open_file = {
            window_picker = {
              enable = false,
            },
          },
        },
      })
    end,
  },

  --calltree--
  {
    'ldelossa/litee.nvim',
    event = "VeryLazy",
    opts = {
      notify = { enabled = false },
      panel = {
        orientation = "bottom",
        panel_size = 10,
      },
    },
    config = function(_, opts) require('litee.lib').setup(opts) end
  },

  {
    'ldelossa/litee-calltree.nvim',
    dependencies = 'ldelossa/litee.nvim',
    event = "VeryLazy",
    opts = {
      on_open = "panel",
      map_resize_keys = false,
    },
    config = function(_, opts) require('litee.calltree').setup(opts) end
  },

}
