local fs = require("util.fs")

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

					local filepath = vim.api.nvim_buf_get_name(0)
					local dirpath = fs.get_parent_path(filepath)

					api.tree.open({ path = nil, find_file = true, update_root = true })
					local node = api.tree.get_node_under_cursor()
					api.node.navigate.parent(node)
					node = api.tree.get_node_under_cursor()
					api.tree.change_root_to_node(node)
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

					change_dir = {
						-- Do not change the working directory when changing the trees directory
						enable = false,
					},
				},
			})
		end,
	},

	-- call hierarchy
	{
		"nvimdev/lspsaga.nvim",
		dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
		event = "LspAttach",
		keys = {
			{ "<leader>ci", "<cmd>Lspsaga incoming_calls<cr>", desc = "Incoming Calls" },
			{ "<leader>cO", "<cmd>Lspsaga outgoing_calls<cr>", desc = "Outgoing Calls" },
		},
		opts = {
			-- Disable lspsaga features already covered by other plugins
			lightbulb = { enable = false },
			beacon = { enable = false },
			symbol_in_winbar = { enable = false },
		},
	},
}
