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
}
