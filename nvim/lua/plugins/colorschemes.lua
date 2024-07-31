return {
	-- catppuccin
	{
		"catppuccin/nvim",
		lazy = false,
		name = "catppuccin",
		priority = 1000,
		config = function()
			vim.cmd([[colorscheme catppuccin-mocha]])
		end,
	},

	-- gruvbox
	{
		"morhetz/gruvbox",
		lazy = true,
		priority = 999,
	},

	-- tokyonight
	{
		"folke/tokyonight.nvim",
		lazy = true,
		priority = 998,
		--  config = function()
		--    local tokyonight = require("tokyonight")
		--    tokyonight.setup({ style = "moon" })
		--    tokyonight.load()
		--  end,
	},
}
