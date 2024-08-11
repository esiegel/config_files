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

	{
		"morhetz/gruvbox",
		lazy = true,
		priority = 999,
	},

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

	{
		"EdenEast/nightfox.nvim",
		lazy = true,
		priority = 998,
	},

	{
		"rebelot/kanagawa.nvim",
		lazy = true,
		priority = 998,
	},
}
