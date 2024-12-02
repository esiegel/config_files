return {
	{
		"dmmulroy/tsc.nvim",
		event = "VeryLazy",
		config = function()
			require("tsc").setup({
				use_trouble_qflist = true,
			})
		end,
	},
}
