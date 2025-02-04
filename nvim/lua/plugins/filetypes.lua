return {
	{
		"martian-lang/martian",
		event = "VeryLazy",
		config = function(plugin)
			vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
				pattern = "*.mro", -- Change to desired file pattern or "*" for all files
				callback = function()
					local syntax_path = plugin.dir .. "/tools/syntax/vim/martian.vim"
					vim.cmd("source " .. syntax_path)
				end,
			})
		end,
	},
}
