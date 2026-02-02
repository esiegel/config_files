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

	-- reads txt files that includes ansii escape codes and colors them
	{
		"m00qek/baleia.nvim",
		event = "VeryLazy",
		version = "*",
		config = function()
			vim.g.baleia = require("baleia").setup({})

			-- Command to colorize the current buffer
			vim.api.nvim_create_user_command("BaleiaColorize", function()
				vim.g.baleia.once(vim.api.nvim_get_current_buf())
			end, { bang = true })

			-- Command to show logs
			vim.api.nvim_create_user_command("BaleiaLogs", vim.g.baleia.logger.show, { bang = true })
		end,
	},
}
