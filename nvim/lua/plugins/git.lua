local shell = require("util.shell")

local diff_main = function()
	local mainbranch, err = shell.cmd("git mainbranch")
	if err then
		return
	end

	vim.cmd("Gdiff " .. mainbranch)
end

-- Git source control management
return {
	{
		"tpope/vim-fugitive",
		dependencies = {
			"tpope/vim-rhubarb", -- for adding GBrowse support
		},
		lazy = false,
		keys = {
			{ "<leader>v", "<cmd>Gdiff<cr>", desc = "Git Diff file" },
			{ "<leader>d", diff_main, desc = "Git Diff file with main" },
		},
	},

	{
		"lewis6991/gitsigns.nvim",
		event = "VeryLazy",
		config = function()
			local base, err = shell.cmd("git merge-base main HEAD")
			if err then
				print("THERE WAS AN ERROR GETTING MERGE BASE")
				return
			end

			-- remove the trailing newline
			base = string.sub(base, 0, -2)

			require("gitsigns").setup({ base = base })
		end,
	},
}
