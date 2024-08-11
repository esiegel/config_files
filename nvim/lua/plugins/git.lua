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
	"tpope/vim-fugitive",
	dependencies = {
		"tpope/vim-rhubarb", -- for adding GBrowse support
	},
	lazy = false,
	keys = {
		{ "<leader>v", "<cmd>Gdiff<cr>", desc = "Git Diff file" },
		{ "<leader>d", diff_main, desc = "Git Diff file with main" },
	},
}
