local diff_main = function()
	local mainbranch = os.execute("git mainbranch")
	vim.cmd("Gdiff " .. mainbranch)
end

-- Git source control management
return {
	"tpope/vim-fugitive",
	lazy = false,
	keys = {
		{ "<leader>v", "<cmd>Gdiff<cr>", desc = "Git Diff file" },
		{ "<leader>d", diff_main, desc = "Git Diff file with main" },
	},
}
