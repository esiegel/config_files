local shell = require("util.shell")

local inside_working_tree = function()
	local res, err = shell.cmd_silent("git rev-parse --is-inside-work-tree")

	if err then
		return false
	end

	return res == "true"
end

---@diagnostic disable-next-line: unused-local, unused-function
local has_branch = function(name)
	local sha, err = shell.cmd("git show-ref --heads master")

	if err then
		print("THERE WAS AN ERROR CHECKING BRANCH EXISTANCE")
		return false
	end

	return sha ~= ""
end

local get_mainbranch = function()
	local mainbranch, err = shell.cmd("git mainbranch")
	if err then
		print("THERE WAS AN ERROR GETTING THE MAIN BRANCH")
		return
	end

	return mainbranch
end

local diff_main = function()
	local mainbranch = get_mainbranch()

	vim.cmd("Gdiff " .. mainbranch)
end

local mergebase_main = function()
	local mainbranch = get_mainbranch()
	local base, err = shell.cmd("git merge-base " .. mainbranch .. " HEAD")
	if err then
		print("THERE WAS AN ERROR GETTING MERGE BASE")
		return
	end

	return base
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
			local opts = {}

			if inside_working_tree() then
				-- for some reason this is not being respected
				local base = mergebase_main()
				opts["base"] = base
			end

			require("gitsigns").setup(opts)
		end,
	},

	-- work with github
	{
		"pwntester/octo.nvim",
		event = "VeryLazy",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("octo").setup({
				default_to_projects_v2 = false,
				suppress_missing_scope = {
					projects_v2 = true,
				},
			})
		end,
	},
}
