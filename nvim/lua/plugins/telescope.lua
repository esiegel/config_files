local telescope_util = require("util.telescope")

-- yank all prompt manager entries
local yank_all_entries = function(prompt_bufnr)
	local actions = require("telescope.actions")
	local action_state = require("telescope.actions.state")
	local entry_display = require("telescope.pickers.entry_display")

	local picker = action_state.get_current_picker(prompt_bufnr)
	local manager = picker.manager

	local entries = {}
	for entry in manager:iter() do
		local display, _ = entry_display.resolve(picker, entry)
		table.insert(entries, display)
	end

	local text = table.concat(entries, "\n")

	actions.close(prompt_bufnr)

	vim.fn.setreg("+", text)
end

-- yank preview
local yank_preview_lines = function(prompt_bufnr)
	local actions = require("telescope.actions")
	local action_state = require("telescope.actions.state")

	local picker = action_state.get_current_picker(prompt_bufnr)
	local previewer = picker.previewer
	local winid = previewer.state.winid
	local bufnr = previewer.state.bufnr

	local line_start = vim.fn.line("w0", winid)
	local line_end = vim.fn.line("w$", winid)

	local lines = vim.api.nvim_buf_get_lines(bufnr, line_start, line_end, false)

	local text = table.concat(lines, "\n")

	actions.close(prompt_bufnr)

	vim.fn.setreg("+", text)
end

-- instead of needing this for a map require('telescope.builtin').buffers()
-- you can use_builtin(function(b) b.buffers() end)
local use_builtin = function(visit)
	return function()
		local builtin = require("telescope.builtin")
		visit(builtin)
	end
end

return {
	-- fuzzy finder
	{
		"nvim-telescope/telescope.nvim",
		cmd = "Telescope",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-telescope/telescope-fzf-native.nvim",
		},
		keys = {
			{ "<leader>b", "<cmd>lua require('telescope.builtin').buffers()<cr>", desc = "Buffers" },
			{ "<leader>f", "<cmd>lua require('telescope.builtin').find_files()<cr>", desc = "Find Files" },

			{
				"<leader>g",
				use_builtin(function(b)
					b.grep_string()
				end),
				desc = "Grep",
			},
			{
				"<leader>gg",
				use_builtin(function(b)
					b.grep_string({ grep_open_files = true })
				end),
				desc = "Grep Buffers",
			},
			{
				"<leader>G",
				use_builtin(function(b)
					b.live_grep()
				end),
				desc = "Live Grep",
			},
			{
				"<leader>GG",
				use_builtin(function(b)
					b.live_grep({ grep_open_files = true })
				end),
				desc = "Live Grep Buffers",
			},

			{
				"<leader>gR",
				function()
					local bufnr = vim.api.nvim_get_current_buf()
					local winnr = vim.fn.bufwinid(bufnr)
					telescope_util.references_filter_tests({ bufnr = bufnr, winnr = winnr })
				end,
				desc = "Find References without Tests",
			},

			{ "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent" },
			{ "<leader>gc", "<Cmd>Telescope git_commits<CR>", desc = "commits" },
			{ "<leader>gs", "<Cmd>Telescope git_status<CR>", desc = "status" },
			{ "<leader>hc", "<cmd>Telescope commands<cr>", desc = "Commands" },
			{ "<leader>hf", "<cmd>Telescope filetypes<cr>", desc = "File Types" },
			{ "<leader>hh", "<cmd>Telescope help_tags<cr>", desc = "Help Pages" },
			{ "<leader>hk", "<cmd>Telescope keymaps<cr>", desc = "Key Maps" },
			{ "<leader>ho", "<cmd>Telescope vim_options<cr>", desc = "Options" },
			{ "<leader>sb", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Buffer" },
			{ "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
			{
				"<leader>ss",
				"<cmd>lua require('telescope.builtin').lsp_dynamic_workspace_symbols()<cr>",
				desc = "Workspace Symbols",
			},
			{ "<leader>,", "<cmd>Telescope buffers show_all_buffers=true<cr>", desc = "Switch Buffer" },
			{ "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
		},

		config = function()
			local telescope = require("telescope")
			local actions = require("telescope.actions")
			-- local layout_strategies = require("telescope.pickers.layout_strategies")

			local options = {
				defaults = {
					sorting_strategy = "ascending",
					layout_strategy = "horizontal",
					layout_config = {
						prompt_position = "top",
						height = 0.9,
						width = 0.9,
					},
					mappings = {
						i = {
							["<C-y>"] = yank_all_entries,
							["<C-p>"] = yank_preview_lines,
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,
						},
						n = {
							["<C-y>"] = yank_all_entries,
							["<C-p>"] = yank_preview_lines,
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,
						},
					},
				},
				extensions = {
					fzf = {
						fuzzy = true, -- false will only do exact matching
						override_generic_sorter = true, -- override the generic sorter
						override_file_sorter = true, -- override the file sorter
						case_mode = "smart_case", -- or "ignore_case" or "respect_case"
					},
				},
				pickers = {
					buffers = {
						mappings = {
							i = {
								["<C-d>"] = require("telescope.actions").delete_buffer,
							},
							n = {
								["<C-d>"] = require("telescope.actions").delete_buffer,
							},
						},
					},
				},
			}

			telescope.setup(options)

			telescope.load_extension("fzf")
		end,
	},

	-- telescope fzf extension which allows for fzf syntax
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make",
	},
}
