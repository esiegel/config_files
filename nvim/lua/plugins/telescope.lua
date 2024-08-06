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
					-- b.grep_string({ only_sort_text = true })
					b.grep_string()
				end),
				desc = "Grep",
			},

			{ "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
			{ "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent" },
			{ "<leader>gc", "<Cmd>Telescope git_commits<CR>", desc = "commits" },
			{ "<leader>gs", "<Cmd>Telescope git_status<CR>", desc = "status" },
			{ "<leader>ha", "<cmd>Telescope autocommands<cr>", desc = "Auto Commands" },
			{ "<leader>hc", "<cmd>Telescope commands<cr>", desc = "Commands" },
			{ "<leader>hf", "<cmd>Telescope filetypes<cr>", desc = "File Types" },
			{ "<leader>hh", "<cmd>Telescope help_tags<cr>", desc = "Help Pages" },
			{ "<leader>hk", "<cmd>Telescope keymaps<cr>", desc = "Key Maps" },
			{ "<leader>hm", "<cmd>Telescope man_pages<cr>", desc = "Man Pages" },
			{ "<leader>ho", "<cmd>Telescope vim_options<cr>", desc = "Options" },
			{ "<leader>hs", "<cmd>Telescope highlights<cr>", desc = "Search Highlight Groups" },
			{ "<leader>ht", "<cmd>Telescope builtin<cr>", desc = "Telescope" },
			{ "<leader>sb", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Buffer" },
			{ "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Command History" },
			-- { "<leader>sg", util.telescope("live_grep"), desc = "Grep" },
			{ "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
			{ "<leader>,", "<cmd>Telescope buffers show_all_buffers=true<cr>", desc = "Switch Buffer" },
			{ "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
			-- {
			-- "<leader>ss",
			-- util.telescope("lsp_document_symbols", {
			-- symbols = {
			-- "Class",
			-- "Function",
			-- "Method",
			-- "Constructor",
			-- "Interface",
			-- "Module",
			-- "Struct",
			-- "Trait",
			-- "Field",
			-- "Property",
			-- },
			-- }),
			-- desc = "Goto Symbol",
			-- },
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
