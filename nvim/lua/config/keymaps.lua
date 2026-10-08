local terminal = require("util.terminal")

-- Functional wrapper for mapping custom keybindings
local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end

-- remap escape. Mode is "" or "!" as that force nvo
map("", "<C-j>", "<Esc>")
map("!", "<C-j>", "<Esc>")

-- removes highlighting from search after space
map("n", "<Space>", ":nohlsearch<Bar>echo<cr>", { silent = true })

-- map gp to select recently pasted text
-- fancier `[v`]
-- http://vim.wikia.com/wiki/Selecting_your_pasted_text
vim.cmd([[ nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]' ]])

-- change to next and previous buffers
map("n", "<C-h>", ":bp<cr>", { silent = true })
map("n", "<C-l>", ":bn<cr>", { silent = true })

-- open up config
map("n", "<leader>ev", "<cmd>:vs /Users/eric.siegel/.config/nvim/init.lua <cr>")

-- Use emacs bindings for command mode.
map("c", "<C-A>", "<Home>") --   start     of          line
map("c", "<C-B>", "<Left>") --   back      one         character
map("c", "<C-D>", "<Del>") --   delete    character   under          cursor
map("c", "<C-E>", "<End>") --   end       of          line
map("c", "<C-F>", "<Right>") --   forward   one         character
map("c", "<C-N>", "<Down>") --   recall    newer       command-line
map("c", "<C-P>", "<Up>") --   recall    previous    (older)        command-line
map("c", "<Esc><C-B>", "<S-Left>") --   back      one         word
map("c", "<Esc><C-F>", "<S-Right>") --   forward   one         word

-- escape to terminal normal
map("t", "<C-j>", "<C-\\><C-n>")
map("n", "<leader>z", terminal.toggle_term)

-- terminal buffer picker: open telescope buffer list and insert selected buffer's filepath
map("t", "<C-x><C-b>", function()
	local term_buf = vim.api.nvim_get_current_buf()
	local term_chan = vim.b[term_buf].terminal_job_id
	if not term_chan then
		return
	end

	vim.cmd("stopinsert")

	require("telescope.builtin").buffers({
		attach_mappings = function(prompt_bufnr, _)
			local actions = require("telescope.actions")
			local action_state = require("telescope.actions.state")

			actions.select_default:replace(function()
				local selection = action_state.get_selected_entry()
				actions.close(prompt_bufnr)

				if selection then
					local filepath = vim.api.nvim_buf_get_name(selection.bufnr)
					if filepath ~= "" then
						vim.api.nvim_chan_send(term_chan, filepath)
					end
				end

				-- reenter insert mode, but schedule this to give telescope time to close
				vim.schedule(function()
					vim.cmd("startinsert")
				end)
			end)

			return true
		end,
	})
end)

-- terminal buffer picker: open telescope buffer list and insert files filepath
vim.keymap.set("t", "<C-x><C-f>", function()
	local term_buf = vim.api.nvim_get_current_buf()
	local term_chan = vim.b[term_buf].terminal_job_id
	if not term_chan then
		return
	end

	-- Exit terminal insert mode so Telescope can capture input cleanly
	vim.cmd("stopinsert")

	-- Use 'find_files' (standard Telescope builtin)
	require("telescope.builtin").find_files({
		attach_mappings = function(prompt_bufnr, _)
			local actions = require("telescope.actions")
			local action_state = require("telescope.actions.state")

			actions.select_default:replace(function()
				local selection = action_state.get_selected_entry()
				actions.close(prompt_bufnr)

				if selection and selection.value then
					-- selection.value contains the relative or absolute path string
					local filepath = selection.value

					-- Optional: Turn relative paths into absolute paths if needed:
					-- filepath = vim.fn.fnamemodify(filepath, ":p")

					-- Send the path to the terminal channel
					-- Added a space " " at the end so you can keep typing arguments
					vim.api.nvim_chan_send(term_chan, filepath .. " ")
				end

				-- Re-enter terminal insert mode safely
				vim.schedule(function()
					vim.cmd("startinsert")
				end)
			end)

			return true
		end,
	})
end)

-- change to next quickfix error
map("n", "<leader>h", function()
	vim.cmd("cprev")
end, { silent = true })
map("n", "<leader>l", function()
	vim.cmd("cnext")
end, { silent = true })

-- Commenting
map("n", "<leader>c<Space>", "<cmd>:normal gcc<CR>")
map("x", "<leader>c<Space>", "<cmd>:normal gcc<CR>")
