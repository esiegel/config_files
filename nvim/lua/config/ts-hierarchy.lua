local M = {}

--- Traces a symbol hierarchy and loads the output into the Neovim Quickfix list.
---@param opts table|nil Configuration options:
---   - symbol (string|nil): Target symbol name. Defaults to word under cursor.
---   - file (string|nil): Target file path. Defaults to current buffer path.
---   - extra_args (table|nil): Array of additional CLI flags, e.g. { "--callees", "--no-test" }
function M.trace(opts)
	opts = opts or {}
	local file = opts.file or vim.fn.expand("%")
	local symbol = opts.symbol or vim.fn.expand("<cword>")

	if file == "" then
		vim.notify("ts-hierarchy: Current buffer has no file path", vim.log.levels.WARN)
		return
	end

	if symbol == "" then
		vim.notify("ts-hierarchy: No symbol specified or found under cursor", vim.log.levels.WARN)
		return
	end

	-- Build command array
	local cmd = { "ts-hierarchy", "-f", file, "-n", symbol, "--vim" }

	if opts.extra_args then
		for _, arg in ipairs(opts.extra_args) do
			table.insert(cmd, arg)
		end
	end

	-- Execute synchronously and capture output lines
	local lines = vim.fn.systemlist(cmd)

	if vim.v.shell_error ~= 0 then
		local err_msg = table.concat(lines, "\n")
		vim.notify("ts-hierarchy error:\n" .. err_msg, vim.log.levels.ERROR)
		return
	end

	if #lines == 0 then
		vim.notify("ts-hierarchy: No callers or callees found for '" .. symbol .. "'", vim.log.levels.INFO)
		return
	end

	-- Populate Quickfix list and open Quickfix window
	vim.fn.setqflist({}, "r", {
		title = string.format("ts-hierarchy: %s (%s)", symbol, vim.fn.fnamemodify(file, ":t")),
		lines = lines,
	})

	vim.cmd("copen")
end

-- Create keymaps
vim.keymap.set("n", "<leader>hc", function()
	M.trace()
end, { desc = "Trace Upstream Callers (Quickfix)" })

vim.keymap.set("n", "<leader>hd", function()
	M.trace({ extra_args = { "--callees" } })
end, { desc = "Trace Downstream Callees (Quickfix)" })

vim.keymap.set("n", "<leader>hg", function()
	M.trace({ extra_args = { "--changed-only" } })
end, { desc = "Trace Callers in Modified Git Files" })

return M
