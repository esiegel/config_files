local M = {}

-- Trim whitespace from both sides
local trim_whitespace = function(s)
	return s:match("^%s*(.-)%s*$")
end

-- Reverses the elements in the list like table
local reverse_table = function(tbl)
	local reversed = {}
	for i = #tbl, 1, -1 do
		table.insert(reversed, tbl[i])
	end
	return reversed
end

-- processes a stacktrace and adds it to the quickfix
--- @param stacktrace string[]
function M.ts_stacktrace_to_quickfix(stacktrace)
	local qf_entries = {}

	-- at Object.<anonymous> (/SOME/PATH/TO/CODE/FILE.ts:303:26)
	local pattern = "%(?([^: ]+):(%d+):(%d+)%)?$"

	for _, line in pairs(stacktrace) do
		line = trim_whitespace(line)
		local file, line_num, col_num = line:match(pattern)

		if file and line_num and col_num then
			-- Add an entry to the quickfix list
			table.insert(qf_entries, {
				filename = file,
				lnum = tonumber(line_num),
				col = tonumber(col_num),
				text = line,
			})
		end
	end

	if #qf_entries > 0 then
		local reversed_qf_entries = reverse_table(qf_entries)
		vim.fn.setqflist(reversed_qf_entries, "r")
		vim.cmd("copen")
	end
end

-- Stacktrace visually selected to quickfix
function M.ts_stacktrace_to_quickfix_visual()
	local start_pos = vim.fn.getpos("'<")
	local end_pos = vim.fn.getpos("'>")
	local lines = vim.fn.getline(start_pos[2], end_pos[2])

	---@diagnostic disable-next-line: param-type-mismatch
	M.ts_stacktrace_to_quickfix(lines)
end

return M
