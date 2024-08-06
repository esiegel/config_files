local M = {}

local first_element = function(tbl)
	for _, el in ipairs(tbl) do
		return el
	end
end

local starts_with = function(str, start)
	return string.sub(str, 1, string.len(start)) == start
end

-- Returns all buffer numbers for buffers that are terminals
local find_terminal_bufs = function()
	local bufs = vim.api.nvim_list_bufs()
	local found = {}

	for i, buf in ipairs(bufs) do
		local name = vim.api.nvim_buf_get_name(buf)
		if starts_with(name, "term://") then
			table.insert(found, i)
		end
	end

	return found
end

-- Activates the terminal, or closes it.
-- TODO: figure out how to deal with multiple terms
function M.toggle_term()
	local bufs = find_terminal_bufs()

	if #bufs == 0 then
		local all_bufs = vim.api.nvim_list_bufs()
		local first_buf = first_element(all_bufs)

		if #all_bufs == 1 and vim.api.nvim_buf_get_name(first_buf) == "" then
			print("OPENING" .. first_buf)
			vim.cmd("terminal")
		else
			vim.cmd("vsplit | terminal ++curwin")
		end

		return
	end

	local cur_buf = vim.api.nvim_get_current_buf()

	-- get first terminal buffer
	local term_buf = first_element(bufs)
	local term_win = vim.fn.bufwinid(term_buf)

	if term_win == -1 then
		vim.cmd("vsplit")
		vim.cmd("buffer " .. term_buf)
	else
		local force_close = false
		vim.api.nvim_win_close(term_win, force_close)
	end
end

return M
