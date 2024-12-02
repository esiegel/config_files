local M = {}

-- Remove a traling newline
-- Returns a string without the ending newline
-- @return string
function M.strip_trailing_newline(s)
	if string.sub(s, -1) == "\n" then
		return string.sub(s, 0, -2)
	end

	return s
end

return M
