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

-- See if a string s matches a FZF query
-- @param str string
-- @param query string
function M.fzf_match(str, query)
	if not query or query == "" then
		return true
	end

	for _, part in ipairs(vim.split(query, "%s+")) do
		if part ~= "" then
			local is_neg = part:sub(1, 1) == "!"
			local term = is_neg and part:sub(2) or part

			if term ~= "" then
				local found = str:find(term, 1, true) ~= nil

				if (is_neg and found) or (not is_neg and not found) then
					return false
				end
			end
		end
	end

	return true
end

return M
