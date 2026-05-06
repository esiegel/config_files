local M = {}

-- Checks if a file exists
-- @return bool
function M.file_exists(filepath)
	local stat = vim.loop.fs_stat(filepath)
	return stat ~= nil
end

function M.get_parent_path(filepath)
	return vim.fn.fnamemodify(filepath, ":h")
end

-- Walks up from `path` to find `filename`, returns the directory containing it or nil
function M.find_upward(filename, path)
	local found = vim.fs.find(filename, { path = path, upward = true })[1]
	if found then
		return vim.fn.fnamemodify(found, ":h")
	end
end

return M
