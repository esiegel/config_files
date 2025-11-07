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

return M
