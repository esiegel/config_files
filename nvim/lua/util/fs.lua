local M = {}

-- Checks if a file exists
-- @return bool
function M.file_exists(filepath)
	local stat = vim.loop.fs_stat(filepath)
	return stat ~= nil
end

return M
