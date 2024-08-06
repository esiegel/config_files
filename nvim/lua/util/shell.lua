local M = {}

-- Runs a shell command
-- Returns stdout, err_string
function M.cmd(command)
	local handle = io.popen(command)
	if not handle then
		return nil, "Failed to run command"
	end

	local result = handle:read("*a") -- *a reads the entire output
	handle:close()

	if not result then
		return nil, "Failed to read command output"
	end

	return result
end

return M
