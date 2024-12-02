local str = require("util.str")

local M = {}

-- Runs a shell command
-- Returns the command output string, and an error string
-- @return string, string|nil
function M.cmd(command)
	local handle = io.popen(command)
	if not handle then
		return "", "Failed to run command"
	end

	local result = handle:read("*a") -- *a reads the entire output
	handle:close()

	if not result then
		return "", "Failed to read command output"
	end

	return str.strip_trailing_newline(result), nil
end

-- Runs a shell command silently
-- Returns the command output string, and an error string
-- @return string, string|nil
function M.cmd_silent(command)
	return M.cmd(command .. " 2>/dev/null")
end

return M
