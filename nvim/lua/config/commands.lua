-- Define the Redir function in Lua
local function redir(command)
	vim.cmd("redir @*")
	vim.cmd("silent " .. command)
	vim.cmd("redir END")
end

-- Create the :R command that takes arguments and calls the Lua redir function
vim.api.nvim_create_user_command("R", function(opts)
	redir(opts.args)
end, { nargs = "+" })

vim.api.nvim_create_user_command("FormatDisable", function()
	vim.b.disable_autoformat = true
	vim.g.disable_autoformat = true
end, {
	desc = "Disable autoformat-on-save",
})

vim.api.nvim_create_user_command("FormatEnable", function()
	vim.b.disable_autoformat = false
	vim.g.disable_autoformat = false
end, {
	desc = "Enable autoformat-on-save",
})
