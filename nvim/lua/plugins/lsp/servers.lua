---@diagnostic disable: missing-fields

-- Add any servers here together with their settings
---@type lspconfig.options
local servers = {
	bashls = {},
	clangd = {},
	cssls = {},
	eslint = {
		workingDirectory = { mode = "auto" },
	},
	gopls = {},
	tsserver = {},
	html = {},
	jsonls = {},
	pyright = {},
	yamlls = {},
	lua_ls = {
		settings = {
			Lua = {
				diagnostics = {
					globals = { "vim" },
				},
				workspace = {
					checkThirdParty = false,
					library = vim.api.nvim_get_runtime_file("", true),
				},
				completion = {
					callSnippet = "Replace",
				},
			},
		},
	},

	rust_analyzer = {},
}

return servers
