---@diagnostic disable: missing-fields

-- Add any servers here together with their settings
---@type lspconfig.options
local servers = {
	bashls = {},
	clangd = {},
	cssls = {},
	gopls = {},
	html = {},
	jsonls = {},
	yamlls = {},

	-- JS/TS
	ts_ls = {},
	eslint = {
		workingDirectory = { mode = "auto" },
	},

	-- prettier = {
	--   workingDirectory = { mode = "auto" },
	-- },

	-- lua
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

	-- rust
	rust_analyzer = {
		settings = {
			["rust-analyzer"] = {
				inlayHints = {
					enable = false,
				},
			},
		},
	},

	-- python
	pyright = {},
	ruff = {},

	-- R lang
	r_language_server = {},
}

return servers
