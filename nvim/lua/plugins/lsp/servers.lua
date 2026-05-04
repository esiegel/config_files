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
	ts_ls = {
		single_file_support = true,
	},
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
				hint = {
					enable = false,
					semicolon = "Disable",
				},
			},
		},
	},

	-- rust
	rust_analyzer = {
		settings = {
			["rust-analyzer"] = {
				inlayHints = {
					typeHints = { enable = false },
					parameterHints = { enable = false },
					chainingHints = { enable = false },
					closingBraceHints = { enable = false },
				},
				cargo = { features = "all" },
				checkOnSave = true,
				check = { command = "clippy" },
				imports = {
					group = {
						enable = false,
					},
				},
				completion = {
					postfix = {
						enable = false,
					},
				},
			},
		},
	},

	-- python
	pyright = {},
	ruff = {},

	-- R lang
	r_language_server = {},

	-- terraform infranstructure lang
	terraformls = {},
}

return servers
