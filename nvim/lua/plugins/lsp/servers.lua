---@diagnostic disable: missing-fields

-- Add any servers here together with their settings
---@type lspconfig.options
local servers = {
	bashls = {},
	clangd = {},
	cssls = {},
	gopls = {
		-- Prevent gopls from attaching to non-file buffers (e.g. fugitive://),
		-- which gopls rejects. Not calling on_dir aborts the client start.
		root_dir = function(bufnr, on_dir)
			local uri = vim.uri_from_bufnr(bufnr)
			if not uri:match("^file://") then
				return
			end
			local root = vim.fs.root(bufnr, { "go.work", "go.mod", ".git" })
			on_dir(root or vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":h"))
		end,
	},
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
