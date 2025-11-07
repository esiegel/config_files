local fs = require("util.fs")

-- On Lsp server attach
---@param on_attach fun(client, buffer)
function on_attach(on_attach)
	vim.api.nvim_create_autocmd("LspAttach", {
		callback = function(args)
			local buffer = args.buf
			local client = vim.lsp.get_client_by_id(args.data.client_id)
			on_attach(client, buffer)
		end,
	})
end

return {
	-- lspconfig
	{
		"neovim/nvim-lspconfig",
		event = "BufReadPre",
		dependencies = {
			{ "folke/neoconf.nvim", cmd = "Neoconf", config = true },
			{ "folke/neodev.nvim", config = true },
			{ "mason-org/mason.nvim", version = "^1.0.0" },
			{ "mason-org/mason-lspconfig.nvim", version = "^1.0.0", config = { automatic_installation = true } },
			"hrsh7th/cmp-nvim-lsp",
		},
		---@type lspconfig.options
		servers = nil,
		config = function(plugin)
			-- setup formatting and keymaps
			on_attach(function(client, buffer)
				-- TODO: should i reenable formatting from the lsp or use conform?
				-- require("plugins.lsp.format").on_attach(client, buffer)
				--
				require("plugins.lsp.keymaps").on_attach(client, buffer)
			end)

			-- diagnostics
			for name, icon in pairs(require("config.icons").diagnostics) do
				name = "DiagnosticSign" .. name
				vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
			end
			vim.diagnostic.config({
				underline = true,
				update_in_insert = false,
				virtual_text = { spacing = 4, prefix = "●" },
				severity_sort = true,
			})

			-- lspconfig
			local servers = plugin.servers or require("plugins.lsp.servers")
			local capabilities =
				require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

			local global_defaults = {
				capabilities = capabilities,
			}

			-- Update the configuration for all LSP clients.
			for name, opts in pairs(servers) do
				vim.lsp.config(name, vim.tbl_deep_extend("force", {}, global_defaults, opts or {}))
			end

			-- Auto-starts LSP when a buffer is opened, based on the |lsp-config|
			vim.lsp.enable(vim.tbl_keys(servers))
		end,
	},

	-- formatters
	-- {
	-- 	"nvimtools/none-ls.nvim",
	-- 	event = "BufReadPre",
	-- 	dependencies = {
	-- 		"mason.nvim",
	-- 		"nvimtools/none-ls-extras.nvim",
	-- 	},
	-- 	config = function()
	-- 		local nls = require("null-ls")
	--
	-- 		nls.setup({
	-- 			sources = {
	-- 				require("none-ls.diagnostics.flake8"),
	-- 				nls.builtins.formatting.black,
	-- 				nls.builtins.formatting.prettier,
	-- 				nls.builtins.formatting.stylua,
	-- 			},
	-- 		})
	-- 	end,
	-- },

	{
		"mfussenegger/nvim-lint",
		event = {
			"BufReadPre",
			"BufNewFile",
		},
		config = function()
			local lint = require("lint")

			local eslint = lint.linters.eslint
			eslint.args = {
				"--format",
				"json",
				"--stdin",
				"--stdin-filename",
				function()
					if fs.file_exists("packages/eslint-config/index.js") then
						return "--config packages/eslint-config/index.js"
					elseif fs.file_exists(".eslintrc.js") then
						return "--config .eslintrc.js"
					elseif fs.file_exists(".eslintrc.cjs") then
						return "--config .eslintrc.cjs"
					else
						return ""
					end
				end,
				function()
					return vim.api.nvim_buf_get_name(0)
				end,
			}

			-- customize luacheck so that it will use the luacheckrc
			local luacheck = lint.linters.luacheck
			luacheck.args = {
				"--default-config",
				vim.fn.expand("~/.config/nvim/.luacheckrc"),
				"--formatter",
				"plain",
				"--codes",
				"--ranges",
				"-",
			}

			lint.linters_by_ft = {
				css = { "stylelint" },
				javascript = { "eslint" },
				javascriptreact = { "eslint" },
				go = { "golangcilint" },
				lua = { "luacheck" },
				python = { "ruff" },
				typescript = { "eslint" },
				typescriptreact = { "eslint" },
			}

			local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

			vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
				group = lint_augroup,
				callback = function()
					lint.try_lint(nil, { ignore_errors = true })
				end,
			})
		end,
	},

	{
		"stevearc/conform.nvim",
		event = { "BufReadPre", "BufNewFile" },
		cmd = { "ConformInfo" },

		-- This comments provides type hinting with LuaLS
		---@module "conform"
		---@type conform.setupOpts
		opts = {
			formatters_by_ft = {
				lua = { "stylua" },
				python = { "isort", "black" },

				-- web
				css = { "prettierd", "prettier", stop_after_first = true },
				html = { "prettierd", "prettier", stop_after_first = true },
				javascript = { "prettierd", "prettier", stop_after_first = true },
				javascriptreact = { "prettierd", "prettier", stop_after_first = true },
				json = { "prettierd", "prettier", stop_after_first = true },
				typescript = { "prettierd", "prettier", stop_after_first = true },
				typescriptreact = { "prettierd", "prettier", stop_after_first = true },
			},
			-- Set default options
			default_format_opts = {
				lsp_format = "fallback",
			},
			format_on_save = function(bufnr)
				-- Disable autoformat on certain filetypes
				local ignore_filetypes = {}
				if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
					return
				end

				-- Disable with a global or buffer-local variable
				if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
					return
				end

				-- Disable autoformat for files in a certain path
				local bufname = vim.api.nvim_buf_get_name(bufnr)
				if bufname:match("/node_modules/") then
					return
				end

				-- ...additional logic...
				return { timeout_ms = 500, lsp_format = "fallback" }
			end,
			-- Customize formatters
			formatters = {
				shfmt = {
					prepend_args = { "-i", "3" },
				},
			},
		},
		init = function()
			-- If you want the formatexpr, here is the place to set it
			vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
		end,
	},

	-- cmdline tools and lsp servers
	{

		"mason-org/mason.nvim",
		version = "^1.0.0",
		cmd = "Mason",
		keys = { { "<leader>cm", "<cmd>Mason<cr>", desc = "Mason" } },
		ensure_installed = {
			"black",
			"flake8",
			"prettier",
			"rust-analyzer",
			"shellcheck",
			"shfmt",
			"stylua",
		},
		config = function(plugin)
			require("mason").setup()
			local mr = require("mason-registry")
			for _, tool in ipairs(plugin.ensure_installed) do
				local p = mr.get_package(tool)
				if not p:is_installed() then
					p:install()
				end
			end
		end,
	},
}
