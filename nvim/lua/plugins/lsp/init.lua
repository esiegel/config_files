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
