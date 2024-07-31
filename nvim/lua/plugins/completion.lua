return {
	-- auto completion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		-- dependencies = {
		-- 	"neovim/nvim-lspconfig",
		-- 	"hrsh7th/cmp-nvim-lsp",
		-- 	"hrsh7th/cmp-buffer",
		-- 	"hrsh7th/cmp-path",
		-- 	"hrsh7th/cmp-emoji",
		-- 	"saadparwaiz1/cmp_luasnip",
		-- },
		dependencies = {
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
			"onsails/lspkind.nvim",
			"saadparwaiz1/cmp_luasnip",
			{ "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
		},
		config = function()
			local cmp = require("cmp")

			cmp.setup({
				completion = {
					autocomplete = false, -- only complete when explicitly invoking
				},

				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete({}),
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
				}),

				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "luasnip" },
					{ name = "buffer" },
					{ name = "path" },
					{ name = "emoji" },
				}),

				formatting = {
					format = function(_, item)
						local icons = require("config.icons")

						if icons[item.kind] then
							item.kind = icons[item.kind] .. item.kind
						end

						return item
					end,
				},
				experimental = {
					ghost_text = {
						hl_group = "LspCodeLens",
					},
				},

				-- Enable luasnip to handle snippet expansion for nvim-cmp
				snippet = {
					expand = function(args)
						vim.snippet.expand(args.body)
					end,
				},
			})

			-- Set up lspconfig.
			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			require("lspconfig")["rust_analyzer"].setup({
				capabilities = capabilities,
			})
		end,
	},
}
