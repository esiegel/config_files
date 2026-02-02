return {
	{
		"jbyuki/one-small-step-for-vimkind",
		dependencies = { "mfussenegger/nvim-dap" },
		lazy = false,
		config = function()
			local dap = require("dap")

			dap.adapters.nlua = function(callback, config)
				callback({
					type = "server",
					host = config.host or "127.0.0.1",
					port = config.port or 8086,
				})
			end

			dap.configurations.lua = dap.configurations.lua or {}
			table.insert(dap.configurations.lua, {
				type = "nlua",
				request = "attach",
				name = "Attach to running Neovim",
				host = "127.0.0.1",
				port = 8086,
			})
		end,
	},
}
