return {
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		lazy = true,
		cmd = { "CopilotChat", "CopilotChatOpen", "CopilotChatToggle" },
		dependencies = {
			{ "github/copilot.vim" }, -- or zbirenbaum/copilot.lua
			{ "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
		},
		build = "make tiktoken", -- Only on MacOS or Linux
		opts = {},
	},

	-- Copilot itself: start disabled + load on :Copilot
	{
		"github/copilot.vim",
		lazy = true,
		cmd = { "Copilot" },
		init = function()
			vim.g.copilot_enabled = 0
		end,
	},
}
