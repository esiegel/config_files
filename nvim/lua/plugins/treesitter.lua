return {
	{
		"nvim-treesitter/nvim-treesitter",
		branch = "main",
		lazy = false,
		build = ":TSUpdate",
		init = function()
			-- The main branch stores queries under runtime/queries/ rather than queries/ at the
			-- plugin root. Lazy only adds the plugin root to rtp, so without this prepend Neovim
			-- cannot find highlight queries for bundled parsers (anything not explicitly TSInstalled).
			vim.opt.rtp:prepend(vim.fn.stdpath("data") .. "/lazy/nvim-treesitter/runtime")
		end,
		config = function()
			require("nvim-treesitter").install({ "c", "go", "lua", "rust", "typescript" })

			vim.api.nvim_create_autocmd("FileType", {
				callback = function(args)
					local buf = args.buf

					local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
					if ok and stats and stats.size > 100 * 1024 then
						return
					end

					pcall(vim.treesitter.start, buf)
				end,
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		branch = "main",
		config = function()
			vim.keymap.set("n", "<leader>a", function()
				require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
			end)
			vim.keymap.set("n", "<leader>A", function()
				require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.inner")
			end)
		end,
	},
}
