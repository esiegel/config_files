local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.runtimepath:prepend(vim.env.LAZY or lazypath)

require("lazy").setup({
	spec = "plugins", -- search lua/plugins/* for plugin config
	defaults = {
		lazy = true,
		version = "*",
		checker = {
			notify = false, -- get a notification when new updates are found
		},
	},
	install = { colorscheme = { "catppuccin-mocha" } },
	checker = { enabled = true },
})
