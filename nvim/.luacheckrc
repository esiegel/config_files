-- Allow global `vim` (Neovim API)
globals = {
	"vim",

	-- Lazy.nvim / packer.nvim plugin specs
	"use", -- for packer
	"lazy", -- for lazy.nvim configs if used

	-- LuaSnip or cmp-related global calls (optional)
	"cmp",
	"ls", -- LuaSnip shorthand if you use it
	"snippet", -- general snippet globals

	-- Busted test framework (for plugin/test devs)
	"describe",
	"it",
	"before_each",
	"after_each",
	"pending",
	"setup",
	"teardown",

	-- Telescope pickers or config helpers
	"require_picker",

	-- Utility globals you might define manually
	"P", -- for print/debug shortcuts
	"R", -- for reloaders like R('mod.name')
}

-- Exclude bundled plugins or generated files (adjust as needed)
exclude_files = {
	"lua/**/packer_compiled.lua",
	"lua/**/lazy-lock.json",
}

-- Optional: ignore unused `args` like `_` or `_G`
unused_args = false
ignore = {
	"111", -- ignore setting non-standard globals (optional if using `globals` above)
}
