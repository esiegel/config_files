local terminal = require("util.terminal")

-- Functional wrapper for mapping custom keybindings
local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end

-- remap escape. Mode is "" or "!" as that force nvo
map("", "<C-j>", "<Esc>")
map("!", "<C-j>", "<Esc>")

-- removes highlighting from search after space
map("n", "<Space>", ":nohlsearch<Bar>echo<cr>", { silent = true })

-- map gp to select recently pasted text
-- fancier `[v`]
-- http://vim.wikia.com/wiki/Selecting_your_pasted_text
vim.cmd([[ nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]' ]])

-- change to next and previous buffers
map("n", "<C-h>", ":bp<cr>", { silent = true })
map("n", "<C-l>", ":bn<cr>", { silent = true })

-- open up config
map("n", "<leader>ev", "<cmd>:vs /Users/eric.siegel/.config/nvim/init.lua <cr>")

-- Use emacs bindings for command mode.
map("c", "<C-A>", "<Home>") --   start     of          line
map("c", "<C-B>", "<Left>") --   back      one         character
map("c", "<C-D>", "<Del>") --   delete    character   under          cursor
map("c", "<C-E>", "<End>") --   end       of          line
map("c", "<C-F>", "<Right>") --   forward   one         character
map("c", "<C-N>", "<Down>") --   recall    newer       command-line
map("c", "<C-P>", "<Up>") --   recall    previous    (older)        command-line
map("c", "<Esc><C-B>", "<S-Left>") --   back      one         word
map("c", "<Esc><C-F>", "<S-Right>") --   forward   one         word

-- escape to terminal normal
map("t", "<C-j>", "<C-\\><C-n>")
map("n", "<leader>z", terminal.toggle_term)

-- change to next quickfix error
map("n", "<leader>h", function()
	vim.cmd("cprev")
end, { silent = true })
map("n", "<leader>l", function()
	vim.cmd("cnext")
end, { silent = true })

-- Commenting
map("n", "<leader>c<Space>", "<cmd>:normal gcc<CR>")
map("x", "<leader>c<Space>", "<cmd>:normal gcc<CR>")
