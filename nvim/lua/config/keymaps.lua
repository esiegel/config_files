-- Functional wrapper for mapping custom keybindings
local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
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
vim.cmd([[cnoremap <C-A> <Home>]]) -- start of line
vim.cmd([[cnoremap <C-B> <Left>]]) -- back one character
vim.cmd([[cnoremap <C-D> <Del>]]) -- delete character under cursor
vim.cmd([[cnoremap <C-E> <End>]]) -- end of line
vim.cmd([[cnoremap <C-F> <Right>]]) -- forward one character
vim.cmd([[cnoremap <C-N> <Down>]]) -- recall newer command-line
vim.cmd([[cnoremap <C-P> <Up>]]) -- recall previous (older) command-line
vim.cmd([[cnoremap <Esc><C-B>	<S-Left>]]) -- back one word
vim.cmd([[cnoremap <Esc><C-F>	<S-Right>]]) -- forward one word

-- escape to terminal normal
vim.cmd([[tnoremap <C-j> <C-\><C-n>]])

-- change to next quickfix error
-- map("n", "<leader>h", function() vim.cmd("cprev") end, { silent = true })
-- map("n", "<leader>l", function() vim.cmd("cnext") end, { silent = true })
