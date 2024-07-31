return {
	{
		"godlygeek/tabular",
		lazy = false,
		config = function()
			vim.cmd([[nnoremap <Leader>a= :Tabularize /=<CR>]])
			vim.cmd([[vnoremap <Leader>a= :Tabularize /=<CR>]])
			vim.cmd([[nnoremap <Leader>a: :Tabularize /:\zs/l0r1<CR>]])
			vim.cmd([[vnoremap <Leader>a: :Tabularize /:\zs/l0r1<CR>]])
			vim.cmd([[nnoremap <Leader>a, :Tabularize /,\zs/l0r1<CR>]])
			vim.cmd([[vnoremap <Leader>a, :Tabularize /,\zs/l0r1<CR>]])
			vim.cmd([[nnoremap <Leader>a<Space> :Tabularize / \zs/l0r1<CR>]])
			vim.cmd([[vnoremap <Leader>a<Space> :Tabularize / \zs/l0r1<CR>]])
		end,
	},
}
