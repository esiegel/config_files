source ~/.vim/vimrc

" neovim python for running old python libs
let g:python_host_prog='/Users/eric/.virtualenvs/neovim/bin/python'

"""""""""""""""""""""""""TERMINAL"""""""""""""""""""""""""""""""
" ctrl J to escape
tnoremap <C-j> <C-\><C-n>

" launch terminal
nnoremap <Leader>tt :Ttoggle

" sends current file or selection
nnoremap <Leader>ts :TREPLSendFile
xnoremap <Leader>ts :TREPLSendFile

let g:neoterm_position = 'vertical'
let g:neoterm_autoinsert = 1

"""""""""""""""""""""""""NEOMAKE"""""""""""""""""""""""""""""""
autocmd! BufWritePost * Neomake

let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_ruby_enabled_makers = ['rubocop']
let g:neomake_coffeescript_enabled_makers = ['coffeelint']
