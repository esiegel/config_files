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
