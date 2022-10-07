set number
set nocompatible
set hidden
set nocp
filetype plugin on
call plug#begin()
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
call plug#end()
imap <F5> <Esc>:w<CR>:!clear;python %<CR>
