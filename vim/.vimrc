" Configure Vundle
set nocompatible
filetype off
let mapleader="\<Space>"
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'othree/eregex.vim'
Plugin 'ctrlpvim/ctrlp.vim'
call vundle#end()
filetype plugin indent on

" Configure Powerline
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" Ensure Powerline is always visible
set laststatus=2

" Configure editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" CtrlP config
" Use git for file listings if available, which means it respects
" .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" Aliases
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>s :w<CR>
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

syntax on
set autoindent
set number
set expandtab
set tabstop=2
set mouse=a " Full mouse support
