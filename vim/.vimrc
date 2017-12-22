" Configure Vundle
set nocompatible
filetype off
let mapleader=","
set shell=/bin/bash
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'othree/eregex.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Townk/vim-autoclose'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'MaxMEllon/vim-jsx-pretty'
Plugin 'tpope/vim-endwise'
Plugin 'airblade/vim-gitgutter'
Plugin 'rhysd/vim-gfm-syntax'
Plugin 'jkramer/vim-checkbox'
Plugin 'ecomba/vim-ruby-refactoring'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-fugitive'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'jgdavey/vim-blockle'
Plugin 'hashivim/vim-terraform'
Plugin 'leafgarland/typescript-vim'
Plugin 'elixir-editors/vim-elixir'
Plugin 'easymotion/vim-easymotion'
call vundle#end()
filetype plugin indent on

" Configure editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" CtrlP config
" Use git for file listings if available, which means it respects
" .gitignore
let g:ctrlp_user_command = ['.git', 'git ls-files %s -co --exclude-standard']

" Terraform stuff
let g:terraform_align = 1
let g:terraform_fmt_on_save = 1
autocmd FileType terraform setlocal commentstring=#%s

" Aliases
nnoremap <Leader>o :CtrlPMixed<CR>
nnoremap <Leader>s :w<CR>
nnoremap <c-N> :GitGutterNextHunk<CR>
nnoremap <c-P> :GitGutterPrevHunk<CR>
nnoremap <c-U> :GitGutterUndoHunk<CR>

" Keep blocks selected when indenting
vnoremap < <gv
vnoremap > >gv

vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P
imap jk <Esc>

syntax on
set autoindent
set number
set expandtab
set tabstop=2
set mouse=a " Full mouse support
set hidden " Don't force buffers to be saved
set listchars=tab:>-,trail:·
set list
match ErrorMsg '\%>80v.\+' " Highlight long lines
match ErrorMsg '\s\+$'     " Highlight trailing whitespace

set directory=~/.local/vim/swapfiles//
set backupdir=~/.local/vim/backups//
