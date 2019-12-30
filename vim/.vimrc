" Configure Vundle
set nocompatible
filetype off
let mapleader=","
set shell=/bin/bash
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=/usr/local/opt/fzf
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'w0rp/ale'
Plugin 'ElmCast/elm-vim'
Plugin 'othree/eregex.vim'
Plugin 'Townk/vim-autoclose'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'MaxMEllon/vim-jsx-pretty'
Plugin 'tpope/vim-endwise'
Plugin 'rhysd/vim-gfm-syntax'
Plugin 'jkramer/vim-checkbox'
Plugin 'ecomba/vim-ruby-refactoring'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-fugitive'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'jgdavey/vim-blockle'
Plugin 'leafgarland/typescript-vim'
Plugin 'elixir-editors/vim-elixir'
Plugin 'easymotion/vim-easymotion'
Plugin 'benmills/vimux'
Plugin 'rust-lang/rust.vim'
Plugin 'junegunn/fzf.vim'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'burnettk/vim-angular'
call vundle#end()
filetype plugin indent on

" Configure editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" ALE config
let g:ale_open_list = 1
let g:ale_fix_on_save = 1

let g:ale_fixers = {
\      'ruby': ['rubocop', 'remove_trailing_lines', 'trim_whitespace']
\}

set diffopt+=vertical

" Aliases
nnoremap <Leader>o :GFiles<CR>
nnoremap <Leader>i :Buffers<CR>
nnoremap <Leader>u :BLines<CR>

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

" Status line
set laststatus=2
set statusline=%f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %P%)
set statusline+=%{FugitiveStatusline()}

" Vimux stuff
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vz :call VimuxZoomRunner()<CR>
map <Leader>vq :VimuxCloseRunner<CR>

" Send current selection to tmux pane
function! VimuxSlime()
  call VimuxOpenRunner()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
endfunction

vmap <Leader>vs "vy :call VimuxSlime()<CR>

" Run focused spec
function! VimuxSpec()
  let line=line('.')
  call VimuxOpenRunner()
  call VimuxSendText('rspec ' . expand('%') . ':' . line('.') . "\n")
endfunction

map <Leader>vr :call VimuxSpec()<CR>
map <Leader>vf :call VimuxSendText('rspec --next-failure' . "\n")<CR>
