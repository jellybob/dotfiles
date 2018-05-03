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
Plugin 'benmills/vimux'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'junegunn/fzf.vim'
Plugin 'Yggdroot/indentLine'
call vundle#end()
filetype plugin indent on

" Configure editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" CtrlP config
" Use git for file listings if available, which means it respects
" .gitignore
let g:ctrlp_user_command = ['.git', 'git ls-files %s -co --exclude-standard']

" Syntastic config
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_quiet_messages = {}

" Terraform stuff
let g:terraform_align = 1
let g:terraform_fmt_on_save = 1
autocmd FileType terraform setlocal commentstring=#%s

" Rust stuff
let g:rustfmt_autosave = 1
let g:syntastic_rust_checkers = ['cargo', 'rust']

" Javascript
let g:syntastic_javascript_checkers = ['eslint']

" Ruby
let g:syntastic_ruby_checkers = ['mri', 'rubocop']

" Aliases
nnoremap <Leader>o :GFiles<CR>
nnoremap <Leader>i :Buffers<CR>
nnoremap <Leader>u :BLines<CR>
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

map <Leader>s :SplitjoinSplit<CR>
map <Leader>j :SplitjoinJoin<CR>

" Vimux stuff
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vz :call VimuxZoomRunner()<CR>
map <Leader>vq :VimuxCloseRunner<CR>

" Send current selection to tmux pane
function! VimuxSlime()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
endfunction

vmap <Leader>vs "vy :call VimuxSlime()<CR>

" Run focused spec
function! VimuxSpec()
  let line=line('.')
  call VimuxSendText('rspec ' . expand('%') . ':' . line('.') . "\n")
endfunction

map <Leader>vr :call VimuxSpec()<CR>
map <Leader>vf :call VimuxSendText('rspec --next-failure' . "\n")<CR>

" Indent guides
let g:indentLine_char = '┆'
