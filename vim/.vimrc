" Configure Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'othree/eregex.vim'
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

syntax on
set autoindent
set number
set expandtab
set tabstop=2
