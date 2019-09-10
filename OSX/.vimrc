" basics
set nocompatible                            " be iMproved
set nu                                      " set row number
set t_Co=256                                " vim color scheme
set cursorline                              " highlight the current line
set encoding=utf-8                          " utf-8
set showmatch                               " show mathced brackets
set autoindent smartindent                  " smart auto-indent
set foldmethod=indent                       " default indent mode
set showcmd                                 " show commands that not finished
set directory=/tmp                          " set directory for .swp files
set autochdir                               " automatically change directory
set incsearch hlsearch ignorecase smartcase " settings for search
set title                                   " enable dynamic title
set expandtab smarttab                      " using 4 spaces to replace tab
set shiftwidth=4
set softtabstop=4
set backup                                  " backup
set backupdir=/tmp
syntax on                                   " syntax

" ESC
inoremap jk <Esc>
inoremap <c-[> <Esc>

" color schemes
if ! has("gui_running")
    colorscheme lucius
    LuciusDark
else  " /gvim/
    colorscheme lucius
    LuciusLight
    set guioptions=a  "remove menus, using clipboard instead of primary
    set guicursor=a:blinkwait600-blinkoff600-blinkon600 "blink frequency
endif

" status line
set laststatus=2
set statusline=%<%h%m%r\ %f%=[%{&filetype},%{&fileencoding},%{&fileformat}]%k\ %-14.(%l/%L,%c%V%)\ %P

" copy/paste via clipboard/primary (no diff on Win and OSX)
noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p
" paste from clipboard without auto-indentation
" enable by =:set paste= (to disable =:set nopaste=)
set pastetoggle=<F2>

" keybindings
nnoremap <F8> :vertical wincmd gf<CR>
nnoremap <Leader>nl :nohlsearch<CR>

" language spell checking
autocmd FileType tex setlocal spell spelllang=en_us
autocmd BufNewFile,BufRead *.txt setlocal spell spelllang=en_us

" auto remove extra spaces
autocmd BufWritePre * :%s/\s\+$//e

" --------------------------------------------------------------------------
" Plugins
" --------------------------------------------------------------------------

" ---- Vundle ----
filetype off				" required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" List of installed plugins:
" vim-scripts repos
  Plugin 'taglist.vim'
" repos on github
  "Plugin 'majutsushi/tagbar'   " replace old taglist
  Plugin 'scrooloose/nerdcommenter'
  Plugin 'scrooloose/nerdtree'
  Plugin 'jamessan/vim-gnupg'
  "Plugin 'ervandew/supertab'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" Help
" :PluginList          - list configured bundles
" :PluginInstall       - install(update) bundles
" :PluginSearch foo    - search(or refresh cache first) for foo
" :PluginClean         - confirm(or auto-approve) removal of unused bundles
" see :h vundle for more details or wiki for FAQ

" ---- Taglist ----
let Tlist_Enable_Fold_Column=0
let Tlist_Exit_OnlyWindow=1
let Tlist_GainFocus_On_ToggleOpen=0
let Tlist_Show_One_File=1
let tlist_tex_settings   = 'latex;s:sections;g:graphics;l:labels'
nnoremap <Leader>t :TlistToggle<CR>

" ---- Tagbar ----
"let g:tagbar_autofocus=0
"let g:tagbar_width=40
"nmap <Leader>t :TagbarToggle<CR>
"autocmd BufEnter *.py :call tagbar#autoopen(0)
"autocmd BufWinLeave *.py :TagbarClose

" ---- NERDTree ----
map <Leader>T :NERDTreeToggle<CR>
" close vim if only NERDTree window left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
