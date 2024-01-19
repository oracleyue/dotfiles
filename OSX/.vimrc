" basics
set nocompatible                            " be iMproved
set encoding=utf-8                          " utf-8
set t_Co=256                                " vim color scheme
set title                                   " enable dynamic title
set cursorline                              " highlight the current line
set laststatus=2                            " status line
set number relativenumber                   " show row number
set showmatch                               " show mathced brackets
set incsearch hlsearch ignorecase smartcase " settings for search
set autoindent smartindent                  " smart auto-indent
set foldmethod=indent                       " default indent mode
set foldlevel=2                             " open folds up to level [N]
set backspace=indent,eol,start              " enable BACKSPACE on Mac
set expandtab smarttab                      " using 4 spaces to replace tab
set shiftwidth=4
set softtabstop=4
set showcmd                                 " show commands that not finished
set autochdir                               " automatically change directory
set backupdir=/tmp
set backup                                  " backup
set tags=./tags                             " default ctag files
syntax on                                   " syntax

" ESC and Leader key
inoremap jj <Esc>
inoremap jk <Esc>
let mapleader = " "

" color schemes
colorscheme lucius | LuciusDark  "LuciusWhite
if has("gui_running")
    set guifont=Roboto\ Mono\ for\ Powerline\ 11
    set guioptions=a  "remove menus, using clipboard instead of primary
    set lines=45 columns=81
endif

" default auto-complete by C-n/p
" better auto-complete via omni-complete
" [usage]: prefix ctrl+x, ctrl+f (filename), +i (path), +l (wholeline), +o (omni)
"set omnifunc=syntaxcomplete#Complete

" keybindings
nnoremap <Leader>qq :q<CR>
nnoremap <Leader>nl :nohlsearch<CR>
" avoid mistyping q for recording
nnoremap Q q
nnoremap q <Nop>

" locating (H/M/L to head/middle/bottom; zt/zz/zb to top/center/bottom this line)

" buffers (use :b[NUM] to switch; :bd to close)
set hidden
nnoremap <Leader>b :buffers<CR>
nnoremap <Leader>qb :bd<CR>

" copy/paste via clipboard/primary (no diff on Win and OSX)
noremap <Leader>y "+y
noremap <Leader>p "+p
" paste from clipboard without auto-indentation
" enable by =:set paste= (to disable =:set nopaste=)
set pastetoggle=<F2>

" press F12 to change file into gbk encoding
"nnoremap <F12> :e ++enc=gbk<CR>
:command SaveInGBK :e ++enc=gbk

" language spell checking
autocmd FileType tex setlocal spell spelllang=en_us
autocmd BufNewFile,BufRead *.md setlocal spell spelllang=en_us

" auto remove extra spaces
autocmd BufWritePre * :%s/\s\+$//e

" --------------------------------------------------------------------------
" Plugin Management
" --------------------------------------------------------------------------
filetype off				" required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" List of installed plugins:
  "Plugin 'vim-scripts/taglist.vim'
  Plugin 'scrooloose/nerdcommenter'
  Plugin 'scrooloose/nerdtree'
  Plugin 'jamessan/vim-gnupg'
  Plugin 'vim-airline/vim-airline'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" :PluginList          - list configured bundles
" :PluginInstall       - install(update) bundles
" :PluginClean         - confirm(or auto-approve) removal of unused bundles
" see :h vundle for more details or wiki for FAQ

" ---- NerdCommenter ----
let g:NERDSpaceDelims=1  " add a space when comment
nnoremap <Leader>/ :call nerdcommenter#Comment(0,"toggle")<CR>

" ---- NerdTree ----
nnoremap <Leader>e :NERDTreeToggle<CR>

" ---- vim-airline ----
" require powerline-symbol patched font, e.g. Roboto Mono in .vim/fonts/
let g:airline_powerline_fonts = 1

" ---- Taglist ----
" let Tlist_Enable_Fold_Column=0
" let Tlist_Exit_OnlyWindow=1
" let Tlist_Show_One_File=1
" nnoremap <Leader>t :TlistToggle<CR>
" let g:tlist_tex_settings='tex;c:chapters;s:sections;u:subsections;b:subsubsections;l:labels'
