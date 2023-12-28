" basics
set nocompatible                            " be iMproved
set encoding=utf-8                          " utf-8
set nu                                      " show row number
set numberwidth=4
set t_Co=256                                " vim color scheme
set cursorline                              " highlight the current line
set showmatch                               " show mathced brackets
set autoindent smartindent                  " smart auto-indent
set foldmethod=indent                       " default indent mode
set foldlevel=2                             " open folds up to level [N]
set showcmd                                 " show commands that not finished
set directory=/tmp                          " set directory for .swp files
set autochdir                               " automatically change directory
set incsearch hlsearch ignorecase smartcase " settings for search
set title                                   " enable dynamic title
set laststatus=2                            " status line
set expandtab smarttab                      " using 4 spaces to replace tab
set shiftwidth=4
set softtabstop=4
set backspace=indent,eol,start              " enable BACKSPACE on Mac
set backup                                  " backup
set backupdir=/tmp
syntax on                                   " syntax

" ESC and Leader key
inoremap jj <Esc>
inoremap jk <Esc>
let mapleader = " "

" color schemes
colorscheme lucius | LuciusDark  "LuciusWhite

" keybindings
nnoremap <Leader>nl :nohlsearch<CR>

" copy/paste via clipboard/primary (no diff on Win and OSX)
noremap <Leader>y "+y
noremap <Leader>p "+p
" paste from clipboard without auto-indentation
" enable by =:set paste= (to disable =:set nopaste=)
set pastetoggle=<F2>

" press F12 to change file into gbk encoding
nnoremap <F12> :e ++enc=gbk<CR>

" language spell checking
autocmd FileType tex setlocal spell spelllang=en_us
autocmd BufNewFile,BufRead *.md setlocal spell spelllang=en_us

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
  Plugin 'scrooloose/nerdcommenter'
  "Plugin 'scrooloose/nerdtree'
  Plugin 'jamessan/vim-gnupg'
  "Plugin 'airblade/vim-gitgutter'
  Plugin 'vim-airline/vim-airline'
  "Plugin 'vim-airline/vim-airline-themes'
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

" ---- vim-airline ----
" require powerline-symbol patched font, e.g. Roboto Mono in .vim/fonts/
let g:airline_powerline_fonts = 1
