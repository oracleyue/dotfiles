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
set expandtab smarttab                      " using 4 spaces to replace tab
set shiftwidth=4
set softtabstop=4
set backspace=indent,eol,start              " enable BACKSPACE on Mac
set backup                                  " backup
set backupdir=/tmp
syntax on                                   " syntax

" ESC
inoremap jk <Esc>
inoremap <c-[> <Esc>

" color schemes
if !has("gui_running")
    colorscheme dracula
else  " /gvim/ or /macvim/
    colorscheme dracula
    "colorscheme lucius | LuciusDark  "LuciusWhite
    set guifont=Roboto\ Mono\ for\ Powerline:h15
    set guioptions=a  "remove menus, using clipboard instead of primary
    set guicursor=a:blinkwait600-blinkoff600-blinkon600 "blink frequency
    set lines=48 columns=90
endif

" status line
set laststatus=2
"set statusline=%<%h%m%r\ %f%=[%{&filetype},%{&fileencoding},%{&fileformat}]%k\ %-14.(%l/%L,%c%V%)\ %P

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
  " productivity
  Plugin 'scrooloose/nerdcommenter'
  " filetype supports
  Plugin 'jamessan/vim-gnupg'
  " git
  Plugin 'airblade/vim-gitgutter'
  " interface/theme/wm
  Plugin 'scrooloose/nerdtree'
  Plugin 'vim-airline/vim-airline'
  Plugin 'vim-airline/vim-airline-themes'
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

" ---- NERDTree ----
map <Leader>T :NERDTreeToggle<CR>
" close vim if only NERDTree window left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" ---- vim-airline ----
" require powerline-symbol patched font installed
" install Roboto Mono given in .vim/fonts/
let g:airline_powerline_fonts = 1
" remove empty angle at the end
let g:airline_skip_empty_sections = 1
" set airline theme
let g:airline_theme='deus'
" extension for tab line
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#formatter = 'unique_tail'

" ---- vim-gitgutter ----
set updatetime=250
" jump to next/prev change/chunk: "]c", "[c"
" undo hanks: "<Leader>hu"
" preview hunk: "<Leader>hp"
