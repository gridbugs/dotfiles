" Vundle preamble and packages
set nocompatible
filetype off
set rtp+=~/.config/nvim/Vundle.vim
call vundle#begin()

" Let Vundle manage itself
Plugin 'VundleVim/Vundle.vim'

" Rust Completion
Plugin 'racer-rust/vim-racer'

" Fuzzy Find
Plugin 'junegunn/fzf.vim'

" Git Annotations
Plugin 'tpope/vim-fugitive'

" Code Completion
Plugin 'Valloric/YouCompleteMe'

" Multiple Visual Cursors
Plugin 'terryma/vim-multiple-cursors'

call vundle#end()
filetype plugin indent on

" End of Vundle preamble

" Custom Leader
let mapleader=","

" Searching
set ignorecase
set smartcase
set hlsearch

" Don't redraw while executing macros
set lazyredraw

" Show matching bracket
set showmatch

" Line numbers
set number

" Enable mouse
set mouse=a

" Fast saving
nmap <leader>w :w!<cr>

" Colours
syntax enable
set t_Co=256
set background=dark
colorscheme ron

" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Backup files
set nobackup
set nowb
set noswapfile

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Padding below current cursor position
set so=8

" Tab rules
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Remap VIM 0 to first non-blank character
map 0 ^

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Toggle paste mode on and off
set pastetoggle=<F2>

" Racer Config
let g:racer_cmd="~/.cargo/bin/racer"
let g:racer_experimental_completer = 1

" Keep undo history across sessions by storing it in a file
" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

" Multiple Cursors Config
let g:multi_cursor_exit_from_insert_mode = 0

" Shortcut to open this config
nnoremap <F8> :e $MYVIMRC<CR>
autocmd! bufwritepost $MYVIMRC source %
