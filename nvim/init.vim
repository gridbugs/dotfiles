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
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'

" Git Annotations
Plugin 'tpope/vim-fugitive'

" See unstaged changes
Plugin 'airblade/vim-gitgutter'

" Code Completion
Plugin 'Valloric/YouCompleteMe', { 'do': './install.py --racer-completer' }

" Multiple Visual Cursors
Plugin 'terryma/vim-multiple-cursors'

" TOML Syntax Highlighting
Plugin 'cespare/vim-toml'

" Rust Plugin
Plugin 'rust-lang/rust.vim'

" Javascript Plugin
Plugin 'pangloss/vim-javascript'

" JSX Plugin
Plugin 'mxw/vim-jsx'

" Typescript Plugin
Plugin 'leafgarland/typescript-vim'

" Typescript JSX Plugin
Plugin 'peitalin/vim-jsx-typescript'

" Fish Plugin
Plugin 'dag/vim-fish'

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
nmap <leader>x :x!<cr>

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

" Dvorak Remappings
noremap k t
noremap K T
noremap t <DOWN>
noremap n <UP>
noremap h <LEFT>
noremap s <RIGHT>
noremap T 10<DOWN>
noremap N 10<UP>
noremap H 10<LEFT>
noremap S 10<RIGHT>
noremap b n
noremap B N

" Map fzf
map <C-f> :FZF<CR>

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

" F3 lists files in the current file's directory
map <F3> :let g:f3_filename=expand("%:t")<CR>:e %:h<CR>10j:call search('^\V' . g:f3_filename, 'c')<CR>

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
set smartindent
set autoindent

" Bell rules
set visualbell t_vb=
set novisualbell

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
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

" Auto format rust code on save
let g:rustfmt_autosave = 1

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

" set filetypes as typescript.tsx
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.tsx
au FileType typescript.tsx,typescript,javascript set shiftwidth=2
au FileType typescript.tsx,typescript,javascript set tabstop=2

" Multiple Cursors Config
let g:multi_cursor_exit_from_insert_mode = 0

" highlight trailing whitespace
set list
if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
else
    set listchars=tab:>\ ,trail:*,extends:#,nbsp:.
endif

" Fix shift mistakes.
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Resize windows with +/-
if bufwinnr(1)
    map + <C-W>+
    map - <C-W>-
endif

" Underline spelling errors
hi clear SpellBad
hi SpellBad cterm=bold,underline

if exists('$TMUX')
    let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[4 q\<Esc>\\"
    let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\"
    autocmd VimLeave * silent !echo -ne "\033Ptmux;\033\033[0 q\033\\"
else
    let &t_SI .= "\<Esc>[4 q"
    let &t_EI .= "\<Esc>[2 q"
    autocmd VimLeave * silent !echo -ne "\033[0 q"
endif

" This sets the cursor to an underline when leaving vim
:au VimLeave * set guicursor=a:hor20-blinkon0

" GitGutter Colours
highlight GitGutterAdd    guifg=#009900 guibg=0 ctermfg=2 ctermbg=0
highlight GitGutterChange guifg=#bbbb00 guibg=0 ctermfg=3 ctermbg=0
highlight GitGutterDelete guifg=#ff2222 guibg=0 ctermfg=1 ctermbg=0
autocmd BufWritePost * GitGutter

" Shortcut to open this config
nnoremap <F8> :e $MYVIMRC<CR>
autocmd! bufwritepost $MYVIMRC source %
