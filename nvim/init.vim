" Colours
syntax enable
set t_Co=256
set background=dark
colorscheme ron

if filereadable(expand("~/.vimrc_extra"))
    source ~/.vimrc_extra
endif

if filereadable(expand("~/.vim/plugins.vim"))
    source ~/.vim/plugins.vim
endif

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

" Preview window at bottom of screen
set splitbelow

" Fast saving
nmap <leader>w :w!<cr>
nmap <leader>x :x!<cr>

" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

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

" Pressing ,ss will toggle spell checking
map <leader>ss :setlocal spell!<cr>

" Toggle paste mode on and off
set pastetoggle=<F2>

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

" filetype-specific indentation
au FileType typescript.tsx,typescript,javascript,yaml,python,conf set shiftwidth=2
au FileType typescript.tsx,typescript,javascript,yaml,python,conf set tabstop=2

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

" Cursor changes depedning on mode
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

" Easier navigation between windows
noremap <C-W>h <C-W>h
noremap <C-W>t <C-W>j
noremap <C-W>n <C-W>k
noremap <C-W>s <C-W>l

" Shortcut to open this config
nnoremap <F8> :e $MYVIMRC<CR>

highlight StatusLine ctermbg=15 ctermfg=0
set statusline=
set statusline +=\ %n\             "buffer number
set statusline +=%{&ff}            "file format
set statusline +=%y                "file type
set statusline +=\ %<%F            "full path
set statusline +=%m                "modified flag
set statusline +=%=%5l             "current line
set statusline +=/%L               "total lines
set statusline +=%4v\              "virtual column number
set statusline +=0x%04B\           "character under cursor

if filereadable(expand("~/.vim/dvorak.vim"))
    source ~/.vim/dvorak.vim
endif
