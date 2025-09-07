" Common config shared between neovim and vim. Neovim will also load some
" additional config files.

" Colours
syntax enable
set t_Co=256
set background=dark
set termguicolors

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

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

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
set so=4

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

" Pressing ,ss will toggle spell checking
map <leader>ss :setlocal spell!<cr>

" Keep undo history across sessions by storing it in a file
" Put plugins and dictionaries in this dir (also on Windows)
if has('nvim')
    let vimDir = '$HOME/.config/nvim'
else
    let vimDir = '$HOME/.vim'
endif
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
au FileType yaml,conf,html,css,scss,ocaml,lua,nix set shiftwidth=2 tabstop=2
au FileType typescript.tsx,typescript,javascript,html,htmldjango set noexpandtab

" fix dune indentation to make it respect configured shiftwidth
autocmd FileType dune setlocal indentexpr=DuneIndent() lispoptions=expr:1
function! DuneIndent()
    let prev_line = getline(v:lnum - 1)
    let prev_indent = indent(v:lnum - 1)
    let current_indent = prev_indent
    for i in range(len(prev_line))
        if prev_line[i] == '('
            let current_indent += &shiftwidth
        endif
        if prev_line[i] == ')'
            let current_indent -= &shiftwidth
        endif
    endfor
    return current_indent
endfunction

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
cnoreabbrev W w
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Resize windows with +/-
if bufwinnr(1)
    map + <C-W>+
    map - <C-W>-
endif

" Run omnifunc with control+space
imap <c-space> <c-x><c-o>

" Configure status line
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

" Shortcuts to resize windows
noremap <leader>j :res -1<CR>
noremap <leader>k :res +1<CR>
noremap <leader>h :vertical resize -1<CR>
noremap <leader>l :vertical resize +1<CR>
noremap <leader>J :res -10<CR>
noremap <leader>K :res +10<CR>
noremap <leader>H :vertical resize -10<CR>
noremap <leader>L :vertical resize +10<CR>

set foldmethod=indent
set nofoldenable
