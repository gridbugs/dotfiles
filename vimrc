call plug#begin()
Plug 'powerline/powerline'
Plug 'vim-airline/vim-airline'
Plug 'Chiel92/vim-autoformat'
Plug 'chazy/cscope_maps'
Plug 'cespare/vim-toml'
Plug 'tpope/vim-sensible'
Plug 'tikhomirov/vim-glsl'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'OCamlPro/ocp-indent'
Plug 'ocaml/merlin'
Plug 'rgrinberg/vim-ocaml'
Plug 'terryma/vim-multiple-cursors'
Plug 'mhinz/vim-signify'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'rust-lang/rust.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'racer-rust/vim-racer'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
call plug#end()

filetype plugin indent on
syntax enable
set softtabstop=4
set shiftwidth=4
set tabstop=4
set expandtab
set wildmenu
set number
set visualbell t_vb=
set novisualbell
set incsearch
set ignorecase
set smartcase
set hlsearch
set t_Co=256

" mouse support
set mouse=a
set ttymouse=xterm2 " required for window resizing in tmux

set pastetoggle=<F2>
set bg=dark

" highlight trailing whitespace
set list

if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
else
    set listchars=tab:>\ ,trail:*,extends:#,nbsp:.
endif

" Different cursors in different modes
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
      
" Resize windows with +/-
if bufwinnr(1)
  map + <C-W>+
  map - <C-W>-
endif

" dvorak remapping
map t <DOWN>
map n <UP>
map h <LEFT>
map s <RIGHT>
map T 10<DOWN>
map N 10<UP>
map H 10<LEFT>
map S 10<RIGHT>

" Use qwerty b and e to jump within word
no . e
no x b

" Use b and B to navigate search
no b n
no B N

" Fix shift mistakes.
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Map Up and Down the way you would expect them to work with wrapped lines.
nmap <silent> t gj
nmap <silent> n gk
nmap <silent> $ g<End>
nmap <silent> 0 g<Home>

colorscheme ron
set cursorcolumn
set cursorline
hi CursorColumn term=reverse ctermbg=235 guibg=Grey40
hi CursorLine ctermbg=235 cterm=none
hi clear SpellBad
hi SpellBad cterm=bold,underline

set guicursor=i-n-v-c:block-Cursor

if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }

nnoremap <silent> ,,t :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent>go <C-O>

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"

" Rust racer settings
let g:racer_cmd="/home/steve/.cargo/bin/racer"
let g:racer_experimental_completer = 1
let g:autofmt_autosave = 1

" highlight signs in Sy
highlight SignifySignAdd    cterm=bold ctermbg=237  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=237  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=237 ctermfg=227

let g:multi_cursor_exit_from_insert_mode = 0
let g:airline_powerline_fonts = 1

" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Auto reload vimrc
autocmd! bufwritepost .vimrc source %
