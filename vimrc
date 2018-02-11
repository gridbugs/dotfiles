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
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --racer-completer' }
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
"set ttymouse=xterm2 " required for window resizing in tmux

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

"" dvorak remapping
map t <DOWN>
map n <UP>
map h <LEFT>
map s <RIGHT>
map T 10<DOWN>
map N 10<UP>
map H 10<LEFT>
map S 10<RIGHT>

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

hi CursorColumn term=reverse ctermbg=234 guibg=Grey40
hi CursorLine ctermbg=235 cterm=bold

if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif

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
