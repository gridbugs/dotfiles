call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'kien/rainbow_parentheses.vim'
Plug 'racer-rust/vim-racer'
Plug 'Chiel92/vim-autoformat'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/syntastic'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --racer-completer' }
Plug 'chazy/cscope_maps'
Plug 'cespare/vim-toml'
call plug#end()

filetype plugin indent on
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
set mouse=a
set t_Co=256

set pastetoggle=<F2>
set bg=dark

" highlight trailing whitespace
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" dvorak remapping
no t j
no n k
no s l
no l n
no j t
no k s
no b n
no B N

" Fix shift mistakes.
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Map Up and Down the way you would expect them to work with wrapped lines.
nmap <silent> t gj
nmap <silent> n gk
nmap <silent> $ g<End>
nmap <silent> 0 g<Home>

" rainbow parentheses
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle

" Rust auto-formatting
let g:formatdef_rustfmt = '"rustfmt"'
let g:formatters_rust = ['rustfmt']
let g:rustfmt_autosave = 0

" Rust racer settings
set hidden
let g:racer_cmd = "/home/steve/.cargo/bin/racer"
let $RUST_SRC_PATH="/home/steve/src/rustc-1.11.0/src"
