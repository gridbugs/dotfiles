call plug#begin()
Plug 'racer-rust/vim-racer'
Plug 'Chiel92/vim-autoformat'
Plug 'rust-lang/rust.vim'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --racer-completer' }
Plug 'chazy/cscope_maps'
Plug 'cespare/vim-toml'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-sensible'
Plug 'tikhomirov/vim-glsl'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'OCamlPro/ocp-indent'
Plug 'ocaml/merlin'
Plug 'rgrinberg/vim-ocaml'
Plug 'terryma/vim-multiple-cursors'
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

if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
else
    set listchars=tab:>\ ,trail:*,extends:#,nbsp:.
endif

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
cnoreabbrev Q q
cnoreabbrev Wq wq
cnoreabbrev WQ wq

" Map Up and Down the way you would expect them to work with wrapped lines.
nmap <silent> t gj
nmap <silent> n gk
nmap <silent> $ g<End>
nmap <silent> 0 g<Home>

" Rust racer settings
set hidden
let g:racer_cmd="/home/steve/.cargo/bin/racer"
let g:racer_experimental_completer = 1
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

" ocaml
set rtp+=~/.opam/4.05.0/share/merlin/vim/doc
source ~/.vim/ocaml.vim
au FileType ocaml nmap ,,t :MerlinTypeOf<CR>

" Per-language tab width
au FileType python setl sw=2 sts=2 et
au FileType ocaml setl sw=2 sts=2 et
au FileType html setl sw=2 sts=2 et
au FileType yaml setl sw=2 sts=2 et
au BufRead,BufNewFile jbuild set lisp
au BufRead,BufNewFile jbuild set syntax=scm
