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
Plug 'junegunn/fzf.vim'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --system-libclang --clang-completer --rust-completer --racer-completer --js-completer ' }
Plug 'scrooloose/syntastic'
Plug 'bitc/vim-hdevtools'
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

" F3 lists files in the current file's directory
map <F3> :let g:f3_filename=expand("%:t")<CR>:e %:h<CR>10j:call search('^\V' . g:f3_filename, 'c')<CR>

set bg=dark

" highlight trailing whitespace
set list

if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
else
    set listchars=tab:>\ ,trail:*,extends:#,nbsp:.
endif

if exists('$TMUX')
    " tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
    let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[6 q\<Esc>\\"
    let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\"
    autocmd VimLeave * silent !echo -ne "\033Ptmux;\033\033[0 q\033\\"
else
    let &t_SI .= "\<Esc>[6 q"
    let &t_EI .= "\<Esc>[2 q"
    autocmd VimLeave * silent !echo -ne "\033[0 q"
endif

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

" Use s and d keys (o and e in dvorak) to move between words
no o b
" d is already the e key

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

let mapleader = ",,"

" Rust racer settings
let g:racer_cmd="~/.cargo/bin/racer"
let g:racer_experimental_completer = 1
let g:autofmt_autosave = 1

let g:ycm_rust_src_path = '~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
nnoremap <Leader>s :YcmCompleter GoTo<CR>

autocmd BufWritePost *.rs RustFmt

" highlight signs in Sy
highlight SignifySignAdd    cterm=bold ctermbg=237  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=237  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=237 ctermfg=227

let g:multi_cursor_exit_from_insert_mode = 0
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

let g:airline#extensions#whitespace#checks = []

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

command! NoReflow setl fo=n1croql

" space is also useless in normal mode, : is awesome
nnoremap <space> :
vnoremap <space> :

au FileType haskell nnoremap <Leader>t :HdevtoolsType<CR>

" Syntastic settings
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let stack_ghc_args = '$(stack-hdevtools-args)'
let g:syntastic_haskell_hdevtools_args = stack_ghc_args
let g:hdevtools_options = stack_ghc_args

let g:fzf_command_prefix = 'Nth'
let g:fzf_history_dir = '~/.fzf-history/vim'

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>, "--all-text",
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Auto reload vimrc
autocmd! bufwritepost .vimrc source %
