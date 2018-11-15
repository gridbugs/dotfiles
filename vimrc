call plug#begin()
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
Plug 'bitc/vim-hdevtools'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

filetype plugin indent on
syntax enable
set softtabstop=4
set shiftwidth=4
set tabstop=4
set expandtab
set wildmode=longest:list,full
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
hi clear SpellBad
hi SpellBad cterm=bold,underline

let mapleader = ",,"

" Rust racer settings
let g:racer_cmd="~/.cargo/bin/racer"
"let g:racer_experimental_completer = 1
"let g:autofmt_autosave = 1
au FileType rust nmap <leader>gd <Plug>(rust-def)
au FileType rust nmap <leader>gs <Plug>(rust-def-split)
au FileType rust nmap <leader>gv <Plug>(rust-def-vertical)

let g:ycm_rust_src_path = '~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
"nnoremap <Leader>s :YcmCompleter GoTo<CR>
let g:ycm_key_list_select_completion = ['<tab>', '<Down>']

let g:rustfmt_autosave = 1

" highlight signs in Sy
highlight SignifySignAdd    cterm=bold ctermbg=237  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=237  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=237 ctermfg=227

let g:multi_cursor_exit_from_insert_mode = 0

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


" useful aliases
fun! SetupCommandAlias(from, to)
    exec 'cnoreabbrev <expr> '.a:from
                \ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:from.'")'
                \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfun
call SetupCommandAlias("W","w")
call SetupCommandAlias("X","x")
call SetupCommandAlias("Q","q")
call SetupCommandAlias("Ls","ls")
call SetupCommandAlias("B","b")

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
    execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
    execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
    let l:dir = s:opam_share_dir . "/merlin/vim"
    execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
    " Respect package order (merlin should be after ocp-index)
    if count(s:opam_available_tools, tool) > 0
        call s:opam_configuration[tool]()
    endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line

"" ocaml setup
"let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
"execute "set rtp+=" . g:opamshare . "/merlin/vim"
au FileType ocaml nmap <Leader>t :MerlinTypeOf<CR>
au FileType ocaml vmap <Leader>t :MerlinTypeOfSel<CR>
au FileType ocaml nmap <Leader>s :MerlinLocate<CR>
au FileType ocaml set softtabstop=2
au FileType ocaml set shiftwidth=2
au FileType ocaml set tabstop=2
"autocmd FileType ocaml exec "source " . g:opamshare . "/ocp-indent/vim/indent/ocaml.vim"

au FileType yaml set softtabstop=2
au FileType yaml set shiftwidth=2
au FileType yaml set tabstop=2

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline_theme = 'jellybeans'

let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
let g:airline_symbols.maxlinenr = ''

let g:airline#extensions#whitespace#checks = []
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#show_splits = 0


" Auto reload vimrc
nnoremap <F8> :e $MYVIMRC<CR>
autocmd! bufwritepost .vimrc source %
