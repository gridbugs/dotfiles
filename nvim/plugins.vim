call plug#begin('~/.vim/plugged')

" Git operations from git
Plug 'tpope/vim-fugitive'

" Git changed markers in gutter
Plug 'airblade/vim-gitgutter'

" Multiple Visual Cursors
Plug 'terryma/vim-multiple-cursors'

" TOML Syntax Highlighting
Plug 'cespare/vim-toml'

" Rust syntax highlighting and autoformatting
Plug 'rust-lang/rust.vim'

" Ledger Highlighting
Plug 'ledger/vim-ledger'

" Fuzzy Find
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" NERD Tree
Plug 'preservim/nerdtree'

" Cram tests (used for dune tests)
Plug 'gridbugs/vim-cram'

" Version info in Cargo.toml
Plug 'nvim-lua/plenary.nvim'
Plug 'saecki/crates.nvim', { 'tag': 'stable' }

if has('nvim')
    " Colour scheme
    Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

    " Status line
    Plug 'itchyny/lightline.vim'
endif

call plug#end()

au FileType ocaml map <leader><leader>m :e %:p:s,.mli$,.X123X,:s,.ml$,.mli,:s,.X123X$,.ml,<CR>

" use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Auto format rust code on save
let g:rustfmt_autosave = 1

" Disable preview window when using autocomplete
set completeopt-=preview

" Multiple Cursors
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_next_key = '<C-a>'
let g:multi_cursor_quit_key = '<Esc>'

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

" Highlight comments in json
autocmd FileType json syntax match Comment +\/\/.\+$+

" GitGutter
if filereadable(expand("~/.vim/plugged/vim-gitgutter/autoload/gitgutter.vim"))
    highlight GitGutterAdd ctermfg=2 ctermbg=0
    highlight GitGutterChange ctermfg=3 ctermbg=0
    highlight GitGutterDelete ctermfg=1 ctermbg=0
    autocmd BufWritePost * GitGutter
endif

" Hitting enter on a file node in NERDTree will only reuse buffers on the
" current tab
let NERDTreeCustomOpenArgs = {'file': {'reuse': 'currenttab', 'where': 'p', 'keepopen': 1}, 'dir': {}}
nnoremap <leader><leader>F :NERDTreeFind<CR>
nnoremap <leader><leader>f :NERDTreeToggle<CR>

" Catpuccin settings
if has('nvim') && filereadable(expand("~/.vim/plugged/catppuccin/README.md"))
    source ~/.vim/config-catpuccin.vim
endif

if has('nvim') && filereadable(expand("~/.vim/plugged/crates.nvim/README.md"))
    source ~/.vim/config-crates.vim
endif
