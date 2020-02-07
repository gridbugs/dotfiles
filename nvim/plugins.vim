call plug#begin('~/.vim/plugged')

" Fuzzy Find
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" See unstaged changes
Plug 'airblade/vim-gitgutter'

" Multiple Visual Cursors
Plug 'terryma/vim-multiple-cursors'

" TOML Syntax Highlighting
Plug 'cespare/vim-toml'

" Rust Plug
Plug 'rust-lang/rust.vim'

" Javascript Plug
Plug 'pangloss/vim-javascript'

" JSX Plug
Plug 'mxw/vim-jsx'

" Typescript Plug
Plug 'leafgarland/typescript-vim'

" Typescript JSX Plug
Plug 'peitalin/vim-jsx-typescript'

" Rainbow Parens
Plug 'luochen1990/rainbow'

" Auto Format C
Plug 'rhysd/vim-clang-format'

" Rust Completion
Plug 'racer-rust/vim-racer'

" Scala Highlighting
Plug 'derekwyatt/vim-scala'

" LSP Client
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Scala Autocomplete
Plug 'scalameta/coc-metals', {'do': 'yarn install --frozen-lockfile'}

" Code Completion (requires manual install)
" For rust, c, and typescript completion, update RUST_TOOLCHAIN in
" ~/.vim/plugged/YouCompleteMe/third_party/ycmd/build.py and run:
" ~/.vim/plugged/YouCompleteMe/install.py --racer-completer --clang-completer --ts-completer
Plug 'Valloric/YouCompleteMe'

call plug#end()

" Auto format rust code on save
let g:rustfmt_autosave = 1

" GitGutter
if filereadable(expand("~/.vim/plugged/vim-gitgutter/autoload/gitgutter.vim"))
    if has('nvim')
        highlight GitGutterAdd    guifg=#009900 guibg=0
        highlight GitGutterChange guifg=#bbbb00 guibg=0
        highlight GitGutterDelete guifg=#ff2222 guibg=0
    else
        highlight GitGutterAdd    ctermfg=2 ctermbg=0
        highlight GitGutterChange ctermfg=3 ctermbg=0
        highlight GitGutterDelete ctermfg=1 ctermbg=0
    endif
    autocmd BufWritePost * GitGutter
endif

" Disable preview window when using autocomplete
set completeopt-=preview

" Multiple Cursors
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_next_key = '<C-a>'
let g:multi_cursor_quit_key = '<Esc>'

" Racer Config
let g:racer_cmd="~/.cargo/bin/racer"
let g:racer_experimental_completer = 1
au FileType rust nmap <leader><leader>d <Plug>(rust-def)
au FileType rust nmap <leader><leader>s <Plug>(rust-def-split)
au FileType rust nmap <leader><leader>x <Plug>(rust-def-vertical)
au FileType rust nmap <leader><leader>D <Plug>(rust-doc)
au FileType rust nmap <leader><leader>t :YcmCompleter GetType<CR>
au FileType c nmap <leader><leader>t :YcmCompleter GetType<CR>

" Ycm Colours
highlight YcmWarningSection ctermbg=238
highlight YcmErrorSection ctermfg=1
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'

" Enable rainbow parens
let g:rainbow_active = 1

" Auto format c code on save
autocmd FileType c ClangFormatAutoEnable

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

" Highlight comments in json
autocmd FileType json syntax match Comment +\/\/.\+$+

if filereadable(expand("~/.vim/plugged/coc.nvim/package.json"))
    source ~/.vim/coc.vim
endif
