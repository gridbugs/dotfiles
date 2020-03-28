call plug#begin('~/.vim/plugged')

" Fuzzy Find
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Version Controlt
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'shumphrey/fugitive-gitlab.vim'
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

" Rust Completion
Plug 'racer-rust/vim-racer'

" Scala Highlighting
Plug 'derekwyatt/vim-scala'

" LSP Client
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Scala Autocomplete
Plug 'scalameta/coc-metals', {'do': 'yarn install --frozen-lockfile'}

" CtrlP
Plug 'kien/ctrlp.vim'

" Code Completion (requires manual install)
" For rust, c, and typescript completion, update RUST_TOOLCHAIN in
" ~/.vim/plugged/YouCompleteMe/third_party/ycmd/build.py and run:
" ~/.vim/plugged/YouCompleteMe/install.py --racer-completer --clang-completer --ts-completer
Plug 'Valloric/YouCompleteMe'

call plug#end()

" Auto format rust code on save
let g:rustfmt_autosave = 1

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

" YCM Global Config
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'

" YCM Colours
highlight YcmWarningSection ctermbg=236
highlight YcmErrorSection ctermfg=1

" COC Colours
highlight MyCocFloating ctermfg=236
highlight default link CocFloating MyCocFloating

" Enable rainbow parens
let g:rainbow_active = 1

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

" Highlight comments in json
autocmd FileType json syntax match Comment +\/\/.\+$+

" YCM Shortcuts
au FileType rust,typescript,tsx nmap <leader><leader>t :YcmCompleter GetType<CR>
au FileType rust,typescript,tsx nmap <leader><leader>d :YcmCompleter GoToDefinition<CR>
au FileType rust,typescript,tsx nmap <leader><leader>D :YcmCompleter GetDoc<CR>

" COC Shortcuts
au FileType scala nmap <leader><leader>t :call CocAction('doHover')<CR>
au FileType scala nmap <leader><leader>d <Plug>(coc-definition)

" Stop YCM from handling filetypes that COC handles
let g:ycm_filetype_blacklist = {
  \ 'scala': 1,
  \ 'cpp': 1,
  \ 'c': 1,
  \ 'h': 1,
  \}

" GitGutter
if filereadable(expand("~/.vim/plugged/vim-gitgutter/autoload/gitgutter.vim"))
    highlight GitGutterAdd    ctermfg=2
    highlight GitGutterChange ctermfg=3
    highlight GitGutterDelete ctermfg=1
    autocmd BufWritePost * GitGutter
endif
