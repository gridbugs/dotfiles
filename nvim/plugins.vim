" Vundle preamble and packages
set nocompatible
filetype off
set rtp+=~/.config/nvim/Vundle.vim
call vundle#begin()

" Let Vundle manage itself
Plugin 'VundleVim/Vundle.vim'

" Fuzzy Find
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'

" See unstaged changes
Plugin 'airblade/vim-gitgutter'

" Multiple Visual Cursors
Plugin 'terryma/vim-multiple-cursors'

" TOML Syntax Highlighting
Plugin 'cespare/vim-toml'

" Rust Plugin
Plugin 'rust-lang/rust.vim'

" Javascript Plugin
Plugin 'pangloss/vim-javascript'

" JSX Plugin
Plugin 'mxw/vim-jsx'

" Typescript Plugin
Plugin 'leafgarland/typescript-vim'

" Typescript JSX Plugin
Plugin 'peitalin/vim-jsx-typescript'

" Rainbow Parens
Plugin 'luochen1990/rainbow'

" Auto Format C
Plugin 'rhysd/vim-clang-format'

" Rust Completion
Plugin 'racer-rust/vim-racer'

" Scala Highlighting
Plugin 'derekwyatt/vim-scala'

" LSP Client (requires manual install)
Plugin 'neoclide/coc.nvim', {'branch': 'release'}

" Code Completion (requires manual install)
Plugin 'Valloric/YouCompleteMe'

call vundle#end()
filetype plugin indent on

" End of Vundle preamble

" Auto format rust code on save
let g:rustfmt_autosave = 1

" GitGutter
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

source ~/.vim/coc.vim
