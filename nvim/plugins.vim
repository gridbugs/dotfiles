call plug#begin('~/.vim/plugged')

" Version Control
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

" Scala Highlighting
Plug 'derekwyatt/vim-scala'

" OpenCL Highlighting
Plug 'petRUShka/vim-opencl'

" Ledger Highlighting
Plug 'ledger/vim-ledger'

" Fuzzy Find
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" NERD Tree
Plug 'preservim/nerdtree'

" Nix Highlighting
Plug 'LnL7/vim-nix'

" Autoformatting
Plug 'sbdchd/neoformat'

" GLSL Highlighting
Plug 'beyondmarc/glsl.vim'

" WGSL Highlighting
Plug 'DingDean/wgsl.vim'

" Latex
Plug 'vim-latex/vim-latex'

" Cram tests
Plug 'gridbugs/vim-cram'

" Version info in Cargo.toml
Plug 'nvim-lua/plenary.nvim'
Plug 'saecki/crates.nvim', { 'tag': 'stable' }

if has('nvim')
    " Required for ncm2
    Plug 'roxma/nvim-yarp'

    Plug 'ncm2/ncm2'
    Plug 'ncm2/ncm2-bufword'
    Plug 'ncm2/ncm2-path'

    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }

    " Colour
    Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

    " Status line
    Plug 'itchyny/lightline.vim'
endif

call plug#end()

" Language Client Servers
let g:LanguageClient_serverCommands = {
\ 'rust': ['rust-analyzer'],
\ 'scala': ['metals-vim'],
\ 'python': ['pylsp'],
\ 'typescript': ['typescript-language-server', '--stdio'],
\ 'typescript.tsx': ['typescript-language-server', '--stdio'],
\ 'javascript': ['typescript-language-server', '--stdio'],
\ 'javascript.jsx': ['typescript-language-server', '--stdio'],
\ 'c': ['clangd'],
\ 'cpp': ['clangd'],
\ 'ocaml': ['opam',  'exec', 'ocamllsp', '--', '--fallback-read-dot-merlin'],
\ 'erlang': ['erlang_ls'],
\ }

let g:LanguageClient_loggingFile = expand('~/.vim/LanguageClient.log')
let g:LanguageClient_hoverPreview = "Always"
let g:LanguageClient_useFloatingHover = 0
highlight LanguageClientWarning ctermbg=black
highlight LanguageClientInfo ctermbg=red

" Language Client Shortcuts
au FileType * nmap <leader><leader>t :call LanguageClient#textDocument_hover()<CR>
au FileType * nmap <leader><leader>d :call LanguageClient#textDocument_definition()<CR>
au FileType * nmap <leader><leader>r :call LanguageClient#textDocument_rename()<CR>
au FileType * nmap <leader><leader>n :call LanguageClient#diagnosticsNext()<CR>
au FileType * nmap <leader><leader>N :call LanguageClient#diagnosticsPrevious()<CR>

au FileType ocaml map <leader><leader>m :e %:p:s,.mli$,.X123X,:s,.ml$,.mli,:s,.X123X$,.ml,<CR>

" use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

if filereadable(expand("~/.vim/plugged/ncm2/autoload/ncm2.vim")) && has('nvim')
    " enable ncm2 for all buffers
    autocmd BufEnter * call ncm2#enable_for_buffer()
endif
 " IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect

" Auto format rust code on save
let g:rustfmt_autosave = 1

" Disable preview window when using autocomplete
set completeopt-=preview

" Multiple Cursors
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_next_key = '<C-a>'
let g:multi_cursor_quit_key = '<Esc>'

" Enable rainbow parens
let g:rainbow_active = 1

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

" Neoformat settings
if filereadable(expand("~/.vim/plugged/neoformat/autoload/neoformat.vim"))
    let g:neoformat_ocaml_ocamlformat = {
                \ 'exe': 'ocamlformat',
                \ 'no_append': 1,
                \ 'stdin': 1,
                \ 'args': ['--name', '"%:p"', '-', '--no-version-check']
                \ }

    let g:neoformat_enabled_ocaml = ['ocamlformat']

    augroup fmt
      autocmd!
      autocmd BufWritePre *.ml,*.mli,*.erl try | undojoin | Neoformat | catch /E790/ | Neoformat | endtry
    augroup END
endif

if has('nvim') && filereadable(expand("~/.vim/plugged/catppuccin/README.md"))
lua <<EOF
require("catppuccin").setup({
    flavour = "macchiato",
    color_overrides = {
        macchiato = {
        },
    },
    highlight_overrides = {
        all = function(colors)
            return {
                MatchParen = { bg = colors.surface2 },
                LineNr = { fg = colors.overlay1 },
                Comment = { fg = colors.overlay1 },
            }
        end,
    },
})
vim.cmd.colorscheme "catppuccin"
EOF
   source ~/.vim/catppuccin-lightline.vim
   let g:lightline = {
      \ 'colorscheme': 'catppuccin',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'relativepath', 'modified' ] ]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'readonly', 'relativepath', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

    " Don't display the mode again in the statusbar
    set noshowmode
endif

if has('nvim') && filereadable(expand("~/.vim/plugged/crates.nvim/README.md"))
lua <<EOF
require('crates').setup {
    text = {
        loading = "  Loading...",
        version = "  ✓ %s",
        prerelease = " * %s",
        yanked = "  x yanked",
        nomatch = "  Not found",
        upgrade = "  ↑ %s",
        error = "  Error fetching crate",
    },
    popup = {
        text = {
            title = "# %s",
            pill_left = "",
            pill_right = "",
            created_label = "created        ",
            updated_label = "updated        ",
            downloads_label = "downloads      ",
            homepage_label = "homepage       ",
            repository_label = "repository     ",
            documentation_label = "documentation  ",
            crates_io_label = "crates.io      ",
            categories_label = "categories     ",
            keywords_label = "keywords       ",
            version = "%s",
            prerelease = "%s pre-release",
            yanked = "%s yanked",
            enabled = "* s",
            transitive = "~ s",
            normal_dependencies_title = "  Dependencies",
            build_dependencies_title = "  Build dependencies",
            dev_dependencies_title = "  Dev dependencies",
            optional = "? %s",
            loading = " ...",
        },
    },
    src = {
        text = {
            prerelease = " pre-release ",
            yanked = " yanked ",
        },
    },
}
EOF
endif
