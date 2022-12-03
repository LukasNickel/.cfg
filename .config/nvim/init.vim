set nocompatible
filetype plugin indent on
syntax on
let mapleader = "-"
let maplocalleader = "-"
let g:nvim_conda_path = expand('~/.local/anaconda3/bin/')
let g:python3_host_prog = expand(g:nvim_conda_path..'python')


set showmatch       " show matching brackets
set ignorecase      " case insensitive search
set smartcase       " sensitive search if using uppercases

" TODO: what do I want to achieve here? 
" set mouse=v         " mouse in visual mode
" only in gui?
" for neovide
" set mouse=nicr
" set mouse=a         


set hlsearch

" tabs and indent
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4


set number " show line numbers
set relativenumber
" set cc=80
set scrolloff=2 " 2 lines above/below cursor when scrolling


" mappings
" Its kinda reversed but w/e
" Also not really reachable on a DE keyboard, which keys  do i want to use
" instead?
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel
nnoremap <leader>( viw<esc>a)<esc>bi(<esc>lel
nnoremap <leader>{ viw<esc>a}<esc>bi{<esc>lel
nnoremap <leader>[ viw<esc>a]<esc>bi[<esc>lel
" This should be filetype sensitive
nnoremap // 0i# <esc>
:inoremap jk <esc>
:inoremap <esc> <nop>
:noremap <left> <nop>
:noremap <right> <nop>
:noremap <up> <nop>
:noremap <down> <nop>
:inoremap <left> <nop>
:inoremap <right> <nop>
:inoremap <up> <nop>
:inoremap <down> <nop>

" Clipboard settings, always use clipboard for all delete, yank, change, put
" operation, see https://stackoverflow.com/q/30691466/6064933
if !empty(provider#clipboard#Executable())
  set clipboard+=unnamedplus
endif

set nospell
set spelllang=de,en_gb,en_us
" inoremap <C-s> <c-g>u<Esc>[s1z=`]a<c-g>u

" less flashy colors for git merges

" diff options
set diffopt=
set diffopt+=vertical  " show diff in vertical position
set diffopt+=filler  " show filler for deleted lines
set diffopt+=closeoff  " turn off diff when one file window is closed
set diffopt+=context:3  " context for diff
set diffopt+=internal,indent-heuristic,algorithm:histogram

" External program to use for grep command
if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
  set grepformat=%f:%l:%c:%m
endif

" Wildmenu completion {{{
set wildmenu
set wildmode=list:longest,full
set wildignore=
set wildignore+=.hg,.git,.svn,*.pyc,*.spl,*.o,*.out,*.DS_Store,*.class,*.manifest
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*,*.bak,*.exe,target,tags,gem.tags
set wildignore+=*.pyc,*.DS_Store,*.db,*.min.js
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.zip,*.xc*,*.pbxproj,*.xcodeproj/**,*.xcassets/**
set wildignore+=*.js.map,ui/public/client/*,cassettes/**,node_modules/**
" }}}

" Faster buffer switching
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-l> <C-W>l
nnoremap <C-h> <C-W>h

call plug#begin('~/.local/share/nvim/site/plugged')
    Plug 'itchyny/lightline.vim'                       " Lightline statusbar
    Plug 'vimwiki/vimwiki'
    Plug 'sainnhe/gruvbox-material'
    Plug 'Vimjas/vim-python-pep8-indent'
    Plug 'tpope/vim-fugitive'
    Plug 'jremmen/vim-ripgrep'
    Plug 'JuliaEditorSupport/julia-vim'
        let g:latex_to_unicode_file_types = ".*"
   Plug 'sirver/ultisnips'
        let g:UltiSnipsExpandTrigger = '<tab>'
        let g:UltiSnipsJumpForwardTrigger = '<tab>'
        let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
        let g:UltiSnipsSnippetDirectories=["~/UltiSnips"]
    Plug 'lervag/vimtex'
        let g:vimtex_view_method='zathura'
        let g:vimtex_quickfix_mode=0
        " set conceallevel=2
        let g:tex_conceal='abdmg'
        let g:vimtex_subfile_start_local = 1
    Plug 'honza/vim-snippets'
    Plug 'KeitaNakamura/tex-conceal.vim', {'for': ['tex', 'wiki']}
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
call plug#end()

colorscheme gruvbox-material


" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

let g:vimwiki_list = [
 \ {'path':'~/vimwiki/public', 'auto_tags': 1},
 \ {'path': '~/vimwiki'},
 \ {'path':'~/vimwiki/personal'},
 \ {'path':'~/Documents/university/promotion/lectures/ss21/astro/', 'auto_tags': 1,}]

let g:vimwiki_valid_html_tags = 'b,i,s,u,sub,sup,kbd,br,hr,svg,circle,rect,polygon,ellipse,path,polyline,pattern,marker,line,text,defs,linearGradient,stop'

" Toggle unicode expansion
nnoremap <expr> <F7> LaTeXtoUnicode#Toggle()

" neat pdf export https://askubuntu.com/questions/705973/how-can-i-print-from-vim-to-pdf
command! -range=% PDF <line1>,<line2> hardcopy > %.ps | !ps2pdf %.ps && rm %.ps && echo 'Created: %.pdf'

" read text from pdfs
:command! -complete=file -nargs=1 Rpdf :r !pdftotext -nopgbrk <q-args> - |fmt -csw78

" If buffer modified, update any 'Last modified: ' in the first 20 lines.
" 'Last modified: ' can have up to 10 characters before (they are retained).
" Restores cursor and window position using save_cursor variable.
function! LastModified()
  if &modified
    let save_cursor = getpos(".")
    let n = min([20, line("$")])
    keepjumps exe '1,' . n . 's#^\(.\{,10}Last modified: \).*#\1' .
              \ strftime('%Y-%m-%d') . '#e'
    call histdel('search', -1)
    call setpos('.', save_cursor)
  endif
endfunction
autocmd BufWritePre * call LastModified()

" Clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" Remove trailing whitespace
fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun
command! TW call TrimWhitespace()

" lsp setup
lua << EOF
local nvim_lsp = require('lspconfig')
local servers = {'pyright', 'texlab'}

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup{
    on_attach = on_attach
    }
end

-- Set diganostic sign icons
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization#change-diagnostic-symbols-in-the-sign-column-gutter
local signs = { Error = "E", Warning = "W ", Hint = "H ", Information = "I " }
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

EOF

" Which key to use? Do I want the terminal split?
function! Termpy()
  exec winheight(0)/4."split" | terminal python3 %
endfunction
command! PY call Termpy()
