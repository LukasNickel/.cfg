set nocompatible
filetype plugin indent on
syntax on
let mapleader = "-"
let maplocalleader = "-"


set showmatch       " show matching brackets
set ignorecase      " case insensitive search
set smartcase       " sensitive search if using uppercases

" TODO: what do I want to achieve here? 
set mouse=v         " mouse in visual mode
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
set smartindent " use smart auto indent
set foldmethod=indent
set foldlevelstart=2


set number " show line numbers
set relativenumber
" set cc=80
set scrolloff=2 " 2 lines above/below cursor when scrolling
set showmode " show mode in status bar
set ruler " show cursor position in status bar 


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


" enable copying to clipboard using ctrl c
vnoremap <C-C> :w !xclip -i -sel c<CR><CR>

set nospell
set spelllang=de,en_gb,en_us
" inoremap <C-s> <c-g>u<Esc>[s1z=`]a<c-g>u

" less flashy colors for git merges
if &diff
    colorscheme morning
    syntax off
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
    Plug 'Vimjas/vim-python-pep8-indent'
    Plug 'tpope/vim-fugitive'
    Plug 'jremmen/vim-ripgrep'
    Plug 'JuliaEditorSupport/julia-vim'
        let g:latex_to_unicode_file_types = ".*"
    Plug 'dense-analysis/ale'
        let g:ale_linters = {'python': ['pyflakes']}
        let g:ale_fixers = {'python': ['black']}
        let g:ale_completion_enabled = 1
        let g:ale_completion_autoimport = 1
    Plug 'sirver/ultisnips'
        let g:UltiSnipsExpandTrigger = '<tab>'
        let g:UltiSnipsJumpForwardTrigger = '<tab>'
        let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
        let g:UltiSnipsSnippetDirectories=["~/UltiSnips"]
    Plug 'lervag/vimtex'
        let g:vimtex_view_method='zathura'
        let g:vimtex_quickfix_mode=0
        set conceallevel=2
        let g:tex_conceal='abdmg'
        let g:vimtex_subfile_start_local = 1
    Plug 'honza/vim-snippets'
    Plug 'KeitaNakamura/tex-conceal.vim', {'for': ['tex', 'wiki']}
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()


let g:vimwiki_list = [
 \ {'path':'~/vimwiki/public', 'auto_tags': 1},
 \ {'path': '~/vimwiki'},
 \ {'path':'~/vimwiki/personal'},
 \ {'path':'~/Documents/university/promotion/lectures/ss21/astro/', 'auto_tags': 1,}]

let g:vimwiki_valid_html_tags = 'b,i,s,u,sub,sup,kbd,br,hr,svg,circle,rect,polygon,ellipse,path,polyline,pattern,marker,line,text,defs,linearGradient,stop'

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction
nnoremap <leader>rn <Plug>(coc-rename)
nnoremap <silent> gd <Plug>(coc-definition)
nnoremap <leader>qf  <Plug>(coc-fix-current)

" Toggle unicode expansion
nnoremap <expr> <F7> LaTeXtoUnicode#Toggle()


" neat pdf export https://askubuntu.com/questions/705973/how-can-i-print-from-vim-to-pdf
command! -range=% PDF <line1>,<line2> hardcopy > %.ps | !ps2pdf %.ps && rm %.ps && echo 'Created: %.pdf'

" read text from pdfs (loses formatting unfortunately)
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
command! TrimWhitespace call TrimWhitespace()

