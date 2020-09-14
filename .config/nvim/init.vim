set nocompatible
set showmatch       " show matching brackets
set ignorecase      " case insensitive search
set smartcase       " sensitive search if using uppercases
set mouse=v         " mouse in visual mode
set hlsearch
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set smartindent " use smart auto indent
set number " show line numbers
set relativenumber
set cc=80
set scrolloff=2 " 2 lines above/below cursor when scrolling
set showmode " show mode in status bar
set ruler " show cursor position in status bar 
set hidden " remember undo after quitting

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

filetype plugin indent on
syntax on

call plug#begin('~/.local/share/nvim/site/plugged')
    Plug 'vimwiki/vimwiki'
    Plug 'nvie/vim-flake8'
    Plug 'Vimjas/vim-python-pep8-indent'
    Plug 'tpope/vim-fugitive'
    Plug 'sirver/ultisnips'
        let g:UltiSnipsExpandTrigger = '<tab>'
        let g:UltiSnipsJumpForwardTrigger = '<tab>'
        let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
        let g:UltiSnipsSnippetDirectories=["~/UltiSnips"]
    Plug 'lervag/vimtex'
        let g:tex_flavor='latex'
        let g:vimtex_view_method='zathura'
        let g:vimtex_quickfix_mode=0
        set conceallevel=2
        let g:tex_conceal='abdmg'
    Plug 'KeitaNakamura/tex-conceal.vim', {'for': ['tex', 'wiki']}
call plug#end()

autocmd BufWritePost *.py call Flake8()

let g:vimwiki_list = [{'path':'~/vimwiki/public', 'auto_tags': 1}, {'path': '~/vimwiki'}, {'path':'~/vimwiki/personal'}]


