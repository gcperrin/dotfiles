" ---Initialization---
syntax on
filetype plugin indent on
scriptencoding utf-8

if (has("termguicolors"))
 set termguicolors
endif

let s:uname = system("echo -n \"$(uname)\"")
set rtp+=~/.fzf

" ---Vundle plugin management---
call plug#begin('~/.config/nvim/bundle')

Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'majutsushi/tagbar'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'vim-airline/vim-airline-themes'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'luochen1990/rainbow'
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'othree/es.next.syntax.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'mhartington/oceanic-next'
Plug 'othree/html5.vim'

call plug#end()

" ---General bindings---
let mapleader = "\<Space>"
nmap <c-space> :
nmap <Leader>w :w<CR>
nmap <Leader>c :close<CR>
nmap <Leader>q :q<CR>

" Indents
set autowrite
set linespace=0
set smartindent
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" Look
set cursorline

" Theme
syntax enable
"let g:oceanic_next_terminal_bold = 1
"let g:oceanic_next_terminal_italic = 1
colorscheme afterglow
let g:airline_theme = 'oceanicnext'
let g:airline#extensions#tabline#enabled = 1

" Easy escape from insert
inoremap <Leader>j <Esc>

nmap <Leader>l :bnext<CR>
nmap <Leader>h :bprevious<CR>

" Faster down and up
nnoremap <c-j> 15gj
vnoremap <c-j> 15gj
nnoremap <c-k> 15gk
vnoremap <c-k> 15gk

" Splitting/switching
nnoremap <Leader>\ <c-w>v
nnoremap <Leader>- <c-w>s

" NERDTree
nnoremap <Leader>; :NERDTreeToggle<CR>
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" Reload
nnoremap <Leader>sv :source $MYVIMRC<CR>

" Better search
nnoremap <Leader>k :let @/=""<CR>

set number
set relativenumber
set numberwidth=4
" hi LineNr ctermbg=236 ctermfg=black

let g:javascript_plugin_flow = 1
let g:rainbow_active = 1
" Use deoplete.
let g:deoplete#enable_at_startup = 1

augroup vimrc_autocmd
    autocmd!
    autocmd InsertEnter * set timeoutlen=100
    autocmd InsertLeave * set timeoutlen=1000
augroup END


function! SyntaxAttr()
     let synid = ""
     let guifg = ""
     let guibg = ""
     let gui   = ""

     let id1  = synID(line("."), col("."), 1)
     let tid1 = synIDtrans(id1)

     if synIDattr(id1, "name") != ""
	  let synid = "group: " . synIDattr(id1, "name")
	  if (tid1 != id1)
	       let synid = synid . '->' . synIDattr(tid1, "name")
	  endif
	  let id0 = synID(line("."), col("."), 0)
	  if (synIDattr(id1, "name") != synIDattr(id0, "name"))
	       let synid = synid .  " (" . synIDattr(id0, "name")
	       let tid0 = synIDtrans(id0)
	       if (tid0 != id0)
		    let synid = synid . '->' . synIDattr(tid0, "name")
	       endif
	       let synid = synid . ")"
	  endif
     endif

     " Use the translated id for all the color & attribute lookups; the linked id yields blank values.
     if (synIDattr(tid1, "fg") != "" )
	  let guifg = " guifg=" . synIDattr(tid1, "fg") . "(" . synIDattr(tid1, "fg#") . ")"
     endif
     if (synIDattr(tid1, "bg") != "" )
	  let guibg = " guibg=" . synIDattr(tid1, "bg") . "(" . synIDattr(tid1, "bg#") . ")"
     endif
     if (synIDattr(tid1, "bold"     ))
	  let gui   = gui . ",bold"
     endif
     if (synIDattr(tid1, "italic"   ))
	  let gui   = gui . ",italic"
     endif
     if (synIDattr(tid1, "reverse"  ))
	  let gui   = gui . ",reverse"
     endif
     if (synIDattr(tid1, "inverse"  ))
	  let gui   = gui . ",inverse"
     endif
     if (synIDattr(tid1, "underline"))
	  let gui   = gui . ",underline"
     endif
     if (gui != ""                  )
	  let gui   = substitute(gui, "^,", " gui=", "")
     endif

     echohl MoreMsg
     let message = synid . guifg . guibg . gui
     if message == ""
	  echohl WarningMsg
	  let message = "<no syntax group here>"
     endif
     echo message
     echohl None
endfunction
