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
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'luochen1990/rainbow'
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'othree/es.next.syntax.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'mhartington/oceanic-next'
Plug 'nightsense/office'
Plug 'altercation/vim-colors-solarized'
Plug 'othree/html5.vim'
Plug 'neomake/neomake'
" Plug 'neoclide/tern-neovim'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'carlitux/deoplete-ternjs', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/jspc.vim', { 'for': ['javascript', 'javascript.jsx'] }

call plug#end()

 "--------------------------- Autocmds -----------------------------------------
augroup vimrc_autocmd
  autocmd!
  "autocmd VimEnter * if argc() == 0 && !exists("s:std_in") |:NERDTreeToggle|endif
  autocmd StdinReadPre * let s:std_in=1
  " no beeps
  set noerrorbells visualbell t_vb=
  if has('autocmd')
    autocmd GUIEnter * set visualbell t_vb=
  endif

  autocmd InsertEnter * set timeoutlen=100
  autocmd InsertLeave * set timeoutlen=1000

  autocmd WinEnter * call NERDTreeQuit()

augroup end


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
"let g:oceanic_next_terminal_bold = 1
"let g:oceanic_next_terminal_italic = 1

syntax on
colorscheme solarized

let g:airline_theme = 'oceanicnext'
let g:airline#extensions#tabline#enabled = 1

" Easy escape from insert
imap fd <Esc>
imap fD <Esc>
imap FD <Esc>

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

" Fix all indents
nnoremap <leader>t<CR> mzgg=G`z:w<CR>

set relativenumber
set numberwidth=4
" hi LineNr ctermbg=NONE ctermfg=grey
" hi Normal guibg=NONE ctermbg=NONE
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

let g:javascript_plugin_flow = 1
let g:rainbow_active = 1

" Use deoplete.
let g:deoplete#enable_at_startup = 1

set completeopt=longest,menuone,preview
let g:deoplete#sources = {}
let g:deoplete#sources['javascript.jsx'] = ['file', 'ultisnips', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent']

let g:deoplete#omni#functions = {}

let g:deoplete#omni#functions.javascript = [
  \ 'tern#Complete',
  \]

" Neomake
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_javascript_jshint_maker = {
      \ 'args': ['--verbose'],
      \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
      \ }

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

" -------------------- strip trailing whitespace -------------------------
function! <SID>StripTrailingWhitespaces()
  "Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

" ---------------- Quit NERDTree if it is the last buffer --------------------
function! NERDTreeQuit()
  redir => buffersoutput
  silent buffers
  redir END
  "                     1BufNo  2Mods.     3File           4LineNo
  let pattern = '^\s*\(\d\+\)\(.....\) "\(.*\)"\s\+line \(\d\+\)$'
  let windowfound = 0

  for bline in split(buffersoutput, "\n")
    let m = matchlist(bline, pattern)

    if (len(m) > 0)
      if (m[2] =~ '..a..')
        let windowfound = 1
      endif
    endif
  endfor

  if (!windowfound)
    quitall
  endif
endfunction
