" ---Vundle plugin management---
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'junegunn/fzf.vim'
Plugin 'scrooloose/nerdtree'

call vundle#end()

" ---Initialization---
syntax on
filetype plugin indent on

let s:uname = system("echo -n \"$(uname)\"")
if s:uname == "Darwin"
  set rtp+=/usr/local/opt/fzf
else
  set rtp+=~/.fzf
endif

" ---General bindings---
let mapleader = "\<Space>"
nmap <c-space> :
nmap <Leader>w :w<CR>
nmap <Leader>c :close<CR>
set cursorline

" Easy escape from insert
inoremap <c-i> <Esc>

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
nnoremap <Leader>j <c-w>h
nnoremap <Leader>k <c-w>l

" Reload
nnoremap <Leader>sv :source $MYVIMRC<CR>

