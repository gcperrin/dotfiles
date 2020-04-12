let s:uname = system("echo -n \"$(uname)\"")
set rtp+=~/.fzf
" ----------------- Vundle ----------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" File finders
Plugin 'junegunn/fzf.vim'
Plugin 'scrooloose/nerdtree'

" Syntax
Plugin 'w0rp/ale'
Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-commentary'

Plugin 'scrooloose/nerdcommenter'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'unblevable/quick-scope'

" Languages
Plugin 'groenewege/vim-less'
Plugin 'mxw/vim-jsx'
Plugin 'pangloss/vim-javascript'
Plugin 'rust-lang/rust.vim'
Plugin 'tomlion/vim-solidity'
Plugin 'fatih/vim-go'
Plugin 'keith/swift.vim'
Plugin 'udalov/kotlin-vim'
Plugin 'elixir-editors/vim-elixir'
Plugin 'heavenshell/vim-jsdoc'

" Conveniance
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-fugitive'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'Valloric/YouCompleteMe'

" Looks
Plugin 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'ryanoasis/vim-devicons'
Plugin 'morhetz/gruvbox'
Plugin 'lifepillar/vim-solarized8'
Plugin 'dracula/vim', { 'name': 'dracula' }

Plugin 'octol/vim-cpp-enhanced-highlight'

call vundle#end()

"--------------------------- Autocmds -----------------------------------------
augroup vimrc_autocmd
  autocmd!
  autocmd StdinReadPre * let s:std_in=1
  " no beeps
  set noerrorbells visualbell t_vb=
  autocmd GUIEnter * set visualbell t_vb=

  autocmd InsertEnter * set timeoutlen=100
  autocmd InsertLeave * set timeoutlen=1000

  " autocmd FileType * autocmd BufWritePre <buffer> StripWhitespace

  autocmd WinEnter * call NERDTreeQuit()

augroup END

" Don't save backups of *.gpg files
set backupskip+=*.gpg
" To avoid that parts of the file is saved to .viminfo when yanking or
" deleting, empty the 'viminfo' option.
set viminfo=

augroup encrypted
  au!
  " Disable swap files, and set binary file format before reading the file
  autocmd BufReadPre,FileReadPre *.gpg
        \ setlocal noswapfile bin
  " Decrypt the contents after reading the file, reset binary file format
  " and run any BufReadPost autocmds matching the file name without the .gpg
  " extension
  autocmd BufReadPost,FileReadPost *.gpg
        \ execute "'[,']!gpg --decrypt --default-recipient-self" |
        \ setlocal nobin |
        \ execute "doautocmd BufReadPost " . expand("%:r")
  " Set binary file format and encrypt the contents before writing the file
  autocmd BufWritePre,FileWritePre *.gpg
        \ setlocal bin |
        \ '[,']!gpg --encrypt --default-recipient-self
  " After writing the file, do an :undo to revert the encryption in the
  " buffer, and reset binary file format
  autocmd BufWritePost,FileWritePost *.gpg
        \ silent u |
        \ setlocal nobin
augroup END

" --------------- Sets/lets ---------------------
syntax on
filetype plugin indent on

" set shell=/bin/zsh
set shell=/usr/local/bin/zsh
scriptencoding utf-8

if !has('nvim')
  set encoding=utf-8
  set t_Co=256
  set autoread
  set nocompatible
endif

set lazyredraw
set ttyfast
" set autowrite

set linespace=0

" Smart indent
set smartindent

" Tabs
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" Stupid backspace issues
set backspace=indent,eol,start

" Set word wrapping
set whichwrap+=<,>,h,l,[,]

highlight Pmenu guibg=brown gui=bold

set shell=/bin/bash
" set cursorline
"set cursorcolumn
" Use system clipboard buffer

set clipboard+=unnamedplus
if has('unnamedplus')
  " By default, Vim will not use the system clipboard when yanking/pasting to
  " the default register. This option makes Vim use the system default
  " clipboard.
  " Note that on X11, there are _two_ system clipboards: the "standard" one, and
  " the selection/mouse-middle-click one. Vim sees the standard one as register
  " '+' (and this option makes Vim use it by default) and the selection one as
  " '*'.
  " See :h 'clipboard' for details.
  set clipboard=unnamedplus,unnamed
else
  " Vim now also uses the selection system clipboard for default yank/paste.
  if !has('nvim')
    set clipboard=unnamed
  endif
endif

set mouse=a
if !has('nvim')
  set ttymouse=xterm2
endif

" Use relative number in normal mode and absolute number in insert mode
set number
set relativenumber
set hlsearch
highlight Search ctermbg=Yellow ctermfg=Black

" Paren/bracket matching
set showmatch

" Color column
set colorcolumn=0
highlight ColorColumn ctermbg=darkgray

" Buffers
set hidden

" " Folding
" set foldmethod=manual
" set nofoldenable            " Have folds open by default

" Tmux
let g:tmux_navigator_no_mappings = 1

" ctags
set tags=tags
set cino+=(0

" NERDTree
" let NERDTreeIgnore=['\.o$','\.d$', '\~$']

" Nerd Commenter jsx
let g:NERDSpaceDelims = 1
" Nerdtree
let g:NERDTreeMapJumpNextSibling = ''
let g:NERDTreeMapJumpPrevSibling = ''
let NERDTreeShowHidden=1

" YouCompleteMe
let g:ycm_complete_in_comments = 1
let g:ycm_complete_in_strings = 1
let g:ycm_autoclose_preview_window_after_completion = 1

" EXPERIMENTAL
let g:ycm_filetype_whitelist = {'h': 1, 'cpp': 1, 'mm': 1}

" vim devicons
let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFoldersOpenClose = 1

" Ale
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 0
let g:ale_open_list = 0
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_delay = 0

let g:ale_linter_aliases = { 'h': 'cpp' }
let g:ale_linters = {
\   'cpp': ['ccls', 'clangd', 'clangtidy', 'clazy', 'cppcheck', 'cpplint', 'cquery', 'flawfinder'],
\}
" let g:ale_linters = { 'cpp': ['clangd'] }
let g:ale_cpp_parse_compile_commands = 1

" DUMB, but I'm not sure what else to do...
let g:ale_cpp_clangd_options = '
      \ -I/Users/gcperrin/dev/audio/wormhole
      \ -I/Users/gcperrin/dev/audio/wormhole/lib
      \ -I/Users/gcperrin/dev/audio/wormhole/lib/vst3sdk
      \ -I/Users/gcperrin/dev/audio/wormhole/lib/vst3sdk/base
      \ -I/Users/gcperrin/dev/audio/wormhole/lib/vst3sdk/pluginterfaces
      \ -I/Users/gcperrin/dev/audio/wormhole/lib/vst3sdk/public.sdk
      \ -Wall
      \ -Wextra
      \ -Werror
      \ -Wno-long-long
      \ -Wno-variadic-macros
      \ -fexceptions
      \ -DNDEBUG'

let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
let g:tagbar_type_javascript = {
      \ 'ctagstype': 'javascript',
      \ 'kinds': [
      \ 'A:arrays',
      \ 'P:properties',
      \ 'T:tags',
      \ 'O:objects',
      \ 'G:generator functions',
      \ 'F:functions',
      \ 'C:constructors/classes',
      \ 'M:methods',
      \ 'V:variables',
      \ 'I:imports',
      \ 'E:exports',
      \ 'S:styled components'
      \ ]}
"let g:airline#extensions#tagbar#enabled = 0
let g:Tlist_Ctags_Cmd = '/usr/local/bin/ctags'

let g:ycm_clangd_binary_path = '/usr/local/opt/llvm/bin/clangd'

" Quickscope
" let g:qs_lazy_highlight = 1
" let g:qs_enable=0

" ---------------- Look ------------------------
colorscheme dracula
" set background=dark
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
hi Normal guibg=NONE ctermbg=NONE

" --------------------------- Keymaps -----------------------------------------
let mapleader = "\<Space>"
nmap <c-space> :
nmap <Leader>w :w<CR>
nmap <Leader>c :bd<CR>
nmap <Leader>q :q<CR>
nmap <Leader>z ZZ<CR>

" NERDTree
nnoremap <Leader>f :NERDTreeToggle<CR>

" Tagbar
nmap <F8> :TagbarToggle<CR>
nnoremap gd <c-]>

" Turn off highlighted searching
nnoremap <Leader>nh :set hlsearch!<CR>

" Buffer control
nmap <Leader>l :bnext<CR>
nmap <Leader>n :bnext<CR>
nmap <Leader>h :bprevious<CR>
nmap <Leader>p :bprevious<CR>
nmap <Leader>d :bp <BAR> bd #<CR>

" tmux integration
nnoremap <silent> <c-m> :TmuxNavigateDown<CR>
nnoremap <silent> <c-u> :TmuxNavigateUp<CR>
nnoremap <silent> <c-l> :TmuxNavigateRight<CR>
nnoremap <silent> <c-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <C-;> :TmuxNavigatePrevious<cr>

tnoremap jk <C-\><C-n>
tnoremap <ESC> <C-\><C-n>

" ------Make shit easier-----
nmap <Leader>s<CR> :NERDTreeFind<CR>

" Idiot proofing
command W w
command Q q

" Quickly open/reload vim
nnoremap <leader>ev :e $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Easy escape from insert
imap fd <Esc>
"imap fK <Esc>
"imap JK <Esc>
"imap kj <Esc>
"imap KJ <Esc>

" -------- Movement --------
" Allow for homerow up and down in command mode
cnoremap <c-j> <down>
cnoremap <c-k> <up>

" Allow for innerline navagation
nmap j gj
nmap k gk

" Faster down and up
nnoremap <c-j> 15gj
vnoremap <c-j> 15gj
nnoremap <c-k> 15gk
vnoremap <c-k> 15gk

" End and beg of line easier
nnoremap H ^
nnoremap L $

" Splitting/switching
nnoremap <Leader>\ <c-w>v
nnoremap <Leader>- <c-w>s

nnoremap tt :te zsh<CR>

" -------- Formating --------
" These create newlines like o and O but stay in normal mode
nnoremap <silent> zj o<Esc>k
nnoremap <silent> zk O<Esc>j

" Add a space
nnoremap <leader><leader> i <Esc>l

" Fix all indents
nnoremap <leader>t<CR> mzgg=G`z:w<CR>

" Get rid of the fucking stupid OCD whitespace
nnoremap <leader>w :StripWhitespace<CR>:w<CR>

" Emacs indent
nnoremap <Tab> ==
vnoremap <Tab> =

" Code jumping
nnoremap <leader>jp /jumptag<CR>

" Remove upper/lowercase in visual mode.
vnoremap u <Esc>
vnoremap U <Esc>


" Change quotes
nnoremap <leader>' V:s/'/"/g<CR>

" fzf
" nnoremap <C-p> :Files<CR>
" nnoremap <C-s> :BLines<CR>
" nnoremap <C-b> :Buffers<CR>

nnoremap <leader>a :QuickScopeToggle<CR>

" horizontal pane resizing
nnoremap <leader><Right> <c-w>20><CR>
nnoremap <leader><Left> <c-w>20<<CR>

" ------------------------- Strip trailing whitespace -------------------------
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
