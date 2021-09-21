" px86's neovim config
"                                            |  \             
"        _______   ______   ______  __     __ \▓▓______ ____  
"       |       \ /      \ /      \|  \   /  \  \      \    \ 
"       | ▓▓▓▓▓▓▓\  ▓▓▓▓▓▓\  ▓▓▓▓▓▓\\▓▓\ /  ▓▓ ▓▓ ▓▓▓▓▓▓\▓▓▓▓\
"       | ▓▓  | ▓▓ ▓▓    ▓▓ ▓▓  | ▓▓ \▓▓\  ▓▓| ▓▓ ▓▓ | ▓▓ | ▓▓
"       | ▓▓  | ▓▓ ▓▓▓▓▓▓▓▓ ▓▓__/ ▓▓  \▓▓ ▓▓ | ▓▓ ▓▓ | ▓▓ | ▓▓
"       | ▓▓  | ▓▓\▓▓     \\▓▓    ▓▓   \▓▓▓  | ▓▓ ▓▓ | ▓▓ | ▓▓
"        \▓▓   \▓▓ \▓▓▓▓▓▓▓ \▓▓▓▓▓▓     \▓    \▓▓\▓▓  \▓▓  \▓▓
"                                                             
"
" --------------------------------------------------------------------
                                                      

filetype plugin indent on
syntax on
set number relativenumber
set incsearch
set ignorecase
set smartcase
set nohlsearch
set tabstop=4
set softtabstop=0
set shiftwidth=4
set noexpandtab
set nobackup
set nowritebackup
set noswapfile
set nowrap
set sidescroll=5

set splitbelow
set splitright
set hidden

set termguicolors
set cursorline
set laststatus=1

set background=dark
colorscheme dracula

"set noshowmode

let mapleader = " "
set path+=**
set encoding=UTF-8

""" NetRW config
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 3
let g:netrw_winsize = 20

"Mapping for easily switching between split windows.
nnoremap <c-j> <c-w><c-j>
nnoremap <c-k> <c-w><c-k>
nnoremap <c-l> <c-w><c-l>
nnoremap <c-h> <c-w><c-h>

" nnoremap ; :
" nnoremap : ;

nnoremap <leader>o o<esc>k
nnoremap <leader>O O<esc>j

""" copy to clipboard(+ reg) in visualmode
vnoremap <leader>y "+y

tnoremap <silent> <Esc> <c-\><c-n> 

""" COMPILE
autocmd filetype cpp nnoremap <F5> :w <bar> !g++ -ulimit -Wall -Wno-unused-result -std=c++17   -O2   % -o %:r <CR>
autocmd filetype c nnoremap <F5> :w <bar> !gcc  % -o %:r <CR>
autocmd filetype python nnoremap <F5> :w <bar> !python3 % <CR>

