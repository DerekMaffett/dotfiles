call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-surround'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
nnoremap <c-p> :Files<cr>

Plug 'srcery-colors/srcery-vim'

Plug 'tomtom/tcomment_vim'

Plug 'leafgarland/typescript-vim'

Plug 'elmcast/elm-vim'
" let g:elm_format_autosave = 1

Plug 'tpope/vim-eunuch'

Plug 'tpope/vim-rsi'

Plug 'tpope/vim-sleuth'

" 'vitality'

Plug 'tpope/vim-abolish'

Plug 'tpope/vim-vinegar'
set wildignore+=.git/,.DS_Store

Plug '907th/vim-auto-save'
let g:auto_save = 1

Plug 'AndrewRadev/sideways.vim'
nnoremap (( :SidewaysLeft<cr>
nnoremap )) :SidewaysRight<cr>

Plug 'Shougo/deoplete.nvim'

call plug#end()


""""""""""""""""""""""""""""""


let mapleader = ","

" Window management
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
set splitright
set splitbelow

" Quick changes to vimrc
nnoremap <Leader>ev :vsplit ~/.vimrc<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

" Colors
set termguicolors
colorscheme srcery

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Persistent undo
set undofile
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000

" Misc
nnoremap <Leader>qq :qa!<cr>
cnoremap w<cr> echoerr "Use the autosave or :w!"<cr>
inoremap <C-c> <Esc>

" No arrows allowed
map <Left> :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up> :echo "no!"<cr>
map <Down> :echo "no!"<cr>

