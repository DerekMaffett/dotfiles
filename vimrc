call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-surround'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

nnoremap <c-p> :Files<cr>

Plug 'srcery-colors/srcery-vim'

Plug 'tomtom/tcomment_vim'

call plug#end()

let mapleader = ","
nnoremap <Leader>qq :qa!<cr>

nnoremap <Leader>ev :vsplit ~/.vimrc<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

set termguicolors
colorscheme srcery

