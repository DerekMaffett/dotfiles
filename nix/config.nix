let 
  rc = ''
    """"""""""""""""""""""""""""""

    let g:SuperTabDefaultCompletionType = '<c-n>'
    set wildignore+=.git/,.DS_Store
    let g:auto_save = 0
    nnoremap (( :SidewaysLeft<cr>
    nnoremap )) :SidewaysRight<cr>
    let g:deoplete#enable_at_startup = 1
    let g:salve_auto_start_repl = 1
    let g:elm_setup_keybindings = 0

    let mapleader = ','

    nnoremap <c-p> :Files<cr>
    nnoremap <Leader>a :Ag<cr>
    command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, {'options': '--delimiter : --nth 4..'}, <bang>0)

    " If tabs happen, keep them reasonable
    set tabstop=4
    set shiftwidth=4
    set expandtab

    " Window management
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l
    set splitright
    set splitbelow

    " Clojure REPL

    " Uses vim-fireplace to evaluate a block expression
    nnoremap <Leader>ee cpab
    nnoremap <Leader>ec cqab

    " Colors
    set termguicolors
    colorscheme potato

    " Avoid Neoformat on files known to not have formatters
    fun! RunNeoformat()
        if exists('b:noNeoformat')
            return
        endif
        try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | endtry
    endfun

    " Neoformat on save and on ,f
    augroup fmt
    autocmd FileType clojure let b:noNeoformat=1
    au BufWritePre * call RunNeoformat()
    augroup END
    nnoremap <Leader>f :Neoformat<cr>

    " set rtp+=~/.vim/pack/XXX/start/LanguageClient-neovim
    " let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper'] }
    " let g:LanguageClient_settingsPath = '~/settings.json'
    "
    " nnoremap <F5> :call LanguageClient_contextMenu()<CR>
    " map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
    " map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
    " map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
    " map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
    " map <Leader>lb :call LanguageClient#textDocument_references()<CR>
    " map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
    " map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
    
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

    set timeoutlen=1000 ttimeoutlen=0

    " Line numbers
    set number

    " Misc
    nnoremap <Leader>qq :qa!<cr>

    " No arrows allowed
    map <Left> :echo 'no!'<cr>
    map <Right> :echo 'no!'<cr>
    map <Up> :echo 'no!'<cr>
    map <Down> :echo 'no!'<cr>

    " Copy paste system clipboard
    map <leader>y "*y
    map <leader>p "*p
    map <leader>P "*P
    map <leader>gr "*gr

    " Disable wrapping
    set wrap!


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Below copied from Aaron Jensen: https://github.com/DerekMaffett/vimfiles/blob/master/plugin/expandwindows.vim
    " I kinda know how it works but not completely
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Make the window we're on as big as it makes sense to make it
    set winwidth=84

    " We have to have a winheight bigger than we want to set winminheight. But if
    " we set winheight to be huge before winminheight, the winminheight set will
    " fail.
    set winheight=5
    set winminheight=5
    set winheight=999

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Special window size hack
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    function! ResizeWindow()
    if &previewwindow
        set winheight=999
    elseif &buftype == 'quickfix'
        set winheight=10
    else
        set winheight=999
    endif
    endfunction
    autocmd WinEnter * call ResizeWindow()

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  '';
in {
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    sideways-vim = vimUtils.buildVimPlugin {
        name = "sideways.vim";
        src = fetchFromGitHub {
          owner = "AndrewRadev";
          repo = "sideways.vim";
          rev = "17c03c59913f76bbdede07b8f9d4a1f163d9b2f2";
          sha256 = "1f1s5i8rrgvz1rpyy7lnhrid05ps9fnqryyqpz2nfq0aggws93sr";
        };
    };
    potato-colors = vimUtils.buildVimPlugin {
        name = "potato-colors";
        src = fetchFromGitHub {
          owner = "benburrill";
          repo = "potato-colors";
          rev = "847fb980cab48b8a5a240ef1d35473d399d0db9e";
          sha256 = "0w99bzqgc6bc3riqaaba8b04065jaxndxmm83zcsmvav1msbhx95";
        };
    };
    myNeovim = neovim.override {
      vimAlias = true;
      configure = {
        customRC = rc;
        packages.myVimPackage = with pkgs.vimPlugins; {
          # see examples below how to use custom packages
          # vim-sexp vim-sexp-mappings-for-regular-people cljfmt vim-classpath vim-salve
          start = [
            potato-colors 
            vim-nix 
            sideways-vim 
            vim-surround 
            fzfWrapper
            fzf-vim 
            tcomment_vim 
            vim-eunuch 
            vim-rsi 
            vim-sleuth
            neoformat 
            supertab 
            vim-abolish 
            vim-vinegar 
            vim-auto-save 
            vim-tmux-navigator
            deoplete-nvim 
            yats-vim 
            haskell-vim 
            intero-neovim 
            purescript-vim 
            psc-ide-vim
            vim-fireplace
            elm-vim
          ];
          opt = [ ];
        }; 
      };    
    };
  };
}
