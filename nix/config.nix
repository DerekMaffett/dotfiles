let 
  pkgs = import <nixpkgs> {};
  customNodePackages = import ./nodepkgs/default.nix { inherit (pkgs) nodejs pkgs; };
  vimrc = import ./vimrc.nix;
in {
  allowUnfree = true;

  programs = {
    zsh = {
      enable = true;
      promptInit = "source ${pkgs.zsh-powerlevel9k}/share/zsh-powerlevel9k/powerlevel9k.zsh-theme";
      ohMyZsh = {
          enable = true;
          plugins = ["autojump"];
          theme = "powerlevel9k/powerlevel9k";
      };
    };
  };

  packageOverrides = pkgs: with pkgs; rec {
    iterm2ColorSchemes = stdenv.mkDerivation {
        name = "iterm2Colors";
        src = fetchFromGitHub {
            owner = "mbadolato";
            repo = "iTerm2-Color-Schemes";
            rev = "b935cde717cabbfcafe3dca0725e7addc71f92b7";
            sha256 = "0pxrz7h52d4g55nbk9wbn8j51wkdwqvvh0cgg5xc4j4s2kyhdig1";
        };
        installPhase = ''
            mkdir -p $out/share/
            cp -r $src $out/share/
        '';
    };
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
    all = buildEnv {
      name = "all";
      paths = with pkgs; [
        cowsay
        /* xclip */
        /* gnome-tweaks-3.32.0 */
        /* google-chrome  */
        /* postman */
        nodejs
        direnv
        myNeovim
        tmux
        tmuxinator
        fzf
        jq
        zsh
        oh-my-zsh
        autojump
        cloc
        silver-searcher

        iterm2ColorSchemes
        zsh-powerlevel9k

        nodePackages.prettier

        # haskellPackages.brittany

        nodePackages.node2nix
        elm2nix

        purescript

        stack

        elmPackages.elm
        elmPackages.elm-test
        elmPackages.elm-format

        travis
        awscli
      ];
    };
    myNeovim = neovim.override {
      vimAlias = true;
      configure = {
        customRC = vimrc;
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
