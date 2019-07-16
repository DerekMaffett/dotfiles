let 
  pkgs = import <nixpkgs> {};
  customNodePackages = import ./nodepkgs/default.nix { inherit (pkgs) nodejs pkgs; };
  vimrc = import ./.vimrc.vim;
  copyToShare = { name, src }: pkgs.stdenv.mkDerivation {
    inherit name src;
    installPhase = ''
      mkdir -p $out/share/
      cp -r $src $out/share/$name
    '';
  };
  haskellScript = name: pkgs.haskell.lib.buildStackProject {
    inherit name;
    src = ../scripts + "/${name}";
  };
in {
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {
    copy = haskellScript "copy";
    projects = haskellScript "projects";
    private-oh-my-zsh = copyToShare {
      name = "oh-my-zsh";
      src = fetchFromGitHub {
        owner = "robbyrussell";
        repo = "oh-my-zsh";
        rev = "17f4cfca99398cb5511557b8515a17bf1bf2948a";
        sha256 = "19f29mrvnhvndvl48fd5kdiixfs0apmb27h4mck5v95p6yw27b6f";
      };
    };
    private-powerlevel9k = copyToShare {
      name = "powerlevel9k";
      src = fetchFromGitHub {
        owner = "bhilburn";
        repo = "powerlevel9k";
        rev = "3dafd79c41f8601b055e607ffefbfe3250c26040";
        sha256 = "0vc5d7w8djg3ah9jvd87xqbhpin1lpflm6wgmhn3jgijwcjkxpg3";
      };
    };
    iterm2ColorSchemes = copyToShare {
        name = "iterm2Colors";
        src = fetchFromGitHub {
            owner = "mbadolato";
            repo = "iTerm2-Color-Schemes";
            rev = "b935cde717cabbfcafe3dca0725e7addc71f92b7";
            sha256 = "0pxrz7h52d4g55nbk9wbn8j51wkdwqvvh0cgg5xc4j4s2kyhdig1";
        };
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
        /* xclip */
        /* gnome-tweaks-3.32.0 */
        /* google-chrome  */
        /* postman */

        copy
        # projects

        nodejs
        direnv
        myNeovim
        tmux
        tmuxinator
        fzf
        jq
        zsh
        private-powerlevel9k
        private-oh-my-zsh
        powerline-fonts
        zsh-completions
        cloc
        autojump
        silver-searcher

        # iterm2 BROKEN
        iterm2ColorSchemes

        nodePackages.prettier

        haskellPackages.brittany

        nodePackages.node2nix
        elm2nix

        purescript
        stack

        elmPackages.elm
        elmPackages.elm-test
        elmPackages.elm-format

        travis
        awscli
        nixops
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
