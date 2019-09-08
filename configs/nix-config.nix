let 
  pkgs = import <nixpkgs> {};
  customNodePackages = import ./nodepkgs/default.nix { inherit (pkgs) nodejs pkgs; };
  scripts = (import ../scripts/release.nix).project;

  vimrc = import ./.vimrc.vim;

  isNixOS = builtins.pathExists /etc/NIXOS;

  ifNixOS = derivations: if isNixOS then derivations else [];
  ifNotNixOS = derivations: if !isNixOS then derivations else [];

  fromGithubMaster = name: pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github-pkgs/compiled-github-pkgs.json))."${name}";
  copyToShare = { name, src, dir ? "" }: pkgs.stdenv.mkDerivation {
    inherit name src dir;
    installPhase = ''
      mkdir -p $out/share/
      cp -r $src/$dir $out/share/$name
    '';
  };
in {
  allowUnfree = true;

  packageOverrides = _: with pkgs; rec {
    private-qutebrowser = copyToShare {
        name = "qutebrowser";
        src = fromGithubMaster "qutebrowser";
    };
    private-oh-my-zsh = copyToShare {
        name = "oh-my-zsh";
        src = fromGithubMaster "oh-my-zsh";
    };
    private-powerlevel10k = copyToShare {
        name = "powerlevel10k";
        src = fromGithubMaster "powerlevel10k";
    };
    kittyThemes = copyToShare {
        name = "kittyThemes";
        dir = "themes";
        src = fromGithubMaster "kitty-themes";
    };
    sideways-vim = vimUtils.buildVimPlugin {
        name = "sideways.vim";
        src = fromGithubMaster "sideways.vim";
    };
    vimCopyAsRTF = vimUtils.buildVimPlugin {
        name = "vimCopyAsRTF";
        src = fromGithubMaster "vim-copy-as-rtf";
    };
    potato-colors = vimUtils.buildVimPlugin {
        name = "potato-colors";
        src = fromGithubMaster "potato-colors";
    };
    writeWatchScript = { name, src ? ".", exclude ? "//", command }: 
      writeScriptBin name "${fswatch}/bin/fswatch -r -o -l 0.2 -e ${exclude} ${src} | (while read; do ${command}; done)";
    all = buildEnv {
      name = "all";
      paths = with pkgs; [
        scripts

        lastpass-cli
        wmctrl

        kitty
        kittyThemes
        cloc
        jq
        private-powerlevel10k

        direnv
        myNeovim
        tmux
        tmuxPlugins.vim-tmux-navigator
        tmuxinator
        fzf
        zsh
        any-nix-shell
        powerline-fonts
        zsh-completions
        autojump
        silver-searcher
        fswatch

        nodePackages.prettier
        haskellPackages.brittany

        nix-prefetch-git
        nix-prefetch-github
        cabal-install
        cabal2nix
        nodePackages.node2nix
        elm2nix

        nodejs
        yarn
        customNodePackages.pnpm
        customNodePackages.parcel-bundler
        customNodePackages.deepspeech

        git-quick-stats
        customNodePackages.git-stats
        customNodePackages.git-stats-importer
        customNodePackages.river-cli

        purescript
        stack

        elmPackages.elm
        elmPackages.elm-test
        elmPackages.elm-format

        travis
        awscli
        nixops
      ] ++ ifNixOS [
        steam
        qutebrowser
        xclip 
        slack
        postman 
      ] ++ ifNotNixOS [
        private-qutebrowser
        private-oh-my-zsh
      ];
    };
    myNeovim = neovim.override {
      vimAlias = true;
      configure = {
        customRC = vimrc;
        plug.plugins = with pkgs.vimPlugins; [
          haskell-vim
        ];
        packages.myVimPackage = with pkgs.vimPlugins; {
          # see examples below how to use custom packages
          # vim-sexp vim-sexp-mappings-for-regular-people cljfmt vim-classpath vim-salve
          start = [
            potato-colors 
            vim-css-color
            vim-nix 
            sideways-vim 
            vim-surround 
            fzfWrapper
            fzf-vim 
            vim-eunuch 
            tcomment_vim 
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
