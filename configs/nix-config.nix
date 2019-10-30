let 
  pkgs = import <nixpkgs> {};
  customNodePackages = import ./nodepkgs/default.nix { inherit (pkgs) nodejs pkgs; };
  scripts = import "${(fromGithubMaster "scripts")}/default.nix";

  vimrc = import ./.vimrc.vim;

  isNixOS = builtins.pathExists /etc/NIXOS;
  isLinux = builtins.currentSystem == "x86_64-linux";
  isMac = builtins.currentSystem == "x86_64-darwin";

  ifNixOS = derivations: if isNixOS then derivations else [];
  ifNotNixOS = derivations: if !isNixOS then derivations else [];
  linuxOnly = derivations: if isLinux then derivations else [];
  macOnly = derivations: if isMac then derivations else [];

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
  allowBroken = true;

  packageOverrides = _: with pkgs; rec {
    system-update = writeShellScriptBin "system-update" "nix-npm update && nix-github update && nix-env -i all && nix-collect-garbage";
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
    dracula = vimUtils.buildVimPlugin {
        name = "dracula";
        src = fromGithubMaster "vim"; # Yeah, this is a problem... bad repo names out of context
    };
    vim-graphql = vimUtils.buildVimPluginFrom2Nix { # no clue what the difference is right now
      name = "vim-graphql";
      src = fromGithubMaster "vim-graphql";
    };

    writeWatchScript = { name, src ? ".", exclude ? "//", command }: 
      writeShellScriptBin name "${fswatch}/bin/fswatch -0 --event=Updated -r -o -l 0.2 -e ${exclude} ${src} | xargs -0 -I {} -n 1 ${command}";

    localCabalRun = name: executable: writeShellScriptBin name "cabal new-run ${executable} $@";

    all = buildEnv {
      name = "all";
      paths = with pkgs; [
        system-update
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
        unixtools.netstat

        nodePackages.prettier
        haskellPackages.brittany

        nix-prefetch-github
        cabal-install
        cabal2nix
        haskellPackages.hpack
        nodePackages.node2nix
        elm2nix

        nodejs-12_x
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

        haskellPackages.fswatcher
        travis
        awscli
        aws-iam-authenticator
        nixops
        terraform_0_12
      ] ++ ifNixOS [
        qutebrowser
        xclip 
        slack
        postman 
      ] ++ ifNotNixOS [
        private-oh-my-zsh
      ] ++ linuxOnly [
        hydra
        gnome3.gnome-tweak-tool
      ] ++ macOnly [
      ];
    };
    myNeovim = neovim.override {
      vimAlias = true;
      configure = {
        customRC = vimrc;
        plug.plugins = with pkgs.vimPlugins; [
          haskell-vim
          dracula
          vim-graphql
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
            vim-javascript
            vim-jsx-pretty
            purescript-vim 
            Jenkinsfile-vim-syntax
            psc-ide-vim
            vim-fireplace
            elm-vim
            vim-terraform
          ];
          opt = [ ];
        }; 
      };    
    };
  };
}
