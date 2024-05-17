{ config, pkgs, ... }:

let
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

  # Packages

  system-update = pkgs.writeShellScriptBin "system-update" "nix-npm update && nix-github update && home-manager switch && nix-collect-garbage";

  bash-git-prompt = copyToShare {
      name = "bash-git-prompt";
      src = fromGithubMaster "bash-git-prompt";
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
  sideways-vim = pkgs.vimUtils.buildVimPlugin {
      name = "sideways.vim";
      src = fromGithubMaster "sideways.vim";
  };
  vimCopyAsRTF = pkgs.vimUtils.buildVimPlugin {
      name = "vimCopyAsRTF";
      src = fromGithubMaster "vim-copy-as-rtf";
  };
  potato-colors = pkgs.vimUtils.buildVimPlugin {
      name = "potato-colors";
      src = fromGithubMaster "potato-colors";
  };
  dracula = pkgs.vimUtils.buildVimPlugin {
      name = "dracula";
      src = fromGithubMaster "vim"; # Yeah, this is a problem... bad repo names out of context
  };
  vim-graphql = pkgs.vimUtils.buildVimPluginFrom2Nix { # no clue what the difference is right now
    name = "vim-graphql";
    src = fromGithubMaster "vim-graphql";
  };
  elixir-tools = pkgs.vimUtils.buildVimPlugin {
    name = "elixir-tools";
    src = fromGithubMaster "elixir-tools.nvim";
  };

  writeWatchScript = { name, src ? ".", exclude ? "//", command }: 
    pkgs.writeShellScriptBin name "${pkgs.fswatch}/bin/fswatch -0 --event=Updated -r -o -l 0.2 -e ${exclude} ${src} | xargs -0 -I {} -n 1 ${command}";

  localCabalRun = name: executable: pkgs.writeShellScriptBin name "cabal new-run ${executable} -- $@";

  myNeovim = neovimPkg: neovimPkg.override {
    vimAlias = true;
    configure = {
      customRC = vimrc;
      packages.myVimPackage = with pkgs.vimPlugins; {
        # see examples below how to use custom packages
        # vim-sexp vim-sexp-mappings-for-regular-people cljfmt vim-classpath vim-salve
        start = [
          potato-colors 
          vim-graphql
          haskell-vim
          dracula
          vim-css-color
          vim-nix 
          sideways-vim 
          vim-surround 
          vim-repeat
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
          elixir-tools
          vim-elixir
          rustaceanvim
          nvim-treesitter
        ];
        opt = [ ];
      }; 
    };    
  };

in
{
  home.username = "derek";
  home.homeDirectory = "/home/derek";

  home.stateVersion = "24.05";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    git
    curl

    system-update
    scripts

    lastpass-cli
    wmctrl

    kitty
    kittyThemes
    cloc
    jq
    private-powerlevel10k

    go-task
    gnumake
    direnv
    tmux
    tmuxPlugins.vim-tmux-navigator
    tmuxinator
    fzf
    any-nix-shell
    powerline-fonts
    bash-git-prompt
    autojump
    silver-searcher
    fswatch
    unixtools.netstat
    bfg-repo-cleaner

    openconnect

    # haskellPackages.brittany
    customNodePackages.prettier
    clang-tools

    nix-prefetch-github
    cabal-install
    cabal2nix
    haskellPackages.hpack
    nodePackages.node2nix
    elm2nix


    nodejs_20
    yarn
    customNodePackages.pnpm
    customNodePackages.parcel-bundler
    customNodePackages.deepspeech
    jdk

    git-quick-stats
    customNodePackages.git-stats
    customNodePackages.git-stats-importer
    customNodePackages.river-cli
    pm2

    purescript
    stack
    # spago
    # customNodePackages.lerna

    dotnet-sdk_7

    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format

    python39
    python39Packages.pip
    python39Packages.setuptools

    elixir
    protobuf

    rustc
    cargo
    rustfmt

    haskellPackages.fswatcher
    travis
    awscli2
    kubectl
    vault
    aws-iam-authenticator
    (google-cloud-sdk.withExtraComponents ([google-cloud-sdk.components.cloud-run-proxy]))
    packer
    docker-compose
    #nixops
    terraform
    dbeaver
    graphviz

    zip

    jetbrains.datagrip
  ] ++ ifNixOS [
    # qutebrowser
    slack
    postman 
    teams
  ] ++ linuxOnly [
    xclip 
    (myNeovim neovim)
    gnome3.gnome-tweaks
    simplescreenrecorder
  ];
}
