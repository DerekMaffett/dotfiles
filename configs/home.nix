{ config, pkgs, ... }:

let
  currentUsername = let user = builtins.getEnv "USER"; in
    if user != "" then user else throw "USER must be set when evaluating home.nix";
  currentHome = let home = builtins.getEnv "HOME"; in
    if home != "" then home else throw "HOME must be set when evaluating home.nix";
  scripts = import "${(fromGithubMaster "scripts")}/default.nix";

  python3Host = pkgs.python3.withPackages (ps: [ ps.pynvim ]);
  vimrc = ''
    let g:python3_host_prog = '${python3Host}/bin/python3'

  '' + import ./.vimrc.vim;

  linuxOnly = derivations: if pkgs.stdenv.hostPlatform.isLinux then derivations else [];

  fromGithubMaster = name: pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github-pkgs/compiled-github-pkgs.json))."${name}";
  copyToShare = { name, src, dir ? "" }: pkgs.stdenv.mkDerivation {
    inherit name src dir;
    installPhase = ''
      mkdir -p $out/share/
      cp -r $src/$dir $out/share/$name
    '';
  };

  # Packages

  system-update = pkgs.writeShellScriptBin "system-update" ''
    set -euo pipefail

    nix-github update
    home-manager switch
    nix-collect-garbage
  '';

  bash-git-prompt = copyToShare {
      name = "bash-git-prompt";
      src = fromGithubMaster "bash-git-prompt";
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
  vim-graphql = pkgs.vimUtils.buildVimPlugin {
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
          fzf-wrapper
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
          nvim-cmp
          cmp-buffer
          cmp-path
          cmp-cmdline
          cmp-nvim-lsp
          yats-vim 
          vim-javascript
          vim-jsx-pretty
          purescript-vim 
          Jenkinsfile-vim-syntax
          psc-ide-vim
          vim-fireplace
          elm-vim
          vim-terraform
          # elixir-tools
          # vim-elixir
          rustaceanvim
          nvim-treesitter
          markdown-preview-nvim
        ];
        opt = [ ];
      }; 
    };    
  };

in
{
  home.username = currentUsername;
  home.homeDirectory = currentHome;

  home.stateVersion = "25.11";

  programs.home-manager.enable = true;

  # GNOME settings, applied on every switch. Changing these in GNOME
  # Settings only lasts until the next switch.
  dconf.settings = {
    # Caps Lock is an extra Ctrl.
    "org/gnome/desktop/input-sources".xkb-options = [ "ctrl:nocaps" ];
  };

  # GNOME doesn't notice new apps in ~/.nix-profile/share/applications, since
  # that path is a symlink into the nix store that gets swapped rather than
  # modified. Mirror the entries into ~/.local/share/applications, which it
  # does watch. Only symlinks into /nix/store are managed here.
  home.activation.linkDesktopEntries = config.lib.dag.entryAfter [ "installPackages" ] ''
    appsDir="$HOME/.local/share/applications"
    run mkdir -p "$appsDir"
    for f in "$appsDir"/*.desktop; do
      if [ -L "$f" ] && [[ "$(readlink "$f")" == /nix/store/* ]]; then
        run rm "$f"
      fi
    done
    for f in "$HOME"/.nix-profile/share/applications/*.desktop; do
      name="$(basename "$f")"
      if [ ! -e "$appsDir/$name" ]; then
        run ln -s "$(readlink -f "$f")" "$appsDir/$name"
      fi
    done
  '';

#   programs.vscode = {
#   enable = true;
#   profiles.default.extensions = with pkgs.vscode-extensions; [
#     dracula-theme.theme-dracula
#     vscodevim.vim
#     yzhang.markdown-all-in-one
#     github.copilot
#     github.copilot-chat
#
#   ];
# };

  home.packages = with pkgs; [
    git
    curl

    system-update
    scripts

    wmctrl

    cloc
    jq

    go-task
    gnumake
    direnv
    tmux
    tmuxPlugins.vim-tmux-navigator
    tmuxinator
    fzf
    bash-git-prompt
    autojump
    ripgrep
    fswatch
    watchman
    unixtools.netstat
    bfg-repo-cleaner

    openconnect

    # haskellPackages.brittany
    prettier
    clang-tools

    nix-prefetch-github
    cabal-install
    cabal2nix
    haskellPackages.hpack
    elm2nix


    nodejs_24
    yarn
    pnpm
    jdk17
    # android-studio

    git-quick-stats
    pm2

    purescript
    stack
    # spago
    # lerna

    dotnet-sdk

    # elmPackages.elm
    # elmPackages.elm-test
    # elmPackages.elm-format

    python3
    python3Packages.pip
    python3Packages.setuptools

    beamPackages.elixir
    protobuf

    go_latest
    gopls

    rustc
    cargo
    cargo-watch
    rustfmt
    bacon

    maestro

    sqlite

    awscli2
    kubectl
    # vault
    aws-iam-authenticator
    (google-cloud-sdk.withExtraComponents ([google-cloud-sdk.components.cloud-run-proxy]))
    packer
    #nixops
    terraform
    graphviz

    zip

    gh
  ] ++ linuxOnly [
    xclip
    (myNeovim neovim)
    gnome-tweaks
    simplescreenrecorder
  ];
}
