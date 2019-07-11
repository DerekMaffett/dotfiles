with import <nixpkgs> {};

buildEnv {
    name = "workEnv";
    paths = with pkgs; [
        cowsay
        direnv
        neovim
        tmux
        jq
        zsh
        oh-my-zsh
        autojump
        cloc
        silver-searcher
        purescript
        stack
        travis
        awscli
    ];
}
