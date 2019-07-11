with import <nixpkgs> {};

let
    customNodePackages = import ./nodepkgs/default.nix { inherit (pkgs) nodejs pkgs; };
in buildEnv {
    name = "workEnv";
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

        customNodePackages.prettier

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
}
