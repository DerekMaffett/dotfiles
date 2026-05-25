#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT="$SCRIPT_DIR"
CONFIG_ROOT="$REPO_ROOT/configs"

link() {
    src="$CONFIG_ROOT/$1"
    dest="$HOME/$2"

    if [ ! -e "$src" ]; then
        printf 'Missing config source: %s\n' "$src" >&2
        exit 1
    fi

    ln -fvs "$src" "$dest"
}

mkdir -p "$HOME/.config/projects"
mkdir -p "$HOME/.config/brittany"
mkdir -p "$HOME/.config/nvim"
mkdir -p "$HOME/.config/nixpkgs"
mkdir -p "$HOME/.config/nix"
mkdir -p "$HOME/.config/terminator"
mkdir -p "$HOME/.config/kitty"
mkdir -p "$HOME/.config/qutebrowser"
mkdir -p "$HOME/.config/qutebrowser/bookmarks"
mkdir -p "$HOME/.config/home-manager"
mkdir -p "$HOME/.config/direnv"
mkdir -p "$HOME/.xmonad"
mkdir -p "$HOME/.stack"
mkdir -p "$HOME/.ssh"

link .projects.json .config/projects/.projects.json
link brittany.yaml .config/brittany/config.yaml
link nix-config.nix .config/nixpkgs/config.nix
link terminator-config .config/terminator/config
link kitty.conf .config/kitty/kitty.conf
link qutebrowser-config.py .config/qutebrowser/config.py
link qutebrowser-bookmarks .config/qutebrowser/bookmarks/urls
link qutebrowser-quickmarks .config/qutebrowser/quickmarks
link xmonad.hs .xmonad/xmonad.hs
link stack.yaml .stack/config.yaml
link home.nix .config/home-manager/home.nix
link direnv.toml .config/direnv/direnv.toml

link .Xresources .Xresources
link .agignore .agignore
link .gitconfig .gitconfig
link .prettierrc.js .prettierrc.js
link .tmux.conf .tmux.conf
link .zshrc .zshrc
link .bashrc .bashrc
link ssh-init.json .ssh/ssh-init.json
link .nix-channels .nix-channels
