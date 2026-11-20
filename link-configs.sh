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

mkdir -p "$HOME/projects/personal"
mkdir -p "$HOME/.config/nvim"
mkdir -p "$HOME/.config/nixpkgs"
mkdir -p "$HOME/.config/nix"
mkdir -p "$HOME/.config/ghostty"
mkdir -p "$HOME/.config/home-manager"
mkdir -p "$HOME/.config/direnv"
mkdir -p "$HOME/.config/opencode"
mkdir -p "$HOME/.ssh"
mkdir -p "$HOME/.config/autostart"

link workspaces/personal.yml projects/personal/workspace.yml

# Make the repo show up in the personal workspace. If it was cloned straight
# into that path there's nothing to link; if a different real folder is
# there, linking would put a link inside it instead, so stop.
workspace_link="$HOME/projects/personal/dotfiles"
if [ -L "$workspace_link" ] || [ ! -e "$workspace_link" ]; then
    ln -sfvn "$REPO_ROOT" "$workspace_link"
elif [ "$(CDPATH= cd -- "$workspace_link" && pwd -P)" != "$(CDPATH= cd -- "$REPO_ROOT" && pwd -P)" ]; then
    printf '%s is a separate folder, not this repo (%s). Move it aside and re-run.\n' \
        "$workspace_link" "$REPO_ROOT" >&2
    exit 1
fi

link nix-config.nix .config/nixpkgs/config.nix
link ghostty.conf .config/ghostty/config.ghostty
link xdg-terminals.list .config/xdg-terminals.list
link home.nix .config/home-manager/home.nix
link direnv.toml .config/direnv/direnv.toml
link opencode.jsonc .config/opencode/opencode.jsonc

# Apps started at login. Each file is linked on its own, so apps that add
# their own entry to ~/.config/autostart don't write into this repo.
for entry in "$CONFIG_ROOT"/autostart/*.desktop; do
    name=$(basename "$entry")
    link "autostart/$name" ".config/autostart/$name"
done

link .gitconfig .gitconfig
link .prettierrc.js .prettierrc.js
link .tmux.conf .tmux.conf
link .bashrc .bashrc
link ssh-init.json .ssh/ssh-init.json
link .nix-channels .nix-channels
