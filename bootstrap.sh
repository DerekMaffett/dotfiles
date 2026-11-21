#!/bin/sh

# Sets up a fresh machine from nothing. On Ubuntu:
#   sh -c "$(wget -qO- https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
# On NixOS:
#   sh -c "$(curl -fsSL https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
# Clones this repo to ~/dotfiles (or $DOTFILES_DIR), then hands over to
# bootstrap-ubuntu.sh or bootstrap-nixos.sh. Set DOTFILES_BRANCH to clone a
# branch other than master. Safe to re-run: an existing clone is updated
# instead.

set -eu

REPO_URL="https://github.com/DerekMaffett/dotfiles.git"
DEST="${DOTFILES_DIR:-$HOME/dotfiles}"
BRANCH="${DOTFILES_BRANCH:-master}"

# Everything is inside main, which only runs on the last line, so a partly
# downloaded script does nothing instead of running half the steps.
main() {
    if [ ! -f /etc/os-release ]; then
        printf 'Cannot tell which Linux this is (no /etc/os-release).\n' >&2
        exit 1
    fi
    . /etc/os-release

    case "${ID-}" in
        ubuntu)
            missing=""
            for cmd in git curl; do
                command -v "$cmd" >/dev/null 2>&1 || missing="$missing $cmd"
            done
            if [ -n "$missing" ]; then
                printf 'Installing%s\n' "$missing"
                sudo apt-get update
                # shellcheck disable=SC2086
                sudo apt-get install -y $missing
            fi
            git=git
            ;;
        nixos)
            # A fresh NixOS has no git until the flake is applied, so borrow
            # one from nixpkgs for the clone.
            git="$(nix-build '<nixpkgs>' -A git --no-out-link)/bin/git"
            ;;
        *)
            printf 'This bootstrap supports Ubuntu and NixOS (found %s).\n' "${ID-unknown}" >&2
            exit 1
            ;;
    esac

    if [ -d "$DEST/.git" ]; then
        printf 'Using existing clone at %s\n' "$DEST"
        "$git" -C "$DEST" pull --ff-only
    elif [ -e "$DEST" ]; then
        printf '%s exists but is not a git clone; move it aside first.\n' "$DEST" >&2
        exit 1
    else
        "$git" clone --branch "$BRANCH" "$REPO_URL" "$DEST"
    fi

    exec "$DEST/bootstrap-$ID.sh"
}

main "$@"
