#!/bin/sh

# Sets up a fresh Ubuntu machine from nothing:
#   sh -c "$(wget -qO- https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
# Installs git and curl, clones this repo to ~/dotfiles (or $DOTFILES_DIR),
# then runs install.sh. Safe to re-run: an existing clone is updated instead.

set -eu

REPO_URL="https://github.com/DerekMaffett/dotfiles.git"
DEST="${DOTFILES_DIR:-$HOME/dotfiles}"

# Everything is inside main, which only runs on the last line, so a partly
# downloaded script does nothing instead of running half the steps.
main() {
    if [ ! -f /etc/os-release ]; then
        printf 'This bootstrap only supports Ubuntu.\n' >&2
        exit 1
    fi
    . /etc/os-release
    if [ "${ID-}" != "ubuntu" ]; then
        printf 'This bootstrap only supports Ubuntu (found %s).\n' "${ID-unknown}" >&2
        exit 1
    fi

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

    if [ -d "$DEST/.git" ]; then
        printf 'Using existing clone at %s\n' "$DEST"
        git -C "$DEST" pull --ff-only
    elif [ -e "$DEST" ]; then
        printf '%s exists but is not a git clone; move it aside first.\n' "$DEST" >&2
        exit 1
    else
        git clone "$REPO_URL" "$DEST"
    fi

    exec "$DEST/install.sh"
}

main "$@"
