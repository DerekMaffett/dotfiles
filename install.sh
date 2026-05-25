#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT="$SCRIPT_DIR"
NIX_PROFILE_SCRIPT="$HOME/.nix-profile/etc/profile.d/nix.sh"

require_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        printf 'Missing required command: %s\n' "$1" >&2
        exit 1
    fi
}

refresh_nix_environment() {
    if [ ! -f "$NIX_PROFILE_SCRIPT" ]; then
        printf 'Missing Nix profile script: %s\n' "$NIX_PROFILE_SCRIPT" >&2
        exit 1
    fi

    . "$NIX_PROFILE_SCRIPT"
}

if [ ! -f /etc/os-release ]; then
    printf 'This bootstrap only supports Ubuntu.\n' >&2
    exit 1
fi

. /etc/os-release

if [ "${ID-}" != "ubuntu" ]; then
    printf 'Unsupported distribution: %s. This bootstrap only supports Ubuntu.\n' "${ID-unknown}" >&2
    exit 1
fi

require_command curl
require_command sh

if [ ! -f "$NIX_PROFILE_SCRIPT" ]; then
    curl -L https://nixos.org/nix/install | sh
fi

if [ ! -f "$NIX_PROFILE_SCRIPT" ]; then
    printf 'Nix install completed but %s was not created.\n' "$NIX_PROFILE_SCRIPT" >&2
    exit 1
fi

refresh_nix_environment

require_command nix-channel
require_command nix-shell

"$REPO_ROOT/link-configs.sh"

nix-channel --update

if ! command -v home-manager >/dev/null 2>&1; then
    nix-shell '<home-manager>' -A install
    refresh_nix_environment
fi

require_command home-manager

home-manager switch
