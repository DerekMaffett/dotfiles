#!/bin/sh

# Sets up Ubuntu: Nix and Home Manager for the user environment, then the
# desktop apps and services, then reboots. Run by bootstrap.sh, or by hand
# from the clone. Safe to re-run.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT="$SCRIPT_DIR"
NIX_PROFILE_SCRIPT="$HOME/.nix-profile/etc/profile.d/nix.sh"

. "$REPO_ROOT/bootstrap-common.sh"

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
    printf 'This is not Ubuntu. Use bootstrap.sh, which picks the right script.\n' >&2
    exit 1
fi

. /etc/os-release

if [ "${ID-}" != "ubuntu" ]; then
    printf 'This is %s, not Ubuntu. Use bootstrap.sh, which picks the right script.\n' "${ID-unknown}" >&2
    exit 1
fi

require_command curl
require_command sh

# Single-user Nix builds in $TMPDIR. Ubuntu's /tmp is a RAM-backed tmpfs where
# systemd caps each user at 80% of its size, which is too small for big
# from-source builds such as terraform (unfree, so not in the binary cache).
# Build on disk instead. Nix deletes each build folder when it finishes.
TMPDIR="$HOME/.cache/nix-build"
export TMPDIR
mkdir -p "$TMPDIR"

# Single-user Nix (no daemon), which is what NIX_PROFILE_SCRIPT assumes.
# --yes accepts the installer's prompts; it still asks for sudo once to
# create /nix. Downloaded to a file first so a failed download stops here
# instead of piping an empty script into sh.
if [ ! -f "$NIX_PROFILE_SCRIPT" ]; then
    nix_installer=$(mktemp)
    curl -fsSL -o "$nix_installer" https://nixos.org/nix/install
    sh "$nix_installer" --no-daemon --yes
    rm -f "$nix_installer"
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

"$REPO_ROOT/install-apps.sh"

queue_post_install

# A fresh login picks up the shell config, launcher entries and docker group.
reboot_countdown "Ubuntu is set up. After the reboot, log in and post-install.sh opens to finish setup (SSH key, GitHub login)."
