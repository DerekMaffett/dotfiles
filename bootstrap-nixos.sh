#!/bin/sh

# Turns a fresh NixOS install (from the graphical installer) into the full
# setup by applying this repo's flake, then reboots into it. Run by
# bootstrap.sh, or by hand from the clone. Safe to re-run.
#
# The flake defines the user derek, so make that the login name in the
# installer.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT="$SCRIPT_DIR"
HOST="derek"
FLAKE_USER="derek"
HW_CONFIG="$REPO_ROOT/nixos/hosts/$HOST/hardware-configuration.nix"

. "$REPO_ROOT/bootstrap-common.sh"

if [ ! -e /etc/NIXOS ]; then
    printf 'This is not NixOS. Use bootstrap.sh, which picks the right script.\n' >&2
    exit 1
fi

if [ "$(id -un)" != "$FLAKE_USER" ]; then
    printf 'The flake sets up the user %s, but you are logged in as %s.\n' "$FLAKE_USER" "$(id -un)" >&2
    printf 'Reinstall with %s as the login name, or change the user in nixos/hosts/%s/configuration.nix.\n' \
        "$FLAKE_USER" "$HOST" >&2
    exit 1
fi

# The installer's hardware config describes this machine's disks and kernel
# modules. The repo's copy is a placeholder until it's replaced.
if ! cmp -s /etc/nixos/hardware-configuration.nix "$HW_CONFIG"; then
    cp /etc/nixos/hardware-configuration.nix "$HW_CONFIG"
    printf 'Copied this machine'\''s hardware config to %s (commit it to keep it).\n' "$HW_CONFIG"
fi

# Builds the system and makes it the default from the next boot, without
# switching the running desktop over: doing that leaves GNOME's lock screen
# unable to check passwords until a reboot. Flakes aren't enabled on a fresh
# install, so they're turned on for this run. The path: form reads the
# folder as it is, so no git is needed. Suspend is held off while it builds.
systemd-inhibit --what=sleep:idle --who=dotfiles --why="Building the NixOS system" \
    sudo env NIX_CONFIG='experimental-features = nix-command flakes' \
    nixos-rebuild boot --flake "path:$REPO_ROOT#$HOST"

quiet_vm_screen
queue_post_install

reboot_countdown "NixOS is set up. After the reboot, log in and post-install.sh opens to finish setup (SSH key, GitHub login)."
