#!/bin/sh

# Finishes setup after the bootstrap's reboot: SSH key, GitHub login and key
# upload, this repo's remote, then the personal workspace. The bootstrap
# makes it open by itself at login until it has finished once. Safe to
# re-run: finished steps are skipped.
#
# It needs you for the key passphrase and the GitHub sign-in in the browser.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Written by queue_post_install in bootstrap-common.sh.
POST_INSTALL_AUTOSTART="$HOME/.config/autostart/dotfiles-post-install.desktop"

KEY="$HOME/.ssh/id_ecdsa"
SSH_HOST="personal.github.com"
DOTFILES_SSH_URL="git@$SSH_HOST:DerekMaffett/dotfiles"
WORKSPACE="personal"

require_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        printf 'Missing required command: %s (has home-manager switch run?)\n' "$1" >&2
        exit 1
    fi
}

step() {
    printf '\n==> %s\n' "$1"
}

for cmd in ssh-init ssh ssh-keygen gh git projects; do
    require_command "$cmd"
done

step "SSH key and ~/.ssh/config"
if [ -f "$KEY" ]; then
    printf '%s already exists, skipping ssh-init\n' "$KEY"
    if ! grep -q "^Host $SSH_HOST\$" "$HOME/.ssh/config" 2>/dev/null; then
        # ssh-init would try to regenerate the key, so don't run it here.
        printf '~/.ssh/config has no "Host %s" entry. Add it, or move %s aside and re-run.\n' \
            "$SSH_HOST" "$KEY" >&2
        exit 1
    fi
else
    # Generates the keys in ~/.ssh/ssh-init.json (asks for a passphrase) and
    # writes ~/.ssh/config with the personal.github.com alias.
    ssh-init
fi
chmod 700 "$HOME/.ssh"
chmod 600 "$HOME/.ssh/config"

step "GitHub login"
if gh auth status --hostname github.com >/dev/null 2>&1; then
    printf 'Already logged in, skipping\n'
else
    gh auth login --hostname github.com --git-protocol ssh --web --skip-ssh-key \
        --scopes admin:public_key
fi
# An older login may lack the scope needed to manage SSH keys.
if ! gh api user/keys >/dev/null 2>&1; then
    gh auth refresh --hostname github.com --scopes admin:public_key
fi

step "GitHub's SSH host keys"
if ssh-keygen -F github.com >/dev/null 2>&1; then
    printf 'github.com already in known_hosts, skipping\n'
else
    # Taken from GitHub's API over HTTPS, so the first SSH connection
    # doesn't have to trust whatever key it's shown. Needs the login above:
    # gh makes no API calls before it. Fetched into a variable first so a
    # failed or empty answer stops here instead of adding nothing and
    # carrying on.
    host_keys=$(gh api meta --jq '.ssh_keys[]')
    if [ -z "$host_keys" ]; then
        printf 'GitHub returned no SSH host keys, stopping.\n' >&2
        exit 1
    fi
    printf '%s\n' "$host_keys" | sed 's/^/github.com /' >>"$HOME/.ssh/known_hosts"
    printf 'Added GitHub host keys to ~/.ssh/known_hosts\n'
fi

step "Upload SSH key to GitHub"
public_key=$(cut -d' ' -f1,2 "$KEY.pub")
if gh api user/keys --jq '.[].key' | grep -qxF "$public_key"; then
    printf 'Key already on GitHub, skipping\n'
else
    gh ssh-key add "$KEY.pub" --title "$(hostname)"
fi

step "Check SSH access"
ssh_output=$(ssh -T "git@$SSH_HOST" 2>&1 || true)
case $ssh_output in
    *"successfully authenticated"*)
        printf 'SSH to GitHub works\n'
        ;;
    *)
        printf 'SSH to GitHub failed:\n%s\n' "$ssh_output" >&2
        exit 1
        ;;
esac

step "Switch this repo to SSH"
current_url=$(git -C "$SCRIPT_DIR" remote get-url origin)
if [ "$current_url" = "$DOTFILES_SSH_URL" ]; then
    printf 'Already using %s, skipping\n' "$DOTFILES_SSH_URL"
else
    git -C "$SCRIPT_DIR" remote set-url origin "$DOTFILES_SSH_URL"
    printf 'origin: %s -> %s\n' "$current_url" "$DOTFILES_SSH_URL"
fi

# Setup is done, so stop opening this at login.
rm -f "$POST_INSTALL_AUTOSTART"

step "Open the $WORKSPACE workspace"
# Clones the workspace repos that are missing, then starts tmux.
exec projects "$WORKSPACE"
