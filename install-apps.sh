#!/bin/sh

# Installs desktop apps and system services on Ubuntu outside of Nix. These
# need the system's sandbox (AppArmor), GPU driver setup or a root daemon,
# which Nix-installed copies don't get on Ubuntu. Safe to re-run: installed
# apps are skipped.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

SNAPS="slack postman"
# Classic snaps run unconfined, like a normal app. Ghostty needs this to
# launch your shell and tools with full access to the system.
CLASSIC_SNAPS="ghostty"

ARCH=$(dpkg --print-architecture)

# Google's Linux package signing key. The fingerprint is pinned so a
# tampered download is rejected. See https://www.google.com/linuxrepositories/
CHROME_KEY_URL="https://dl.google.com/linux/linux_signing_key.pub"
CHROME_KEY_FINGERPRINT="EB4C1BFD4F042F6DDDCCEC917721F63BD38B4796"

# These paths and the repo entry match what Chrome's own package writes, so
# its updates overwrite them with the same content instead of conflicting.
CHROME_KEYRING="/usr/share/keyrings/google-chrome.gpg"
CHROME_SOURCES="/etc/apt/sources.list.d/google-chrome.sources"

# Chrome installs extensions listed here from the Web Store at startup and
# asks once to enable each. See
# https://developer.chrome.com/docs/extensions/how-to/distribute/install-extensions
CHROME_EXTENSIONS_LIST="$SCRIPT_DIR/configs/chrome-extensions.txt"
CHROME_EXTENSIONS_DIR="/usr/share/google-chrome/extensions"
CHROME_WEBSTORE_UPDATE_URL="https://clients2.google.com/service/update2/crx"

# Docker's official apt repo, set up as in
# https://docs.docker.com/engine/install/ubuntu/ with the key fingerprint pinned.
DOCKER_KEY_URL="https://download.docker.com/linux/ubuntu/gpg"
DOCKER_KEY_FINGERPRINT="9DC858229FC7DD38854AE2D88D81803C0EBFCD88"
DOCKER_KEYRING="/etc/apt/keyrings/docker.gpg"
DOCKER_SOURCES="/etc/apt/sources.list.d/docker.sources"
DOCKER_PACKAGES="docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin"

# Anthropic's apt repo for the Claude desktop app (Linux beta). Paths,
# repo line and fingerprint are from
# https://code.claude.com/docs/en/desktop-linux. There's no official snap.
CLAUDE_KEY_URL="https://downloads.claude.ai/claude-desktop/key.asc"
CLAUDE_KEY_FINGERPRINT="31DDDE24DDFAB679F42D7BD2BAA929FF1A7ECACE"
CLAUDE_KEYRING="/usr/share/keyrings/claude-desktop-archive-keyring.asc"
CLAUDE_SOURCES="/etc/apt/sources.list.d/claude-desktop.list"

require_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        printf 'Missing required command: %s\n' "$1" >&2
        exit 1
    fi
}

# add_apt_repo <name> <key url> <key fingerprint> <keyring path> <sources path> <sources body>
# Downloads a repo signing key, rejects it unless it matches the pinned
# fingerprint, then installs the keyring and sources file. A keyring path
# ending in .asc keeps the key armored; anything else gets the binary form.
add_apt_repo() {
    name=$1 key_url=$2 key_fingerprint=$3 keyring=$4 sources=$5 sources_body=$6

    tmp_dir=$(mktemp -d)
    trap 'rm -rf "$tmp_dir"' EXIT

    curl -fsSL -o "$tmp_dir/key.pub" "$key_url"
    fingerprints=$(gpg --show-keys --with-colons "$tmp_dir/key.pub" |
        awk -F: '$1 == "pub" { want = 1; next } want && $1 == "fpr" { print $10; want = 0 }')
    if [ "$fingerprints" != "$key_fingerprint" ]; then
        printf '%s signing key does not match the pinned fingerprint, aborting.\n' "$name" >&2
        printf 'Expected: %s\nGot: %s\n' "$key_fingerprint" "$fingerprints" >&2
        exit 1
    fi
    case $keyring in
        *.asc) cp "$tmp_dir/key.pub" "$tmp_dir/keyring" ;;
        *) gpg --dearmor -o "$tmp_dir/keyring" "$tmp_dir/key.pub" ;;
    esac
    printf '%s\n' "$sources_body" >"$tmp_dir/repo.sources"

    sudo install -D -m 644 "$tmp_dir/keyring" "$keyring"
    sudo install -D -m 644 "$tmp_dir/repo.sources" "$sources"
    rm -rf "$tmp_dir"
    trap - EXIT
}

# The repo is set up even when Chrome is already installed: Chrome only
# updates through apt, and the repo can be missing (Chrome installed from a
# downloaded .deb, or the sources file dropped by an Ubuntu release upgrade).
install_chrome() {
    add_apt_repo "Google" "$CHROME_KEY_URL" "$CHROME_KEY_FINGERPRINT" \
        "$CHROME_KEYRING" "$CHROME_SOURCES" "Types: deb
URIs: https://dl.google.com/linux/chrome-stable/deb/
Suites: stable
Components: main
Architectures: amd64
Signed-By: $CHROME_KEYRING"

    if dpkg -s google-chrome-stable >/dev/null 2>&1; then
        printf 'google-chrome-stable already installed, skipping (apt upgrade updates it)\n'
        return
    fi

    sudo apt-get update
    sudo apt-get install -y google-chrome-stable
}

# Writes one <id>.json per extension in chrome-extensions.txt. Re-running
# rewrites them with the same content; Chrome skips ones already installed.
install_chrome_extensions() {
    ids=$(sed 's/#.*//' "$CHROME_EXTENSIONS_LIST" | tr -s ' \t' '\n' | grep -v '^$' || true)
    for id in $ids; do
        if ! printf '%s' "$id" | grep -qx '[a-p]\{32\}'; then
            printf 'Not a Chrome extension ID in %s: %s\n' "$CHROME_EXTENSIONS_LIST" "$id" >&2
            exit 1
        fi
    done

    sudo mkdir -p "$CHROME_EXTENSIONS_DIR"
    for id in $ids; do
        printf '{ "external_update_url": "%s" }\n' "$CHROME_WEBSTORE_UPDATE_URL" |
            sudo tee "$CHROME_EXTENSIONS_DIR/$id.json" >/dev/null
        sudo chmod 644 "$CHROME_EXTENSIONS_DIR/$id.json"
    done
    printf 'Chrome extensions queued: %s (enable them when Chrome asks)\n' "$(echo $ids | wc -w)"
}

# Default for web links and HTML files, for the current user only (written
# to ~/.config/mimeapps.list).
set_default_browser() {
    if [ "$(xdg-settings get default-web-browser 2>/dev/null)" = "$1" ]; then
        printf '%s is already the default browser, skipping\n' "$1"
        return
    fi
    xdg-settings set default-web-browser "$1"
    printf 'Default browser set to %s\n' "$1"
}

install_claude() {
    if dpkg -s claude-desktop >/dev/null 2>&1; then
        printf 'claude-desktop already installed, skipping\n'
        return
    fi

    add_apt_repo "Claude" "$CLAUDE_KEY_URL" "$CLAUDE_KEY_FINGERPRINT" \
        "$CLAUDE_KEYRING" "$CLAUDE_SOURCES" \
        "deb [arch=amd64,arm64 signed-by=$CLAUDE_KEYRING] https://downloads.claude.ai/claude-desktop/apt/stable stable main"

    sudo apt-get update
    sudo apt-get install -y claude-desktop
}

# The `claude` command. The desktop app keeps its own copy under
# ~/.config/Claude that isn't on PATH. This is Anthropic's native installer
# (https://code.claude.com/docs/en/setup): it installs to ~/.local/bin for
# the current user and updates itself, so a re-run skips it.
install_claude_code() {
    if [ -x "$HOME/.local/bin/claude" ]; then
        printf 'claude already installed, skipping (it updates itself)\n'
        return
    fi

    tmp_dir=$(mktemp -d)
    trap 'rm -rf "$tmp_dir"' EXIT
    curl -fsSL -o "$tmp_dir/install.sh" https://claude.ai/install.sh
    bash "$tmp_dir/install.sh"
    rm -rf "$tmp_dir"
    trap - EXIT
}

install_docker() {
    if dpkg -s docker-ce >/dev/null 2>&1; then
        printf 'docker-ce already installed, skipping\n'
    else
        . /etc/os-release
        add_apt_repo "Docker" "$DOCKER_KEY_URL" "$DOCKER_KEY_FINGERPRINT" \
            "$DOCKER_KEYRING" "$DOCKER_SOURCES" "Types: deb
URIs: https://download.docker.com/linux/ubuntu
Suites: ${UBUNTU_CODENAME:-$VERSION_CODENAME}
Components: stable
Architectures: $ARCH
Signed-By: $DOCKER_KEYRING"

        sudo apt-get update
        # shellcheck disable=SC2086
        sudo apt-get install -y $DOCKER_PACKAGES
    fi

    # Run docker without sudo. Takes effect at the next login.
    if ! id -nG "$USER" | tr ' ' '\n' | grep -qx docker; then
        sudo usermod -aG docker "$USER"
        printf 'Added %s to the docker group; log out and back in to use docker without sudo.\n' "$USER"
    fi
}

# install_snaps <names> [snap install flags...]
install_snaps() {
    names=$1
    shift
    for name in $names; do
        if snap list "$name" >/dev/null 2>&1; then
            printf '%s already installed, skipping\n' "$name"
        else
            sudo snap install "$name" "$@"
        fi
    done
}

require_command curl
require_command gpg
require_command dpkg
require_command apt-get
require_command snap
require_command sudo
require_command xdg-settings

if [ "$ARCH" = "amd64" ]; then
    install_chrome
    install_chrome_extensions
    set_default_browser google-chrome.desktop
else
    printf 'Skipping Google Chrome: no Linux build for %s\n' "$ARCH" >&2
fi

case $ARCH in
    amd64 | arm64)
        install_claude
        install_claude_code
        ;;
    *) printf 'Skipping Claude Desktop and Claude Code: no Linux build for %s\n' "$ARCH" >&2 ;;
esac

install_docker
install_snaps "$SNAPS"
install_snaps "$CLASSIC_SNAPS" --classic
