# shellcheck shell=sh
# Shared steps for bootstrap-ubuntu.sh and bootstrap-nixos.sh. Sourced, not
# run. Expects REPO_ROOT to be set to this repo's folder.

# GNOME starts this at login. post-install.sh deletes it once it has
# finished, so it keeps opening at each login until setup is done.
POST_INSTALL_AUTOSTART="$HOME/.config/autostart/dotfiles-post-install.desktop"

# Opens post-install.sh in Ghostty at the next login. The window stays open
# if the script fails, so the error can be read.
queue_post_install() {
    mkdir -p "$(dirname "$POST_INSTALL_AUTOSTART")"
    cat >"$POST_INSTALL_AUTOSTART" <<EOF
[Desktop Entry]
Type=Application
Name=Finish dotfiles setup
Exec=ghostty --wait-after-command=true -e $REPO_ROOT/post-install.sh
X-GNOME-Autostart-enabled=true
EOF
    printf 'post-install.sh will open at the next login.\n'
}

# reboot_countdown <message>
reboot_countdown() {
    printf '\n%s\n' "$1"
    printf 'Rebooting in 15 seconds. Press Ctrl+C to cancel and reboot later yourself.\n'
    sleep 15
    systemctl reboot
}
