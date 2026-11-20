# dev-dotfiles

Personal development environment managed with Nix and Home Manager, plus a
few desktop apps and services installed the Ubuntu-native way.

This bootstrap supports Ubuntu only. It expects a user account with sudo
rights; the username and home folder are read from the environment, so any
username works.

## Fresh Install

On a fresh Ubuntu install, open a terminal and run:

```sh
sh -c "$(wget -qO- https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
```

`bootstrap.sh` installs `git` and `curl`, clones this repo over HTTPS to
`~/dotfiles` (set `DOTFILES_DIR` to use another folder), then runs
`install.sh`. It uses `wget` because Ubuntu always has it, and downloads the
whole script before running it. Re-running it updates the existing clone and
carries on. It asks for your sudo password (for `git` and `curl` if missing,
to create `/nix`, then again for the apps).

To do the same by hand: install `git` and `curl`, clone this repo anywhere
except `~/projects/personal/dotfiles` (`link-configs.sh` puts a link back to
the repo there), and run `install.sh` from the clone.

`install.sh` will:

1. Install Nix (single-user, no daemon) if it is missing.
2. Link the tracked config files into your home directory (`link-configs.sh`).
3. Update Nix channels and install Home Manager if needed.
4. Apply the Home Manager configuration (`configs/home.nix`): CLI tools,
   language toolchains, Neovim, the `scripts` repo (`projects`, `ssh-init`,
   `nix-github`, ...) and GNOME settings (Caps Lock as Ctrl).
5. Run `install-apps.sh`, which installs apps that don't work well from Nix
   on Ubuntu:
   - Google Chrome (Google's apt repo, amd64 only), set as the default
     browser, plus the extensions in `configs/chrome-extensions.txt`
   - Docker Engine with the buildx and compose plugins (Docker's official
     apt repo), and adds you to the `docker` group
   - Claude Desktop (Anthropic's apt repo, amd64 and arm64)
   - Slack and Postman (snaps)
   - Ghostty, the terminal (classic snap; config in `configs/ghostty.conf`)

All scripts are safe to re-run.

## After Install

1. Log out and back in. This picks up the shell config, launcher entries and
   `docker` group, and starts the login apps.
2. Enable the Chrome extensions: Chrome asks once for each new one.
3. Run `~/dotfiles/post-install.sh`. It:
   - runs `ssh-init`, which generates the keys in `configs/ssh-init.json`
     (asks for a passphrase) and writes `~/.ssh/config` with the
     `personal.github.com` alias (skipped if `~/.ssh/id_ecdsa` exists)
   - adds GitHub's published SSH host keys to `~/.ssh/known_hosts`
   - logs in to GitHub with `gh` (in the browser) and uploads the key
   - checks that SSH to GitHub works
   - switches this repo's remote to SSH
   - runs `p personal`, which clones the repos in
     `configs/workspaces/personal.yml` and opens the tmux workspace
4. Sign in to apps and services: Chrome (sync), Slack, Postman, Claude,
   LastPass, `gcloud auth login`, `aws configure`, ...
5. Install anything not managed here that `.bashrc` puts on the `PATH`:
   Flutter in `~/Downloads/flutter` and the Android SDK.

## Desktop

- **Default terminal:** `configs/xdg-terminals.list` makes Ghostty the
  terminal for Ctrl+Alt+T and "Open in Terminal".
- **Startup apps:** each `.desktop` file in `configs/autostart/` is linked
  into `~/.config/autostart` and starts at login (currently Claude, Ghostty,
  Chrome and Slack). Add or delete a file there, then re-run
  `./link-configs.sh`. Entries for apps that aren't installed are skipped.
- **GNOME settings:** `dconf.settings` in `configs/home.nix`. Home Manager
  re-applies them on every switch, so changes made in GNOME Settings to
  those keys don't last.
- **Chrome extensions:** add a Web Store ID to
  `configs/chrome-extensions.txt` (take it from the extension's own website
  or GitHub, not a store search) and re-run `./install-apps.sh`. Removing a
  line doesn't uninstall the extension.

## Updating

To refresh packages and re-apply the config, run:

```sh
system-update
```

That runs `nix-github update`, applies Home Manager again and collects
garbage. Re-run `home-manager switch` after editing any config.
