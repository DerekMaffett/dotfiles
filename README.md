# dev-dotfiles

Personal development environment managed with Nix and Home Manager, plus a
few desktop apps and services installed the Ubuntu-native way.

## Fresh Install

This bootstrap currently supports Ubuntu only.

1. Install `git` and `curl`: `sudo apt install -y git curl`
2. Clone this repository over HTTPS (SSH keys don't exist yet):
   `git clone https://github.com/DerekMaffett/dotfiles.git`
3. Run `./install.sh` from the repository root. It asks for your sudo
   password along the way.

`install.sh` will:

1. Install Nix if it is missing.
2. Link the tracked config files into your home directory (`link-configs.sh`).
3. Update Nix channels.
4. Install Home Manager if needed.
5. Apply the Home Manager configuration (`configs/home.nix`): CLI tools,
   language toolchains, Neovim and the `scripts` repo
   (`projects`, `ssh-init`, `nix-github`, ...).
6. Run `install-apps.sh`, which installs apps that don't work well from Nix
   on Ubuntu:
   - Google Chrome (Google's apt repo, amd64 only), set as the default
     browser, and the extensions in
     `configs/chrome-extensions.txt`; Chrome asks once to enable each
   - Docker Engine with the buildx and compose plugins (Docker's official
     apt repo), and adds you to the `docker` group
   - Claude Desktop (Anthropic's apt repo, amd64 and arm64)
   - Slack and Postman (snaps)
   - Ghostty, the terminal (classic snap; config in `configs/ghostty.conf`)

Both scripts are safe to re-run.

## Desktop

- **Default terminal:** `configs/xdg-terminals.list` makes Ghostty the
  terminal for Ctrl+Alt+T and "Open in Terminal".
- **Startup apps:** each `.desktop` file in `configs/autostart/` is linked
  into `~/.config/autostart` and starts at login (currently Claude, Ghostty,
  Chrome and Slack). Add or delete a file there, then re-run
  `./link-configs.sh`. Entries for apps that aren't installed are skipped.

## Assumptions

The Home Manager config derives the username and home directory from the
environment at install time, so it does not require a fixed username.

The bootstrap expects:

1. Ubuntu
2. `curl` and `sh`
3. A user account with sudo rights

## After Install

1. Log out and back in, so the new shell config, desktop entries and
   `docker` group membership take effect.
2. Run `ssh-init`. It generates the keys listed in
   `configs/ssh-init.json` and writes `~/.ssh/config` with the
   `personal.github.com` host alias. Note that it overwrites `~/.ssh/config`.
3. Add the public key to GitHub (for example `gh auth login`, then
   `gh ssh-key add ~/.ssh/id_ecdsa.pub`).
4. Switch this repo to SSH:
   `git remote set-url origin git@personal.github.com:DerekMaffett/dotfiles`
5. Run `p personal` to clone the repos in `configs/workspaces/personal.yml`
   and open the tmux workspace.
6. Sign into apps and services (Chrome, Slack, Postman, `gh`, `gcloud`,
   `aws`, ...).
7. Install anything not managed here that `.bashrc` puts on the `PATH`
   (opencode, Flutter, the Android SDK).

## Updating

To refresh packages and re-apply the config, run:

```sh
system-update
```

That runs `nix-github update`, applies Home Manager again and collects
garbage. Re-run `home-manager switch` after editing any config.
