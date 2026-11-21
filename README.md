# dev-dotfiles

Personal development environment for Ubuntu or NixOS. Home Manager
(`configs/home.nix`) sets up the user environment on both. On NixOS the
flake (`flake.nix`, `nixos/`) also sets up the whole system; on Ubuntu,
scripts install the desktop apps and services the Ubuntu-native way.

## Fresh Install

Install the OS, log in, open a terminal and run one command. It asks for
your sudo password, sets everything up and reboots. At the next login a
terminal opens by itself with `post-install.sh`, which only needs the SSH
key passphrase and a GitHub sign-in in the browser.

**Ubuntu** (any username):

```sh
sh -c "$(wget -qO- https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
```

**NixOS**, from the graphical installer with GNOME and unfree software
allowed. Use `derek` as the login name: the flake defines that user.

```sh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/DerekMaffett/dotfiles/master/bootstrap.sh)"
```

Each OS has what its command uses (`wget` on Ubuntu, `curl` on NixOS). The
whole script downloads before anything runs.

`bootstrap.sh` clones this repo over HTTPS to `~/dotfiles` (set
`DOTFILES_DIR` to use another folder, `DOTFILES_BRANCH` for another
branch), then runs `bootstrap-ubuntu.sh` or `bootstrap-nixos.sh`. All of
the scripts are safe to re-run; an existing clone is updated.

### What `bootstrap-ubuntu.sh` does

1. Installs Nix (single-user, no daemon) if it is missing.
2. Links the tracked config files into your home directory (`link-configs.sh`).
3. Updates Nix channels and installs Home Manager if needed.
4. Applies the Home Manager configuration (`configs/home.nix`): CLI tools,
   language toolchains, Neovim, the `scripts` repo (`projects`, `ssh-init`,
   `nix-github`, ...) and GNOME settings (Caps Lock as Ctrl).
5. Runs `install-apps.sh`, which installs apps that don't work well from
   Nix on Ubuntu:
   - Google Chrome (Google's apt repo, amd64 only), set as the default
     browser, plus the extensions in `configs/chrome-extensions.txt`
   - Docker Engine with the buildx and compose plugins (Docker's official
     apt repo), and adds you to the `docker` group
   - Claude Desktop (Anthropic's apt repo, amd64 and arm64)
   - Slack and Postman (snaps)
   - Ghostty, the terminal (classic snap; config in `configs/ghostty.conf`)
6. Queues `post-install.sh` for the next login and reboots.

### What `bootstrap-nixos.sh` does

1. Copies the installer's `/etc/nixos/hardware-configuration.nix` (this
   machine's disks and kernel modules) over the placeholder in
   `nixos/hosts/derek/`. Commit it if you want to keep it.
2. Builds the flake's system with `nixos-rebuild boot`: GNOME, Docker,
   Chrome with the extensions, Slack, Postman, Ghostty, zram swap and
   `configs/home.nix` through Home Manager. `nixos/home.nix` does what
   `link-configs.sh` does on Ubuntu. It takes effect at the reboot, not in
   the running desktop, since switching a running GNOME over leaves its
   lock screen unable to check passwords.
3. Queues `post-install.sh` for the next login and reboots.

Claude Desktop has no NixOS package yet, so it isn't installed there.

In a VM, both scripts also turn off GNOME's screen blanking and lock screen,
which otherwise show up as "Display output is not active" in the VM window.

## After Install

1. Log in. A terminal opens with `post-install.sh`, which:
   - runs `ssh-init`, which generates the keys in `configs/ssh-init.json`
     (asks for a passphrase) and writes `~/.ssh/config` with the
     `personal.github.com` alias (skipped if `~/.ssh/id_ecdsa` exists)
   - adds GitHub's published SSH host keys to `~/.ssh/known_hosts`
   - logs in to GitHub with `gh` (in the browser) and uploads the key
   - checks that SSH to GitHub works
   - switches this repo's remote to SSH
   - runs `p personal`, which clones the repos in
     `configs/workspaces/personal.yml` and opens the tmux workspace

   It opens at every login until it has finished once. To run it again by
   hand: `~/dotfiles/post-install.sh`.
2. Enable the Chrome extensions: on Ubuntu, Chrome asks once for each new
   one. On NixOS they are installed and enabled by policy.
3. Sign in to apps and services: Chrome (sync), Slack, Postman, Claude,
   LastPass, `gcloud auth login`, `aws configure`, ...
4. Install anything not managed here that `.bashrc` puts on the `PATH`:
   Flutter in `~/Downloads/flutter` and the Android SDK.

## Desktop

- **Default terminal:** `configs/xdg-terminals.list` makes Ghostty the
  terminal for Ctrl+Alt+T and "Open in Terminal".
- **Startup apps:** each `.desktop` file in `configs/autostart/` is linked
  into `~/.config/autostart` and starts at login (currently Claude, Ghostty,
  Chrome and Slack). Add or delete a file there, then re-run
  `./link-configs.sh`. Entries for apps that aren't installed are skipped.
  On NixOS the terminal and startup apps are set in `nixos/home.nix`
  instead, since these files point at snap and apt paths.
- **GNOME settings:** `dconf.settings` in `configs/home.nix`. Home Manager
  re-applies them on every switch, so changes made in GNOME Settings to
  those keys don't last.
- **Chrome extensions:** add a Web Store ID to
  `configs/chrome-extensions.txt` (take it from the extension's own website
  or GitHub, not a store search) and re-run `./install-apps.sh`. Removing a
  line doesn't uninstall the extension. On NixOS, rebuild instead; there,
  removing a line does uninstall it.

## Updating

To refresh packages and re-apply the config, run:

```sh
system-update
```

That runs `nix-github update`, applies the config again and collects
garbage. After editing a config, apply it with `home-manager switch` on
Ubuntu or `sudo nixos-rebuild switch --flake ~/dotfiles` on NixOS.
