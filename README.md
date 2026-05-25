# dev-dotfiles

Personal development environment managed with Nix and Home Manager.

## Fresh Install

This bootstrap currently supports Ubuntu only.

1. Clone this repository.
2. Run `./install.sh` from the repository root.

The installer will:

1. Install Nix if it is missing.
2. Link the tracked config files into your home directory.
3. Update Nix channels.
4. Install Home Manager if needed.
5. Apply the Home Manager configuration.

## Assumptions

The Home Manager config now derives the username and home directory from the environment at install time, so it does not require a fixed username like `derek`.

The bootstrap expects:

1. Ubuntu
2. `curl` and `sh`
3. A user account with permission to install Nix into their own home directory

## After Install

Some parts of the environment still depend on accounts, credentials, or machine-specific setup outside this repo. Typical follow-up steps are:

1. Sign into apps and services you use.
2. Make sure your SSH keys are present.
3. Re-run `home-manager switch` after any config changes.

## Updating

To refresh packages and re-apply the config, run:

```sh
system-update
```

That currently runs the existing Nix and GitHub update helpers, then applies Home Manager again.
