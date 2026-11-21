# Home Manager settings used only on NixOS, next to configs/home.nix.
# Covers what link-configs.sh and parts of install-apps.sh do on Ubuntu.
{ config, ... }:

let
  repo = "${config.home.homeDirectory}/dotfiles";

  # Links straight to the file in the clone, not a copy in the Nix store, so
  # edits take effect without a rebuild, as they do now.
  link = path: config.lib.file.mkOutOfStoreSymlink "${repo}/configs/${path}";

  autostart = name: exec: {
    "autostart/${name}.desktop".text = ''
      [Desktop Entry]
      Type=Application
      Name=${name}
      Exec=${exec}
      X-GNOME-Autostart-enabled=true
    '';
  };
in
{
  # home.nix, nix-config.nix and .nix-channels aren't linked: the flake
  # replaces them.
  home.file = {
    ".bashrc".source = link ".bashrc";
    ".gitconfig".source = link ".gitconfig";
    ".prettierrc.js".source = link ".prettierrc.js";
    ".tmux.conf".source = link ".tmux.conf";
    ".ssh/ssh-init.json".source = link "ssh-init.json";
    "projects/personal/workspace.yml".source = link "workspaces/personal.yml";
    "projects/personal/dotfiles".source = config.lib.file.mkOutOfStoreSymlink repo;
  };

  xdg.configFile = {
    "ghostty/config.ghostty".source = link "ghostty.conf";
    "direnv/direnv.toml".source = link "direnv.toml";
    "opencode/opencode.jsonc".source = link "opencode.jsonc";

    # configs/xdg-terminals.list names the snap's entry; this is nixpkgs'.
    "xdg-terminals.list".text = "com.mitchellh.ghostty.desktop\n";
  }
  # configs/autostart/ points at /snap/bin and /usr/bin, which don't exist
  # here. Claude is left out until it has a package.
  // autostart "Ghostty" "ghostty --gtk-single-instance=true"
  // autostart "Google Chrome" "google-chrome-stable"
  // autostart "Slack" "slack";

  # Chrome as default browser. Note: this makes ~/.config/mimeapps.list
  # read-only, so "set as default" in apps stops working.
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "google-chrome.desktop";
      "x-scheme-handler/http" = "google-chrome.desktop";
      "x-scheme-handler/https" = "google-chrome.desktop";
    };
  };
}
