#! /bin/sh 

# Prompt user to import preferences without overriding defaults
# The prompt isn't working for some reason
grep -qxF "# ADD ./preferences-configuration.nix TO IMPORTS" /etc/nixos/configuration.nix || sudo echo -e "# ADD ./preferences-configuration.nix TO IMPORTS\n\n$(cat /etc/nixos/configuration.nix)" > /etc/nixos/configuration.nix
sudo vim /etc/nixos/configuration.nix

sudo nixos-rebuild switch

# Activates the caps lock switch in preferences-configuration.nix
gsettings reset org.gnome.desktop.input-sources xkb-options
gsettings reset org.gnome.desktop.input-sources sources

# Load custom keybindings
# Maybe I can do this for the caps:ctrl setting too? Not sure if the xkb setting is the same between systems...
cat ./configs/.dconf | dconf load /
