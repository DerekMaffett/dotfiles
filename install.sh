#! /bin/sh 

# Install nix
if test ! -f /etc/NIXOS; then
    curl -L https://nixos.org/nix/install | sh 
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

./link-configs.sh

nix-channel --update
nix-env -i all --remove-all

if [[ "$OSTYPE" == "darwin"* ]]; then
    defaults write com.apple.Dock autohide-delay -float 5 && killall Dock
    defaults write -g ApplePressAndHoldEnabled -bool false
fi


if test -f /etc/NIXOS; then
    # Swap base settings
    ./nixos-reboot.sh
fi
