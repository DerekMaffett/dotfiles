#! /bin/sh 

# Install nix
if test ! -f /etc/NIXOS; then
    curl -L https://nixos.org/nix/install | sh 
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

./link-configs.sh

nix-channel --update

if test -f /etc/NIXOS; then
    # Swap base settings
    ./nixos-rebuild.sh
    home-manager switch
fi
