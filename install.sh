if test -f /etc/NIXOS; then
    echo "" 
else
    curl https://nixos.org/nix/install | sh 

    . ~/.nix-profile/etc/profile.d/nix.sh
fi

./link-configs.sh

nix-env -i all

if [[ "$OSTYPE" == "darwin"* ]]; then
    defaults write com.apple.Dock autohide-delay -float 5 && killall Dock
    defaults write -g ApplePressAndHoldEnabled -bool false
fi

if test -f /etc/NIXOS; then
    echo ""
else
    chsh -s "$(which zsh)"
fi
