curl https://nixos.org/nix/install | sh

./link-configs.sh

defaults write com.apple.Dock autohide-delay -float 5 && killall Dock
defaults write -g ApplePressAndHoldEnabled -bool false

nix-env -i all

echo "test"
chsh -s (which zsh)
