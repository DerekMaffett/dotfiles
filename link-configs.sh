link () {
    ln -fvs $HOME/dotfiles/configs/$1 $HOME/$2
}

mkdir -p ~/.config/projects/
mkdir -p ~/.config/brittany/
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/nixpkgs/
mkdir -p ~/.config/nix/
mkdir -p ~/.config/terminator/
mkdir -p ~/.config/kitty/
mkdir -p ~/.config/qutebrowser/
mkdir -p ~/.config/qutebrowser/bookmarks/
mkdir -p ~/.config/home-manager/
mkdir -p ~/.xmonad/
mkdir -p ~/.stack/
mkdir -p ~/.ssh/

link .projects.json .config/projects/.projects.json
link brittany.yaml .config/brittany/config.yaml
link nix-config.nix .config/nixpkgs/config.nix
link terminator-config .config/terminator/config
link kitty.conf .config/kitty/kitty.conf
link kitty-mac-cmdline-options .config/kitty/macos-launch-services-cmdline
link qutebrowser-config.py .config/qutebrowser/config.py
link qutebrowser-bookmarks .config/qutebrowser/bookmarks/urls
link qutebrowser-quickmarks .config/qutebrowser/quickmarks
link xmonad.hs .xmonad/xmonad.hs
link stack.yaml .stack/config.yaml
link home.nix .config/home-manager/home.nix

link .Xresources .Xresources
link .agignore .agignore
link .gitconfig .gitconfig
link .prettierrc.js .prettierrc.js
link .tmux.conf .tmux.conf
link .zshrc .zshrc
link .bashrc .bashrc
link ssh-init.json .ssh/ssh-init.json
link .nix-channels .nix-channels

if test -f /etc/NIXOS; then
    # Link nixos config to root
    sudo ln -fvs $HOME/dotfiles/configs/preferences-configuration.nix /etc/nixos/preferences-configuration.nix
fi
