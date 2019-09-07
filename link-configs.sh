link () {
    ln -fvs $HOME/dotfiles/configs/$1 $HOME/$2
}

mkdir -p ~/.config/projects/
mkdir -p ~/.config/brittany/
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/nixpkgs/
mkdir -p ~/.config/terminator/
mkdir -p ~/.config/kitty/
mkdir -p ~/.config/qutebrowser/
mkdir -p ~/.config/qutebrowser/bookmarks/
mkdir -p ~/.xmonad/
mkdir -p ~/.stack/
mkdir -p ~/.ssh/

link .projects.json .config/projects/.projects.json
link .work-projects.json .config/projects/.work-projects.json
link brittany.yaml .config/brittany/config.yaml
link init.vim .config/nvim/init.vim
link nix-config.nix .config/nixpkgs/config.nix
link terminator-config .config/terminator/config
link kitty.conf .config/kitty/kitty.conf
link kitty-mac-cmdline-options .config/kitty/macos-launch-services-cmdline
link qutebrowser-config.py .config/qutebrowser/config.py
link qutebrowser-bookmarks .config/qutebrowser/bookmarks/urls
link qutebrowser-quickmarks .config/qutebrowser/quickmarks
link xmonad.hs .xmonad/xmonad.hs
link stack.yaml .stack/config.yaml

link .Xresources .Xresources
link .agignore .agignore
link .gitconfig .gitconfig
link .prettierrc.js .prettierrc.js
link .tmux.conf .tmux.conf
link .zshrc .zshrc
link ssh-init.json .ssh/ssh-init.json

if test -f /etc/NIXOS; then
    # Link nixos config to root
    sudo ln -fvs $HOME/dotfiles/configs/nixos-configuration.nix /etc/nixos/configuration.nix
fi
