link () {
    ln -fvs $HOME/dotfiles/configs/$1 $HOME/$2
}

mkdir -p ~/.config/projects/
mkdir -p ~/.config/brittany/
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/nixpkgs/
mkdir -p ~/.config/terminator/
mkdir -p ~/.config/kitty/
mkdir -p ~/.stack/

link .projects.json .config/projects/.projects.json
link .work-projects.json .config/projects/.work-projects.json
link brittany.yaml .config/brittany/config.yaml
link init.vim .config/nvim/init.vim
link nix-config.nix .config/nixpkgs/config.nix
link terminator-config .config/terminator/config
link kitty.conf .config/kitty/kitty.conf
link kitty-mac-cmdline-options .config/kitty/macos-launch-services-cmdline
link stack.yaml .stack/config.yaml

link .agignore .agignore
link .gitconfig .gitconfig
link .prettierrc.js .prettierrc.js
link .tmux.conf .tmux.conf
link .zprofile .zprofile
link .zshrc .zshrc
link ssh-config .ssh/config
