#!/bin/bash

brew install jump
brew install neovim
brew install cloc
brew install tmux

# Vim-plug package manager
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Installs nvm, sets node version, and implicitly installs npm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
source ~/.nvm/nvm.sh # necessary to access nvm command
nvm install v11.2.0

echo "The following commands may require restarting your terminal..."
npm install -g elm-format
