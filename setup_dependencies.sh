#!/bin/bash

brew install jump
brew install neovim
brew install cloc

# Vim-plug package manager
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim