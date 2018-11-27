#!/bin/bash

HomeDir=`dirname $0`

# Used for prompt
if ! [ -e "$HOME/.bash-git-prompt" ]; then
    git clone https://github.com/magicmonty/bash-git-prompt.git $HOME/.bash-git-prompt --depth=1
fi



cd $HomeDir

link() {
    if [ -h $2 ]; then
        unlink $2
    fi

    echo -n "Link "
    ln -v -s $1 $2
}

# Create symlinks for each config file
configs=("gitconfig" "bashrc" "bash_profile" "bash" "vim" "vimrc", "tmux.conf", "agignore")

F=`pwd |sed -e "s#$HOME/\?##"`
for file in ${configs[@]}
do
    link "$F/$file" "$HOME/.$file"
done




# init.vim needs to be in special nvim config space
# This redirects back to vimrc
if ! [ -e "$HOME/.config/nvim" ]; then
    mkdir -p "$HOME/.config/nvim"
fi

link "$F/vim/init.vim" "$HOME/.config/nvim/init.vim"


# Set up custom scripts
for script in "$F/bash/scripts/*"; do
    chmod u+x $script
done


# OSX hack to get rid of the dock
defaults write com.apple.Dock autohide-delay -float 5 && killall Dock

