#!/bin/bash

HomeDir=`dirname $0`

# Used for prompt
if ! [ -e "$HOME/.bash-git-prompt" ]; then
    git clone https://github.com/magicmonty/bash-git-prompt.git $HOME/.bash-git-prompt --depth=1
fi

cd $HomeDir
F=`pwd |sed -e "s#$HOME/\?##"`

configs=("gitconfig" "bashrc" "bash_profile")

for file in ${configs[@]}
do
  INSTALLATION_ADDRESS="$HOME/.$file"

  if [ -h $INSTALLATION_ADDRESS ]; then
    unlink $INSTALLATION_ADDRESS
  fi

  # create link
  echo -n "Link "
  ln -v -s "$F/$file" $INSTALLATION_ADDRESS
done