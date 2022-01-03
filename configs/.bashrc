alias ls='ls -G'
alias ll='ls -Al'
alias l='ls -lrthG'
alias mkdir='mkdir -p'
alias ..='cd ..'

alias g='git status -s'

alias cloc='cloc --not-match-f package-lock --not-match-d dist'

alias filecount='find . | cut -d/ -f2 | sort | uniq -c | sort -bn'

alias clean='git branch | grep -v "master" | xargs git branch -D'

alias br='copy | xclip -selection clipboard'

alias dot='tmuxinator start dotfiles'
alias os='tmuxinator start open-source'
alias client='tmuxinator start client' 
alias tk='tmux kill-session && refresh'

alias start-browser='wmctrl -a google-chrome.Google-chrome -x'
alias start-term='wmctrl -a kitty.kitty -x'
alias start-slack='wmctrl -a Slack -x'

alias connect='eval `ssh-agent -s` && ssh-add ~/.ssh/ec2-access-client'

export TERM="xterm-256color"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi 

GIT_PROMPT_THEME="Single_line_Ubuntu"
# GIT_PROMPT_COMMAND_FAIL="${Green}✔"
# GIT_PROMPT_END=" "
# GIT_PROMPT_ONLY_IN_REPO=1
GIT_PROMPT_SHOW_UNTRACKED_FILES=no
source $HOME/.nix-profile/share/bash-git-prompt/gitprompt.sh

export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

# Direnv for nix
eval "$(direnv hook bash)"
export DIRENV_LOG_FORMAT=

export EDITOR='nvim'
export VISUAL='nvim'

# Puts tmuxinator config into dotfiles control
export TMUXINATOR_CONFIG="$HOME/dotfiles/configs/tmuxinator"

# Autojump activation, installed through nix
source $HOME/.nix-profile/share/autojump/autojump.bash

# added by travis gem
[ -f /home/derekmaffett/.travis/travis.sh ] && source /home/derekmaffett/.travis/travis.sh

export PATH=$HOME/.local/bin/:$PATH
if [ -e /home/derek/.nix-profile/etc/profile.d/nix.sh ]; then . /home/derek/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
