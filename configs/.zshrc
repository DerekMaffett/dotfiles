alias ls='ls -G'
alias ll='ls -Al'
alias l='ls -lrthG'
alias mkdir='mkdir -p'
alias ..='cd ..'

alias g='git status -s'

alias cloc='cloc --not-match-f package-lock --not-match-d dist'

alias filecount='find . | cut -d/ -f2 | sort | uniq -c | sort -bn'

alias clean='git branch | grep -v "master" | xargs git branch -D'

alias br='copy-branch | xclip -selection clipboard'

alias dot='tmuxinator start dotfiles'
alias os='tmuxinator start open-source'
alias client='tmuxinator start client' 
alias happy='tmuxinator start happy' 
alias tk='tmux kill-session && refresh'

alias start-browser='wmctrl -a google-chrome.Google-chrome -x'
alias start-term='wmctrl -a kitty.kitty -x'
alias start-slack='wmctrl -a Slack -x'

alias connect='eval `ssh-agent -s` && ssh-add ~/.ssh/ec2-access-client'

export TERM="xterm-256color"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi 

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs time)
POWERLEVEL9K_SHORTEN_STRATEGY="Default"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2

# Presentation
# POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir)
# POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()
# POWERLEVEL9K_SHORTEN_DIR_LENGTH=1

source "$HOME/.nix-profile/share/powerlevel10k/powerlevel9k.zsh-theme"

if test -f /etc/NIXOS; then
    # Nix-shell use zsh, usually supplanted by direnv nix support
    any-nix-shell zsh --info-right | source /dev/stdin
else
    # If not NixOS
    # export ZSH_THEME="wezm"
    export ZSH=$HOME/.nix-profile/share/oh-my-zsh
    source "$HOME/.nix-profile/share/oh-my-zsh/oh-my-zsh.sh"
fi

export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

# Direnv for nix
eval "$(direnv hook zsh)"
export DIRENV_LOG_FORMAT=

export EDITOR='nvim'
export VISUAL='nvim'

# Puts tmuxinator config into dotfiles control
export TMUXINATOR_CONFIG="$HOME/dotfiles/configs/tmuxinator"

# Autojump activation, installed through nix
source $HOME/.nix-profile/share/autojump/autojump.zsh

# Should deactivate shared history from oh-my-zsh
unsetopt share_history

# added by travis gem
[ -f /home/derekmaffett/.travis/travis.sh ] && source /home/derekmaffett/.travis/travis.sh

export PATH=$HOME/.local/bin/:$PATH
