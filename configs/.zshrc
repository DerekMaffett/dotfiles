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
alias tk='tmux kill-session && refresh'

alias start-browser='wmctrl -a google-chrome.Google-chrome -x'
alias start-term='wmctrl -a kitty.kitty -x'
alias start-slack='wmctrl -a Slack -x'

export TERM="xterm-256color"


POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs time)
POWERLEVEL9K_SHORTEN_STRATEGY="Default"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2

if [[ "$OSTYPE" == "darwin"* ]]; then
    # OSX hooks into custom nix installations here since it can't use configuration.nix
    # Downside is these packages are custom and get stale easier
    export ZSH=$HOME/.nix-profile/share/oh-my-zsh
    source "$HOME/.nix-profile/share/powerlevel9k/powerlevel9k.zsh-theme"
    source "$HOME/.nix-profile/share/oh-my-zsh/oh-my-zsh.sh"
    export NIX_PATH=nixpkgs-unstable=/Users/derekmaffett/.nix-defexpr/channels/nixpkgs:$NIX_PATH
else
    # Nix-shell use zsh
    # Not available in OSX
    any-nix-shell zsh --info-right | source /dev/stdin
fi

# Associated with vim plugin, so weird to autogenerate this currently...
export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

. $HOME/.nix-profile/etc/profile.d/nix.sh

# Direnv for nix
eval "$(direnv hook zsh)"
export DIRENV_LOG_FORMAT=

export EDITOR='nvim'
export VISUAL='nvim'

# Haskell installation path for executables
export PATH=$HOME/.local/bin:$PATH

# Puts tmuxinator config into dotfiles control
export TMUXINATOR_CONFIG="$HOME/dotfiles/configs/tmuxinator"

# Autojump activation, installed through nix
source $HOME/.nix-profile/share/autojump/autojump.zsh
