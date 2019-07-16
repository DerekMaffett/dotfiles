export DOTFILES=$HOME/dotfiles

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
alias watch-dot='tmuxinator start watch'
alias tk='tmux kill-session && refresh'

export TERM="xterm-256color"

export ZSH=$HOME/.nix-profile/share/oh-my-zsh

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs time)
POWERLEVEL9K_SHORTEN_STRATEGY="Default"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
source "$HOME/.nix-profile/share/powerlevel9k/powerlevel9k.zsh-theme"
source "$HOME/.nix-profile/share/oh-my-zsh/oh-my-zsh.sh"

autoload -U +X compinit && compinit

# Associated with vim plugin, so weird to autogenerate this currently...
export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

. /Users/derekmaffett/.nix-profile/etc/profile.d/nix.sh

# eval "$(direnv hook zsh)"
# export DIRENV_LOG_FORMAT=

export fpath=(path/to/zsh-completions/src $fpath)
rm -f ~/.zcompdump; compinit

export EDITOR='nvim'
export VISUAL='nvim'

export PATH=$HOME/.local/bin:$PATH

export TMUXINATOR_CONFIG="$HOME/dotfiles/configs/tmuxinator"

source $HOME/.nix-profile/share/autojump/autojump.zsh
