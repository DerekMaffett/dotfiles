export DOTFILES=$HOME/dotfiles
export PATH=$DOTFILES/.devfiles/.bin:$PATH

# For easier development of river
export PATH=$HOME/projects/river/bin:$PATH

alias ls='ls -G'
alias ll='ls -Al'
alias l='ls -lrthG'
alias mkdir='mkdir -p'
alias ..='cd ..'

alias g='git status -s'
alias vim='nvim'
alias vi='nvim'

alias cloc='cloc --not-match-f package-lock --not-match-d dist'

alias filecount='find . | cut -d/ -f2 | sort | uniq -c | sort -bn'

alias clean='git branch | grep -v "master" | xargs git branch -D'

alias br='copy-branch | pbcopy'

alias dot='txs dotfiles'
alias os='txs open-source'
alias tk='tmux kill-session && refresh'

export ZSH=$DOTFILES/.devfiles/.installations/robbyrussell/oh-my-zsh

plugins=(
   autojump
   tmuxinator
)

export TERM="xterm-256color"

ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs time)
POWERLEVEL9K_SHORTEN_STRATEGY="Default"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2

autoload -U +X compinit && compinit

source $ZSH/oh-my-zsh.sh

# Associated with vim plugin, so weird to autogenerate this currently...
export FZF_DEFAULT_COMMAND='ag --hidden -g ""'
