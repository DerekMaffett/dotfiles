source ~/.bash/aliases

# Sets up autojump
eval "$(jump shell bash)"

# Prompt
GIT_PROMPT_ONLY_IN_REPO=1
source ~/.bash-git-prompt/gitprompt.sh

export EDITOR='nvim'
export VISUAL='nvim'

# Path
export PATH=$PATH:$HOME/.bash/scripts
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
