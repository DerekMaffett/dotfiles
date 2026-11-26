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

alias tk='tmux kill-session'

alias p=projects

export TERM="xterm-256color"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi 

GIT_PROMPT_THEME="Single_line_Ubuntu"
# GIT_PROMPT_COMMAND_FAIL="${Green}✔"
# GIT_PROMPT_END=" "
# GIT_PROMPT_ONLY_IN_REPO=1
GIT_PROMPT_SHOW_UNTRACKED_FILES=no
source $HOME/.nix-profile/share/bash-git-prompt/gitprompt.sh

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!.git"'

# Direnv for nix
eval "$(direnv hook bash)"
export DIRENV_LOG_FORMAT=

export EDITOR='nvim'
export VISUAL='nvim'

# Autojump activation, installed through nix
source $HOME/.nix-profile/share/autojump/autojump.bash

# Folders you can write to go after the system and Nix ones, so nothing
# dropped in them (a download, an npm install) can stand in for a normal
# command like ls or git. They only add commands that aren't found earlier.
export PATH="$PATH:$HOME/.local/bin"

# opencode
export PATH="$PATH:$HOME/.opencode/bin"
export PATH="$PATH:$HOME/.npm-global/bin"
export PATH="$PATH:$HOME/Downloads/flutter/bin"

# Android SDK platform-tools (adb)
export PATH="$PATH:$HOME/Android/Sdk/platform-tools"

# Workspace launcher: `p <workspace>` plus tab completion
if command -v projects >/dev/null; then
    source <(projects --bash-completion-script "$(command -v projects)")
    complete -o filenames -F _projects p
fi

# Git worktrees: `wt <branch>` cds to that branch's worktree, `vimw <branch>`
# opens vim in it and leaves you where you were on exit (fzf picker with no arg)
_wt_branches() {
    git worktree list --porcelain 2>/dev/null | sed -n 's|^branch refs/heads/||p'
}

_wt_path() {
    local target
    if [ -n "$1" ]; then
        target=$(git worktree list --porcelain | awk -v b="refs/heads/$1" '
            /^worktree /{p=substr($0,10)} $0=="branch "b{print p; exit}')
    else
        target=$(git worktree list | fzf --height=40% | awk '{print $1}')
    fi
    if [ -z "$target" ]; then
        [ -n "$1" ] && echo "no worktree for '$1'" >&2
        return 1
    fi
    echo "$target"
}

wt() {
    local target
    target=$(_wt_path "$1") && cd "$target"
}

vimw() {
    local target
    target=$(_wt_path "$1") || return
    shift
    (cd "$target" && "${EDITOR:-vim}" "$@")
}

_wt_complete() {
    [ "$COMP_CWORD" -eq 1 ] || return
    COMPREPLY=($(compgen -W "$(_wt_branches)" -- "${COMP_WORDS[COMP_CWORD]}"))
}

complete -F _wt_complete wt
complete -o default -F _wt_complete vimw

# Start every prompt on a fresh line. Without this, output that doesn't end
# in a newline (printf 'x', curl, cat of a file without one) leaves the cursor
# mid-line, the prompt is drawn after it, and readline's next redraw (a \r
# back to column 0, on resize, Ctrl-L or typing) overwrites that text so it
# looks like it never printed. Same trick as zsh's PROMPT_EOL_MARK: print a
# dim "%" and pad to the line width. At column 0 the padding fits on the line
# and \r\e[K wipes it; mid-line it wraps, leaving "text%" visible and the
# cursor on a new line. Runs first in PROMPT_COMMAND and keeps $? intact so
# the ✔/✘ indicator still sees the command's exit status.
__prompt_fresh_line() {
    local status=$?
    printf '\e[2m%%\e[0m%*s\r\e[K' "$(( ${COLUMNS:-80} - 1 ))" ''
    return $status
}
if [[ $- == *i* ]]; then
    PROMPT_COMMAND=(__prompt_fresh_line "${PROMPT_COMMAND[@]}")
fi
