# dev-dotfiles
Adapted dotfiles for my own dev environment

# Installation

First install all dependencies

`./setup_dependencies.sh` (May need to run twice)

Then create all symlinks between relevant config files and the correctly named version in `$HOME`

```./setup.sh```

Install vim plugins

`vim`

`:PlugInstall`

Now restart everything and you should be set with Neovim, a collection of decent plugins, and Tmux/Vim integration. Might have to tweak my gitconfig file.

Important highlights:

Vim:

`-` to enter a buffer-specific tree view

`<c-p>` opens a file-finder

`,a` opens an `ag` interface

`<c-{h|j|k|l}>` navigates between vim and tmux buffers fluidly

`,ev` opens the vimrc file

`,sv` sources (loads) the vimrc file while active

`,qq` shuts down vim

`:w` is disabled in favor of autosave. Not sold on it yet, just trying it out.

`,y` copies into the system clipboard

Tmux:

`<c-o>` is leader key

`<c-o>v(s)` opens tmux splits

`<c-o>c` opens a new window

`<c-o><c-{h|j|k|l}>` resizes in that direction

`<c-o>{1|2|3...}` selects a window

`<c-o>[` enters selection mode, copy with `y`
