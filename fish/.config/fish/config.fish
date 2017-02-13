source ~/.config/fish/powerline.fish

set -gx PATH /home/jon/bin $PATH
eval (direnv hook fish)

# Fixes SSH jumps
stty -ixon

alias gs="git status"
