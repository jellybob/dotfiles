source ~/.config/fish/powerline.fish

set -gx PATH /home/jon/bin $PATH
eval (direnv hook fish)

# Check if a key needs adding
if ssh-add -L | grep "no identities" > /dev/null
  ssh-add
end

# Fixes SSH jumps
stty -ixon

alias git="hub"
alias gs="git status"
