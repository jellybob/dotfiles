set -gx PATH /home/jon/bin $PATH
set -gx GIT_TEMPLATE_DIR (overcommit --template-dir)
eval (direnv hook fish)

# Fixes SSH jumps
stty -ixon

alias git="hub"
alias gs="git status"

set -x GPG_TTY (tty)
