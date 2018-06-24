# All the default Ubuntu stuff
. ~/.bash/defaults

alias git='hub'
alias gs='git status'

export PATH=$HOME/bin:$HOME/.npm_global/bin:$PATH
export EDITOR=vim
export GPG_TTY=$(tty)

eval "$(direnv hook bash)"

alias https='http --default-scheme=https'

source ~/.bash-git-prompt/gitprompt.sh
source /usr/share/autojump/autojump.sh

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

alias t="todo-txt"
alias todo="todo-txt add"
alias today="t ls @today"
alias vitodo="vim ~/.todo-txt/todo.txt"

alias dc="docker-compose"
