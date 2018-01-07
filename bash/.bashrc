# All the default Ubuntu stuff
. ~/.bash/defaults

alias gs='git status'

export PATH=$HOME/bin:$PATH
export EDITOR=vim
export GPG_TTY=$(tty)

eval "$(direnv hook bash)"
