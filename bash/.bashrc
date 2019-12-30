# All the default Ubuntu stuff
. ~/.bash/defaults
. ~/.bash/aliases.sh

export PATH=$HOME/bin:$HOME/.fastlane/bin:$HOME/.npm_global/bin:/usr/local/lib/ruby/gems/2.6.0/bin:/usr/local/opt/ruby/bin:$PATH
export EDITOR=vim
export GPG_TTY=$(tty)

eval "$(direnv hook bash)"

export FZF_COMPLETION_OPTS='+c -x'

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
