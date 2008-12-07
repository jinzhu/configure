############################################################
# Pakage Management
#

# Gentoo
alias em='sudo emerge'

# Arch
alias p='pacman'

function sp {
  if [ -f 'script/plugin' ];then
    ./script/plugin $@
  else
    sudo pacman $@
  fi
}

############################################################
# System
#
alias rm=trash
trash()
{
  mv -f $* /tmp
}

alias ls='ls --color=auto'
alias la='ls -A --color=auto'
alias grep='grep --color'

alias sd='sudo shutdown -h now'

# VIM
alias sgv='sudo gvim'
alias gv='gvim'
alias sv='sudo vim'
function v {
  if [ -n "$*" ]; then
    vim $*
  else
    vim .
  fi
}

# Touchpad
alias tpf='synclient touchpadoff=1'
alias tpo='synclient touchpadoff=0'


alias ri='qri'

# Jobs fg bg
alias f='fg'
alias f1='fg 1'
alias f2='fg 2'
alias f3='fg 3'
alias j='jobs'
alias b='bg'

############################################################
# Rails
#
alias ss='./script/server'
alias sg="./script/generate"
alias as="./script/autospec"

alias sc='./script/console'
alias sd='./script/dbconsole'

############################################################
# Git
#

alias gb='git branch -a -v'
alias gs='git status'
alias gd='git diff'
alias gp='git push'

# gc      => git checkout master
# gc bugs => git checkout bugs
function gc {
if [ -z "$1" ]; then
  git checkout master
else
  git checkout $1
fi
}
