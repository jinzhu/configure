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
alias j='jobs'
alias f='fg'
alias b='bg'

for ((i=1;i<5;i++)) do
  eval alias f$i=\'fg $i\';
  eval alias b$i=\'bg $i\';
done

############################################################
# Rails
#
alias ss='./script/server'
alias sg="./script/generate"
function as {
  if [ -f 'script/autospec' ];then
    ./script/autospec $@
  else
    autotest $@
  fi
}


alias sc='./script/console'
alias sd='./script/dbconsole'

############################################################
# Git
#

alias gb='git branch -a -v'
alias gs='git status'
alias gd='git diff'
alias gp='git push'
alias gl='git log'

# gc      => git checkout master
# gc bugs => git checkout bugs
function gc {
if [ -z "$1" ]; then
  git checkout master
else
  git checkout $1
fi
}
