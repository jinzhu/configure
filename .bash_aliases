alias wp='wgetpaste -n Jinzhu'
# Pakage Operation
# Gentoo
alias em='sudo emerge'
# Arch
alias p='sudo pacman'

function sp {
# if Rails App
  if [ -f 'script/plugin' ];then
    ./script/plugin $1
  else
    sudo pacman $1
  fi
}

# Gernel Operation
alias ls='ls --color=auto'
alias grep='grep --color'
alias la='ls -A --color=auto'

alias sgv='sudo gvim'
alias gv='gvim'
alias sv='sudo vim'
alias vi='vim'

# Touchpad
alias tpf='synclient touchpadoff=1'
alias tpo='synclient touchpadoff=0'

# Rails
alias ss='./script/server'
alias sg="./script/generate"

alias sc='./script/console'
alias sd='./script/dbconsole'

# Git
alias gb='git branch -a -v'
alias gs='git status'
alias gd='git diff'

# gc      => git checkout master
# gc bugs => git checkout bugs
function gc {
if [ -z "$1" ]; then
  git checkout master
else
  git checkout $1
fi
}
