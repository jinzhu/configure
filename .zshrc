VISUAL=$EDITOR

source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
  # use oh-my-zsh
  zgen oh-my-zsh

  zgen oh-my-zsh plugins/z
  zgen oh-my-zsh plugins/osx
  zgen oh-my-zsh plugins/sudo
  zgen oh-my-zsh plugins/tmux
  zgen oh-my-zsh plugins/tmuxinator
  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/history-substring-search
  zgen oh-my-zsh plugins/kubectl
  zgen oh-my-zsh plugins/docker

  zgen load iam4x/zsh-iterm-touchbar

  zgen oh-my-zsh themes/amuse

  # generate the init script from plugins above
  zgen save
fi

for file in $HOME/{.shell/zsh,.shell}/*; do
  if [ ! -d $file ]; then
    source $file
  fi
done

##禁用 core dumps
limit coredumpsize 0
##以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
