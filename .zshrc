VISUAL=$EDITOR

for file in $HOME/{.shell/zsh,.shell}/*; do
  if [ ! -d $file ]; then
    source $file
  fi
done

#禁用 core dumps
limit coredumpsize 0
#以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
