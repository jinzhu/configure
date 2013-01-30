EDITOR="vim"
VISUAL=$EDITOR

for file in $HOME/{.shell/zsh,.shell}/*; do
  source $file
done

#禁用 core dumps
limit coredumpsize 0
#以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'

# Completion ssh en fonction des known_hosts (recupere la liste des hosts dans known_hosts
# local _myhosts
# if [ -d ~/.ssh ]; then
#   if [ -f ~/.ssh/known_hosts ];then
#     _myhosts=(${=${${(f)"$(<$HOME/.ssh/known_hosts)"}%%[# ]*}//,/ })
#   fi
# fi
# zstyle ':completion:*' hosts $_myhosts
