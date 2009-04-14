EDITOR="vim"
VISUAL=$EDITOR
ZDOTDIR=~/.zsh

[[ -f $ZDOTDIR/.zshenv ]] && . $ZDOTDIR/.zshenv
[[ -f $HOME/.bash_aliases ]] && . $HOME/.bash_aliases
[[ -f ~/.dir_colors ]] && eval $(dircolors -b ~/.dir_colors)

for file in $HOME/.zsh/*; do
  source $file
done

CDPATH=".:~:~/GIT:~/Lab:/pillar:/pillar/HOME/:~/Documents/"
#禁用 core dumps
limit coredumpsize 0
#以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
PATH="./bin/:$HOME/bin:$PATH:/usr/local/bin/:/usr/local/sbin"
# Completion ssh en fonction des known_hosts (recupere la liste des hosts dans known_hosts
# local _myhosts
# if [ -d ~/.ssh ]; then
#   if [ -f ~/.ssh/known_hosts ];then
#     _myhosts=(${=${${(f)"$(<$HOME/.ssh/known_hosts)"}%%[# ]*}//,/ })
#   fi
# fi
# zstyle ':completion:*' hosts $_myhosts
