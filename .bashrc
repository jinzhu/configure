# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# dircolors --print-database uses its own built-in database
if [ -f ~/.dir_colors ]; then
  eval $(dircolors -b ~/.dir_colors)
fi

# enable programmable completion features
if [ -f /etc/bash_completion ]; then
  source /etc/bash_completion
fi

if [ -f ~/.bash_prompt ]; then
  source ~/.bash_prompt
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
  source ~/.bash_aliases
fi

CDPATH=".:~:~/GIT:~/Lab:~/WEB/:~/GIT/:/pillar:/pillar/HOME/:~/Documents/"
PATH="./bin/:$HOME/bin:$PATH:/usr/local/bin/"

export LC_CTYPE=zh_CN.utf8
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS="@im=fcitx"
