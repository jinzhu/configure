# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set a fancy prompt (non-color, unless we know we "want" color)
PS1='\[\033[01;34m\]\W \$ \[\033[00m\]'

# Change the window title of X terminals 
case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
  PROMPT_COMMAND='echo -ne "\033]0;$(__git_ps1)${PWD/$HOME/~}\007"'
  ;;
  screen)
  PROMPT_COMMAND='echo -ne "\033_$(__git_ps1) ${PWD/$HOME/~}\033\\"'
  ;;
esac

# dircolors --print-database uses its own built-in database
if [ -f ~/.dir_colors ]; then
  eval $(dircolors -b ~/.dir_colors)
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# enable programmable completion features
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

CDPATH=".:~:~/GIT:~/Lab:~/WEB/:~/GIT/:/pillar:/pillar/HOME/:~/Documents/"
PATH="./bin/:$HOME/bin:$HOME/.gem/ruby/1.8/bin/:$PATH:/usr/bin:/usr/local/bin/"

export LC_CTYPE=zh_CN.utf8
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS="@im=fcitx"
