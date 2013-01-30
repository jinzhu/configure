# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# dircolors --print-database uses its own built-in database
[ -f /etc/bash_completion ] && source /etc/bash_completion

for file in $HOME/{.shell/bash,.shell}/*; do
  source $file
done
