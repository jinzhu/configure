# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[[ -f ~/.bashrc ]] && . ~/.bashrc

#Run Xbindkeys,a program that allows you to launch shell commands with your keyboard or your mouse under X Window.
xbindkeys > /dev/null
