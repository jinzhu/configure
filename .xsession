#!/bin/sh

if [ -f ~/.screenlayout/layout.sh ] ; then bash ~/.screenlayout/layout.sh & fi

trayer --expand true --transparent true --alpha 255 --edge top --align right --SetDockType true --widthtype request --margin 0 --height 19 --tint 0x000000 &
if [ -x /usr/bin/urxvt ] ; then urxvt & fi
if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi
if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi
if [ -x /usr/bin/xscreensaver ] ; then xscreensaver & fi
echo 'sdsd' > /home/jinzhu/bbb

[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator
[[ -s $HOME/.scripts/start_tmux ]] && ruby $HOME/.scripts/start_tmux &
