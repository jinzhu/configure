#!/bin/sh

if [ -f ~/.screenlayout/layout.sh ] ; then bash ~/.screenlayout/layout.sh & fi

trayer --expand true --transparent true --alpha 255 --edge top --align right --SetDockType true --widthtype request --margin 0 --height 19 --tint 0x000000 &
if [ -x /usr/bin/gnome-terminal ] ; then gnome-terminal & fi
if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi
if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi
if [ -x /usr/bin/xscreensaver ] ; then xscreensaver & fi
if [ -x ~/.scripts/start_tmux ] ; then source ~/.scripts/start_tmux & fi
