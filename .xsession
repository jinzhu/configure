#!/bin/sh

if [ -f ~/.screenlayout/layout.sh ] ; then bash ~/.screenlayout/layout.sh & fi

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x000000 --height 19 &
if [ -x /usr/bin/gnome-terminal ] ; then gnome-terminal & fi
if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi
if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi
if [ -x /usr/bin/xscreensaver ] ; then xscreensaver & fi

