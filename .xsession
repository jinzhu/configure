#!/bin/sh

if [ -x /usr/bin/trayer ] ; then trayer --expand true --transparent true --alpha 255 --edge top --align right --SetDockType true --widthtype request --margin 0 --height 18 --tint 0x000000 & fi
if [ -x /usr/bin/urxvt ] ; then urxvt & fi
if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi
if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi
if [ -x /usr/bin/xscreensaver ] ; then xscreensaver & fi
if [ -x /usr/bin/dropboxd ] ; then dropboxd & fi
