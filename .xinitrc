#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

/usr/bin/gnome-keyring-daemon --start --components=gpg
/usr/bin/gnome-keyring-daemon --start --components=pkcs11
/usr/bin/gnome-keyring-daemon --start --components=secrets
/usr/bin/gnome-keyring-daemon --start --components=ssh

# Add the 3 lines below to Convert caps-lock into Control
xmodmap -e 'remove Lock = Caps_Lock'
xmodmap -e 'keysym Caps_Lock = Control_L'
xmodmap -e 'add Control = Control_L'

# Add the 3 lines below to use the Right-Control as Caps-Lock
xmodmap -e 'remove Control = Control_R'
xmodmap -e 'keysym Control_R = Caps_Lock'
xmodmap -e 'add Lock = Caps_Lock'

DEFAULT_SESSION=xmonad
case $1 in
xmonad)
    xsetroot -cursor_name left_ptr
    source .xprofile
    exec ck-launch-session xmonad
    ;;
gnome)
    exec ck-launch-session gnome-session
    ;;
*)
    exec ck-launch-session $DEFAULT_SESSION
    ;;
esac
