# My dot files

  for Xmonad, Vim, urxvt, tmux, Archlinux, zsh/bash, git, ruby/rails, xbindkey, vrome, vimperator, yac , eo, chrome...

## Install the dot files:

    rake install

## Remove:

    rake remove

## Install Softwares:

    pacman -S xorg-server xmonad xmonad-contrib xmobar dmenu sudo readline abs
    pacman -S base-devel whois wireless_tools unzip unzip tar p7zip gzip bzip2 pm-utils openssh pkgtools
    pacman -S zsh gvim rxvt-unicode tmux git tig mercurial gdb ranger sdcv xsel openvpn htop iotop ack ctags mutt irssi lsof iptables links go ruby nginx bash-completion
    pacman -S fakeroot rsync network-manager-applet synergy xbindkeys xclip xorg-xmodmap
    pacman -S gimp inkscape gcolor2 dia scrot nautilus-open-terminal memcached mongodb redis mysql postgresql python-beautifulsoup fcitx evince eog smplayer shotwell
    pacman -S chromium firefox

    curl -L https://get.rvm.io | bash -s stable --ruby

    ## pacman.conf
    [archlinuxfr]
    SigLevel = Never
    Server = http://repo-fr.archlinuxcn.org/$arch

    ## /etc/passwd
    change default login shell to zsh

    pacman -S yaourt downgrade
    yaourt -S wqy-bitmapfont ttf-monaco wqy-zenhei --noconfirm
    yaourt -S gocode cgdb trayer-srg-git dropbox nodejs pv kupfer --noconfirm
    yaourt -S shutter trimage-git fontypython nkf agave kdewebdev-klinkstatus pencil sshuttle --noconfirm
    yaourt -S virtualbox virtualbox-guest-modules vboxhost-hook virtualbox-ext-oracle --noconfirm
    yaourt -S filezilla hd2u skype --noconfirm

    sudo systemctl enable gdm
    sudo systemctl enable network
    sudo systemctl enable NetworkManager
    sudo systemctl enable mysqld.service postgresql.service iptables.service nginx.service memcached.service


## Install SwitchySharp config:

    Option > Restore from Online: https://raw.github.com/jinzhu/configure/master/files/SwitchyOptions.bak

## Install Ruby Gems

    gem i tmuxinator vrome zeus
