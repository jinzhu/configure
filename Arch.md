sudo pacman -S xorg-server xmonad xmonad-contrib xmobar dmenu sudo readline abs xorg-xkill --noconfirm
sudo pacman -S base-devel whois wireless_tools unzip unzip tar p7zip gzip bzip2 pm-utils openssh pkgtools --noconfirm
sudo pacman -S acpi acpid --noconfirm
sudo pacman -S zsh gvim emacs rxvt-unicode tmux git tig mercurial gdb ranger sdcv xsel openvpn htop iotop the_silver_searcher ctags mutt irssi lsof iptables links go ruby nginx bash-completion wmctrl --noconfirm
sudo pacman -S fakeroot rsync network-manager-applet synergy xbindkeys xclip xorg-xmodmap gnome-terminal --noconfirm
sudo pacman -S gimp inkscape gcolor2 dia scrot nautilus-open-terminal memcached mongodb redis mysql postgresql python-beautifulsoup fcitx evince eog smplayer shotwell file-roller --noconfirm
sudo pacman -S chromium firefox --noconfirm

sudo pacman -S yaourt downgrade --noconfirm
sudo yaourt -S wqy-bitmapfont ttf-monaco wqy-zenhei --noconfirm
sudo yaourt -S gocode cgdb trayer-srg-git dropbox nodejs pv kupfer --noconfirm
sudo yaourt -S shutter trimage-git fontypython nkf agave kdewebdev-klinkstatus pencil sshuttle --noconfirm
sudo yaourt -S virtualbox virtualbox-guest-modules vboxhost-hook virtualbox-ext-oracle --noconfirm
sudo yaourt -S filezilla hd2u skype --noconfirm
sudo yaourt -S wps-office --noconfirm

curl -L https://get.rvm.io | bash -s stable --ruby
rvm autolibs enable

#sudo systemctl enable gdm
#sudo systemctl enable network
sudo systemctl enable NetworkManager
sudo systemctl enable mysqld.service postgresql.service iptables.service nginx.service memcached.service
