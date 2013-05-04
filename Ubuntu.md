sudo apt-get install -y aptitude iotop htop golang ack zsh tmux vim-gnome exuberant-ctags git tig mercurial nodejs npm pv acpi libreadline-dev
sudo apt-get install -y irssi lsof links bash-completion xbindkeys x11-xserver-utils xclip rsync gconf-editor xsel
sudo apt-get install -y gimp shutter trimage gcolor2 scrot fcitx-table-wbpy agave
sudo apt-get install -y memcached mongodb redis-server mysql-server mysql-client libmysqlclient-dev postgresql postgresql-client nginx
sudo apt-get install -y chromium-browser zim
sudo apt-get install -y nkf sshuttle filezilla icedtea-7-plugin
sudo apt-get install -y virtualbox-guest-utils virtualbox compiz-plugins
sudo groupadd -a -G vboxusers jinzhu
sudo apt-get install -y ubuntu-restricted-addons ubuntu-restricted-extras

sudo sh -c 'echo "deb http://linux.dropbox.com/ubuntu/ raring main" > /etc/apt/sources.list.d/dropbox.list'
sudo apt-get install -y dropbox
sudo sh -c 'echo "deb http://archive.canonical.com/ubuntu/ raring partner" > /etc/apt/sources.list.d/canonical_partner.list'
sudo apt-get install -y skype adobe-flashplugin

curl -L https://get.rvm.io | bash -s stable --ruby
rvm autolibs enable

# WPS
wget $(wget -q "http://community.wps.cn/download/" -O - | sed -n '/http.*~a.*deb/{s/^.*"\(http.*deb\)".*$/\1/;p}' |head -1) -O wps.deb
sudo dpkg -i wps.deb

# Evolus Pencil
wget -O pencil.deb $(wget -q "https://code.google.com/p/evoluspencil/downloads/list?q=label:2.0.3+OpSys-Linux" -O - | grep '\/\/' | sed -n '/.*deb/{s/^.*"\(\/\/.*deb\)".*$/http:\1/;p}' | head -1)
sudo dpkg -i pencil.deb

# Monaco Font
curl -kL https://raw.github.com/cstrap/monaco-font/master/install-font.sh | bash

# gconf-editor > Apps > Gnome Terminal -> Confirm Window Close
gem i tmuxinator vrome zeus yac
sudo npm install coffee-script-redux -g
