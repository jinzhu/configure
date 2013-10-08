su - jinzhu -c "(ps aux |grep -v grep |grep 'emacs --daemon') && /usr/bin/emacsclient --eval '(progn (vpnonline-hook))'"
