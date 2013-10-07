(ps aux |grep -v grep |grep 'emacs --daemon') && /usr/bin/emacsclient --eval "(progn (goto-jabber-or-connect) (weibo-timeline))"
