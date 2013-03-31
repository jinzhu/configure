#!/usr/bin/python
import re, os

# ~/.authinfo.gpg
# machine imap.gmail.com login username@gmail.com password xxxxxx port 993
def get_authinfo_password(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s" % (machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    return p.search(authinfo).group(1)
