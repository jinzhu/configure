;; Jabber
(use-package jabber
  :init
  (progn
    (setq cred (netrc-machine (netrc-parse "~/.authinfo.gpg") "jabber" t))
    (setq jabber-account-list
          `((,(netrc-get cred "login")
             (:password . ,(netrc-get cred "password"))
             (:network-server . "talk.google.com")
             (:connection-type . ssl)
             (:port . 5223))))

    (setq
     jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil)
     jabber-vcard-avatars-retrieve nil
     jabber-mode-line-mode t

     jabber-show-offline-contacts nil
     jabber-backlog-days 3.0
     ;; jabber-keepalive-interval 100
     )

    (add-hook 'jabber-chat-mode-hook 'flyspell-mode)
    (add-hook 'jabber-lost-connection-hooks 'jabber-connect-all)
    (add-hook 'jabber-post-connect-hooks 'jabber-gmail-subscribe)

    (defun goto-jabber-or-connect ()
      (interactive)
      (if (not (get-buffer "*-jabber-roster-*"))
          (jabber-connect-all))
      (switch-to-buffer "*-jabber-roster-*")
      )
    )

  :bind (("<f3>g" . goto-jabber-or-connect)
         ("<f3>l" . jabber-activity-switch-to)
         ("<f3>o" . jabber-send-default-presence))
  )

(use-package twittering-mode
  :init   (setq twittering-use-master-password t)
  :bind ("<f3>t" . twit)
  )

(use-package weibo
  :bind ("<f3>w" . weibo-timeline)
  )
