;; Email
(require 'mu4e)
(require 'mu4e-speedbar)

(mu4e-maildirs-extension)
(define-key global-map [f2] 'mu4e)


(setq
 user-mail-address "wosmvp@gmail.com"
 user-full-name  "Jinzhu"
 mail-reply-to user-mail-address

 mu4e-maildir (expand-file-name "~/.mails")
 mu4e-drafts-folder "/[Gmail].Drafts"
 mu4e-sent-folder   "/[Gmail].Sent Mail"
 mu4e-trash-folder  "/[Gmail].Trash"
 mu4e-refile-folder "/[Gmail].All Mail"
 mu4e-update-interval 300
 mu4e-view-show-images t

 mu4e-get-mail-command "offlineimap"
 ;; don't save message to Sent Messages, GMail/IMAP will take care of this
 mu4e-sent-messages-behavior 'delete
 )

(setq
 message-kill-buffer-on-exit t
 mu4e-use-fancy-chars t

 mu4e-html2text-command "w3m -dump -T text/html"
 mu4e-compose-complete-ignore-address-regexp (regexp-opt '("donotreply" "no-reply" "noreply" "docs.google.com" "reply.github.com" "ticket+theplant" "compute.internal"))

 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 )

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?a)
        ("/[Gmail].Important"  . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

(setq mu4e-bookmarks
      '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
         ("date:today..now"                  "Today's messages"     ?t)
         ("date:7d..now"                     "Last 7 days"          ?w)
         ("mime:image/*"                     "Messages with images" ?i)
         ("to:juice@theplant.jp"             "Company" ?c)
         ))


(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-nauth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(add-hook 'mu4e-index-updated-hook
          (defun new-mail-sound ()
            (shell-command "mplayer -really-quiet /usr/share/sounds/freedesktop/stereo/message-new-instant.oga 2&> /dev/null; notify-send -t 15000 -i '/usr/share/icons/oxygen/32x32/status/mail-unread-new.png' 'New email received'")
            )
          )