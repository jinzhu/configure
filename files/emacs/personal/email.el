;; Email
(require 'mu4e)
(require 'mu4e-speedbar)

(setq
 user-mail-address "wosmvp@gmail.com"
 user-full-name  "Jinzhu"
 mail-reply-to user-mail-address

 mu4e-maildir (expand-file-name "~/.mails")
 mu4e-drafts-folder "/[Gmail].Drafts"
 mu4e-sent-folder   "/[Gmail].Sent Mail"
 mu4e-trash-folder  "/Trash"
 mu4e-update-interval 60
 mu4e-view-show-images t

 mu4e-get-mail-command "offlineimap"
 ;; don't save message to Sent Messages, GMail/IMAP will take care of this
 mu4e-sent-messages-behavior 'delete
 )

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?a)
        ("/[Gmail].Important"  . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))


(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
