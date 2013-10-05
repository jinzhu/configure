(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(setq wl-temporary-file-directory "~/.cache/temp")

(setq wl-summary-always-sticky-folder-list t)
(setq wl-summary-line-format "%n%T%P %D/%M (%W) %h:%m %t%[%25(%c %f%) %] %s")
(setq wl-summary-width 150)

;; HTML Email
(require 'w3m)
(require 'mime-w3m)

(setq mime-view-type-subtype-score-alist
      '(((text . plain) . 4)
        ((text . enriched) . 3)
        ((text . html) . 2)
        ((text . richtext) . 1)))
(setq wl-summary-incorporate-marks '("N" "U" "!" "A" "F" "$"))

;; IMAP, gmail:
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "wosmvp@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl

      ;;for non ascii-characters in folder-names
      elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "wosmvp"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com"
      wl-message-id-domain "smtp.gmail.com")

(setq wl-from "Jinzhu <wosmvp@gmail.com>"

      ;;all system folders (draft, trash, spam, etc) are placed in the
      ;;[Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%inbox"
      wl-draft-folder   "%[Gmail]/Drafts"
      wl-trash-folder   "%[Gmail]/Trash"
      wl-fcc            "%[Gmail]/Sent"

      wl-folder-check-async t

      ;; mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read    t
      ;;for when auto-compleating foldernames
      wl-default-spec "%"

      wl-auto-save-drafts-interval nil
      wl-stay-folder-window t                       ;; show the folder pane (left)
      wl-folder-window-width 25                     ;; toggle on/off with 'i'

      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^Message-Id:"
        "^\\(Posted\\|Date\\):"
        )
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc"))


;; http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html
(require 'bbdb-wl)
(bbdb-wl-setup)
(setq bbdb-wl-folder-regexp    ;; get addresses only from these folders
 "^\.inbox$\\|^.sent")    ;;

(require 'elmo-search)
(elmo-search-register-engine
 'mu 'local-file
 :prog "mu" ;; or wherever you've installed it
 :args '("find" pattern "--fields" "l") :charset 'utf-8)

(setq elmo-search-default-engine 'mu)
;; for when you type "g" in folder or summary.
(setq wl-default-spec "[")

(setq wl-biff-check-folder-list '("%inbox"))
(setq wl-biff-check-interval 10)
(add-hook 'wl-biff-notify-hook
          '(lambda()
            (djcb-popup "Wanderlust" "You have new mail!"
                        "/usr/share/icons/gnome/32x32/status/mail-unread.png"
                        "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga")))
