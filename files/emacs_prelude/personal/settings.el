(require 'package)

;; Main line
(display-time-mode 1)
(setq major-mode 'conf-mode)
(display-battery-mode 1)

(setq frame-title-format
      '("" invocation-name " : " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

(electric-indent-mode +1)
(global-hl-line-mode -1)

(setq savehist-additional-variables '(kill-ring compile-command search-ring regexp-search-ring))

(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)	; makes sure tabs are not used.

(setq backup-by-copying t    ; Don't delink hardlinks
      backup-directory-alist '(("." . "~/.emacs.d/temps/backups"))
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old

      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/temps/autosaves" t))
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/temps/undotrees"))

      display-time-24hr-format t
      display-time-day-and-date 0
      )

;; Dired
(setq dired-recursive-deletes 'top)

(global-set-key (kbd "C-j") 'reindent-newline-and-indent)

;; Theme & Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes")
(load-theme 'monokai t)
(set-default-font "Monaco-14")
(setq default-frame-alist '((font . "Monaco-14"))) ;; emacs --daemon

(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(setq sml/mode-width 5
      sml/name-width 30)

;; Trailing whitespace is unnecessary
(setq prelude-clean-whitespace-on-save nil)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; (add-hook 'before-save-hook (lambda () (prelude-indent-region-or-buffer)))

;; copy & paste
(global-set-key [mouse-2] 'mouse-yank-at-click)
(global-set-key [S-mouse-2] 'mouse-yank-at-click)

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary t)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; multiple-cursors
(require 'multiple-cursors)
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map (kbd "C-m") 'mc/mark-more-like-this-extended)

;; Wrap region
(require 'wrap-region)
(wrap-region-global-mode)

;; Sudo Save
(require 'sudo-save)

;;; IDO
(icomplete-mode t)
(ido-mode t)

(setq
 ido-enable-flex-matching t
 ido-everywhere t
 ido-enable-last-directory-history t
 ido-use-filename-at-point 'guess
 ido-create-new-buffer 'always

 ido-case-fold t
 ido-use-virtual-buffers t
 ido-file-extensions-order '(".org" ".txt" ".py" ".rb" ".go" ".emacs" ".xml" ".el"
                             ".ini" ".cfg" ".conf" ".rake" ".coffee" ".scss")
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*Buffer"
                      ".*Completion" "^\*Ido" "^\*trace" "^\*ediff" "^\*vc")
 )
(add-to-list 'ido-ignore-directories "\\.jabber-avatar")

;; Auto Generate Tags
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)

;; Set limit line length
(setq whitespace-line-column 100)

;; Cua Mode
(cua-mode 'emacs)
(setq cua-enable-cua-keys nil)

;; Write good mode
;; (add-hook 'text-mode-hook 'writegood-mode)
;; (add-hook 'org-mode-hook 'writegood-mode)

;; SQL Mode
(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (yas-minor-mode -1)))

;; Diminish
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))

;; Guru
(setq prelude-guru nil)

;; Undo
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-history-directory-alist (quote (("." . "~/.cache/emacs"))))
(setq undo-tree-auto-save-history t)

;; Recentf
(setq recentf-exclude '(
                         "\.hist$" "/COMMIT_EDITMSG$" "/tmp/" "/ssh:" "/sudo:"
                        ))

;; Keyfreq
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Gnutls
(setq gnutls-min-prime-bits 2048)

;; Sauron
(setq
 sauron-dbus-cookie t
 sauron-separate-frame nil
 )

(add-hook 'sauron-event-added-functions
          (lambda (origin prio msg &optional props)
            (if (string-match "ping" msg)
                (sauron-fx-sox "/usr/share/sounds/freedesktop/stereo/bell.oga")
              (sauron-fx-sox "/usr/share/sounds/freedesktop/stereo/bell.oga"))
            (when (>= prio 4)
              (sauron-fx-sox "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga")
              (sauron-fx-gnome-osd msg 10))))

(sauron-start-hidden)
(global-set-key (kbd "<C-f5>") 'sauron-toggle-hide-show)

;; iCal
(require 'calfw-ical)
(setq calcred (netrc-machine (netrc-parse "~/.authinfo.gpg") "calics" t))
;; (cfw:open-ical-calendar (netrc-get cred "password"))
(require 'calfw-gcal)

;; Doc View
(setq doc-view-continuous t)
(add-hook 'doc-view-mode-hook (lambda ()
                           (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
                           (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
                           (define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
                           (define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)
                           ))

;; Which Func
(use-package which-func
  :init (progn
          (add-to-list 'which-func-modes 'go-mode)
          (which-func-mode nil)))
