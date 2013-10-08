(require 'package)

;; Main line
(display-time-mode 1)
(setq default-major-mode 'conf-mode)
(display-battery-mode 1)

(electric-indent-mode +1)
(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)			; makes sure tabs are not used.

;; Theme & Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes")
(load-theme 'monokai)
(set-default-font "Monaco-14")
(setq default-frame-alist '((font . "Monaco-14"))) ;; emacs --daemon
(global-hl-line-mode -1)

(require 'powerline)
(powerline-default-theme)

;; copy with middle mouse click
(global-set-key [mouse-2] 'mouse-yank-at-click)

(require 'multiple-cursors)
(require 'region-bindings-mode)
(region-bindings-mode-enable)

(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-x o") 'switch-window)

;; Wrap region
(require 'wrap-region)
(wrap-region-mode t)

;; Sudo Save
(require 'sudo-save)

;; git gutter
(global-git-gutter-mode t)
(global-set-key (kbd "<escape>gg") 'git-gutter:toggle)
(global-set-key (kbd "<escape>j") 'git-gutter:next-diff)
(global-set-key (kbd "<escape>k") 'git-gutter:previous-diff)
(global-set-key (kbd "<escape>gd") 'git-gutter:popup-diff)
(global-set-key (kbd "<escape>gr") 'git-gutter:revert-hunk)

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary t)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Trailing whitespace is unnecessary
(setq prelude-clean-whitespace-on-save nil)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; (add-hook 'before-save-hook (lambda () (prelude-indent-region-or-buffer)))

;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'go-autocomplete)

;; GoLang
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-minor-mode-on)

;; Select candidates with C-n/C-p only when completion menu is displayed
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)


(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun copy-current-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;;; key choard
(setq key-chord-two-keys-delay 0.2)
(global-set-key (kbd "<escape>yy") 'copy-current-line)
(key-chord-define-global "yy" 'copy-current-line)
(global-set-key (kbd "<escape>vv") 'select-current-line)
(key-chord-define-global "vv" 'select-current-line)
(global-set-key (kbd "<escape>m") 'magit-status)
(key-chord-define-global ";w" 'save-buffer)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<escape>l") 'helm-all-mark-rings)
(global-set-key (kbd "<escape>cd") 'goto-last-dir)

;;; IDO
(icomplete-mode t)
(ido-mode t)
(ido-everywhere 1)
(setq
 ido-enable-flex-matching t
 ido-enable-last-directory-history t
 ido-case-fold t
 ido-use-virtual-buffers t
 ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el"
                             ".ini" ".cfg" ".conf" ".rb" ".rake" ".coffee" ".scss")
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*Buffer" "^\*scratch"
                      ".*Completion" "^\*Ido" "^\*trace" "^\*ediff" "^\*vc")
 )

;; Auto Generate Tags
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)

;; multi term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "<f3>") 'multi-term-dedicated-toggle)
(global-set-key (kbd "<C-f3>") 'multi-term)
(global-set-key (kbd "<M-f3>") 'multi-term-next)
(global-set-key (kbd "<M-s-f3>") 'multi-term-next)

(setq multi-term-dedicated-select-after-open-p t)

(defun term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
        )
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l"))
      )))

(defun goto-last-dir ()
  (interactive)
  (shell-process-cd
   (replace-regexp-in-string "\n" "" (get-string-from-file "/tmp/.last_dir"))))

(setq
 tramp-default-method "ssh"          ; uses ControlMaster
 comint-scroll-to-bottom-on-input t  ; always insert at the bottom
 comint-scroll-to-bottom-on-output nil ; always add output at the bottom
 comint-scroll-show-maximum-output t
 comint-input-ignoredups t ; no duplicates in command history
 comint-completion-addsuffix t ; insert space/slash after file completion
 comint-buffer-maximum-size 200000
 )

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-l") 'term-send-raw)
                            (define-key term-raw-map (kbd "C-j") 'term-switch-to-shell-mode)
                            (define-key term-raw-map (kbd "<escape><escape>") 'term-send-raw-meta)
                            (define-key term-raw-map (kbd "M-.") 'term-send-raw-meta)
                            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
                            (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                            (yas-minor-mode -1)
                            (ansi-color-for-comint-mode-on)
                            (set (make-local-variable 'outline-regexp) "\$ ")
                            (outline-minor-mode)
                            ))

(add-hook 'shell-mode-hook (lambda ()
                             (define-key shell-mode-map (kbd "<up>") 'term-send-up)
                             (define-key shell-mode-map (kbd "<down>") 'term-send-down)
                             ))

;; Bash Complete
(require 'shell-command)
(shell-command-completion-mode)
(require 'bash-completion)
(bash-completion-setup)

;; Set limit line length
(setq whitespace-line-column 100)

;; Cua Mode
(cua-mode 'emacs)
(setq cua-enable-cua-keys nil)

;; Write good mode
;; (add-hook 'text-mode-hook 'writegood-mode)
;; (add-hook 'org-mode-hook 'writegood-mode)

;; Evil Mode
(require 'evil)
(global-set-key (kbd "<C-escape>") 'evil-mode)
(evil-set-toggle-key "<C-escape>")
(global-set-key (kbd "C-*") 'evil-search-symbol-forward)
(global-set-key (kbd "C-#") 'evil-search-symbol-backward)


;; Jabber
(require 'netrc)
(require 'jabber)
(setq cred (netrc-machine (netrc-parse "~/.authinfo.gpg") "jabber" t))
(setq jabber-account-list
      `((,(netrc-get cred "login")
         (:password . ,(netrc-get cred "password"))
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:port . 5223))))

(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))
(setq jabber-vcard-avatars-retrieve nil)
(setq jabber-mode-line-mode t)
(setq
 jabber-show-offline-contacts nil
 jabber-backlog-days 3.0
 ;; jabber-keepalive-interval 100
 )
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
(add-hook 'jabber-lost-connection-hooks 'jabber-connect-all)
;; (add-hook 'jabber-post-connect-hooks (lambda ()
;;                                        (jabber-gmail-subscribe)
;;                                        (jabber-keepalive-start)
;;                                        (jabber-send-default-presence)))

(defun goto-jabber-or-connect ()
  (interactive)
  (if (not (get-buffer "*-jabber-roster-*"))
      (jabber-connect-all))
  (switch-to-buffer "*-jabber-roster-*")
  )

(global-set-key (kbd "<M-f2>") 'goto-jabber-or-connect)

;; Twitter
(setq twittering-use-master-password t)
(global-set-key (kbd "<M-S-f2>") 'twit)

;; Weibo
(global-set-key (kbd "<C-f2>") 'weibo-timeline)

;; Tramp
(setq tramp-default-method "ssh")

;; deft
(require 'deft)
(setq deft-directory "/jinzhu/Dropbox/Notebooks")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "<M-f1>") 'deft)
(setq deft-use-filename-as-title t)

;; Org Mode
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;; SQL Mode
(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (yas-minor-mode -1)))

;; Disable warnning while edit emacs lisp scripts
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
        (concat ad-return-value
                (let ((plus-minus (vc-git--run-command-string
                                   file "diff" "--numstat" "--")))
                  (and plus-minus
                       (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                       (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus))))
                )
        ))

;; Diminish
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))

;; Guru
(setq prelude-guru nil)

;; Ditaa
(setq ditaa-cmd "java -jar /usr/share/java/ditaa/ditaa-0_9.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
   (concat ditaa-cmd " " buffer-file-name)))

;; Undo
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-history-directory-alist (quote (("." . "~/.cache/emacs"))))
(setq undo-tree-auto-save-history t)

;; Recentf
(setq recentf-exclude '(
                        "\.hist$"
                        "/COMMIT_EDITMSG$"
                        ))

;; Browse Kill Ring
(global-set-key "\C-xy" 'helm-show-kill-ring)
(setq savehist-additional-variables '(kill-ring compile-command search-ring regexp-search-ring))

;; Dired
(global-set-key (kbd "C-c j") 'dired-jump)

;; Keyfreq
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Point Undo
(require 'point-undo)
(global-set-key (kbd "<escape>p") 'point-undo)
(global-set-key (kbd "<escape>n") 'point-redo)
(global-set-key (kbd "<escape>t") 'jump-to-register)
(global-set-key (kbd "<escape>r") 'point-to-register)

;; Linum mode
(define-key global-map [f4] 'linum-mode)

;; Web Jump
(require 'webjump)
(global-set-key (kbd "C-x j") 'webjump)
(setq webjump-sites '(
                      ("Github" . "github.com")
                      ("Qortex" . "qortex.com")
                      ("Qortex cn" . "qortex.cn")
                      ("Weibo" . "weibo.com")
                      ("CnBeta" . "cnbeta.com")
                      ("Gmail" . "gmail.com")
                      ("Google Drive" . "drive.google.com")
                      ("Google Calendar" . "calendar.google.com")
                      ("ThePlant Drive" . "drive.google.com/a/theplant.jp")
                      ("Sina Finance" . "finance.sina.com.cn")
                      ))

;; Find file at point
;; (ffap-bindings)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)


;; Smex
(global-set-key (kbd "<menu>") 'smex)

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
(global-set-key (kbd "<C-f1>") 'sauron-toggle-hide-show)

;; iCal
(require 'calfw-ical)
(setq calcred (netrc-machine (netrc-parse "~/.authinfo.gpg") "calics" t))
;; (cfw:open-ical-calendar (netrc-get cred "password"))
(require 'calfw-gcal)

;; Pomodoro
(global-set-key (kbd "<f5>") 'pomodoro)
(global-set-key (kbd "<M-f5>") 'pomodoro-status)

;; Windmove
(windmove-default-keybindings 'super)

;; Smart Window
(require 'smart-window)
(setq smart-window-remap-keys 0)
(global-set-key (kbd "C-c ,") 'smart-window-rotate)

;; Rename buffer
(global-set-key (kbd "C-x ,") 'rename-buffer)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun goto-emacs-setting-file ()
  (interactive)
  (if (not (get-buffer "settings.el"))
      (find-file (expand-file-name ".emacs.d/personal/settings.el" (getenv "HOME"))))
  (switch-to-buffer "settings.el"))

(defun goto-emacs-tips-file ()
  (interactive)
  (if (not (get-buffer "Emacs.org"))
      (find-file (expand-file-name "GIT/configure/Emacs.org" (getenv "HOME"))))
  (switch-to-buffer "Emacs.org"))

(global-set-key (kbd "<M-f1>") 'goto-emacs-setting-file)
(global-set-key (kbd "<C-f1>") 'goto-emacs-tips-file)
(global-set-key (kbd "<M-S-f1>") 'goto-last-dir)
(global-set-key (kbd "<C-f6>") 'command-history)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'search-forward-regexp)
(global-set-key (kbd "C-M-r") 'search-backward-regexp)

;; Doc View
(setq doc-view-continuous t)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
(define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
(define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)

;; Run in Daemon
(if (not (display-graphic-p))
    (progn
      (mu4e)
      )
  )

;; zlc mode
(zlc-mode t)
(setq zlc-select-completion-immediately t)
(let ((map minibuffer-local-map))
  ;;; like menu select
  (define-key map (kbd "<C-down>")  'zlc-select-next-vertical)
  (define-key map (kbd "<C-up>")    'zlc-select-previous-vertical)
  (define-key map (kbd "<C-right>") 'zlc-select-next)
  (define-key map (kbd "<C-left>")  'zlc-select-previous)

  ;;; reset selection
  (define-key map (kbd "C-c") 'zlc-reset)
  )

;; w3m
(global-set-key (kbd "<escape>w") 'w3m)
(global-set-key (kbd "<escape>W") 'browse-url-at-point)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "chromium")
(setq browse-url-browser-function 'w3m-browse-url)

(setq
   w3m-default-display-inline-images t
   w3m-command-arguments '("-cookie" "-F")
   w3m-use-cookies t
   w3m-use-mule-ucs t
   w3m-new-session-in-background t
   w3m-home-page "http://www.google.com"
 )
(require 'w3m-search)
(add-to-list 'w3m-search-engine-alist
             '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))

(defun w3m-copy-current-url ()
  "Display the current url in the echo area and put it into `kill-ring'."
  (interactive)
  (when w3m-current-url
    (let ((deactivate-mark nil))
      (kill-new w3m-current-url)
      (w3m-print-current-url))))

(add-hook 'w3m-mode-hook (lambda ()
   (define-key w3m-mode-map "n"     'w3m-next-anchor)
   (define-key w3m-mode-map "p"     'w3m-previous-anchor)
   (define-key w3m-mode-map "f"     'w3m-lnum-follow)
   (define-key w3m-mode-map "d"     'w3m-download-this-url)
   (define-key w3m-mode-map "Y"     'w3m-copy-current-url)
   (define-key w3m-mode-map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
   ))

(defun vpnonline-hook ()
  (interactive)
  (if (not (get-buffer "*-jabber-roster-*"))
      (jabber-connect-all))
  (if (not (get-buffer "*weibo-timeline*"))
      (weibo-timeline))
  (if (not (get-buffer ":home"))
      (twit))
  )
