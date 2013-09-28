(require 'package)

;; Main line
(display-time-mode 1)
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

(require 'powerline)
(powerline-default-theme)

(global-hl-line-mode -1)

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

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary t)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Trailing whitespace is unnecessary
(setq prelude-clean-whitespace-on-save nil)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(add-hook 'before-save-hook (lambda () (prelude-indent-region-or-buffer)))

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
(key-chord-define-global "yy" 'copy-current-line)
(key-chord-define-global "vv" 'select-current-line)
(key-chord-define-global ";s" 'git-gutter:next-diff)
(key-chord-define-global ";d" 'git-gutter:previous-diff)
(key-chord-define-global ";w" 'save-buffer)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region-or-line)

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
(setq multi-term-dedicated-select-after-open-p t)

(defun term-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-l") 'term-clear)
                            (yas-minor-mode -1)
                            (ansi-color-for-comint-mode-on)))

;; (add-hook 'term-mode-hook 'term-line-mode)
;; By default, term-char-mode forwards most keys to the terminal
;; (add-hook 'term-mode-hook 'term-char-mode)

;; Bash Complete
(require 'shell-command)
(shell-command-completion-mode)
(require 'bash-completion)
(bash-completion-setup)

;; Twitter
(setq twittering-use-master-password t)

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
(setq jabber-show-offline-contacts nil)

;; Tramp
(setq tramp-default-method "ssh")

;; deft
(require 'deft)
(setq deft-directory "/jinzhu/Dropbox/Notebooks")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(global-set-key [f8] 'deft)
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
(require 'browse-kill-ring)
(global-set-key "\C-xy" 'browse-kill-ring)

;; Dired
(global-set-key (kbd "C-c j") 'dired-jump)

;; Keyfreq
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Point Undo
(require 'point-undo)
(define-key global-map [f5] 'point-undo)
(define-key global-map [f6] 'point-redo)

;; Smart Window
(require 'smart-window)
(setq smart-window-remap-keys 0)
(global-set-key (kbd "C-x ,") 'smart-window-rotate)

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
                      ("ThePlant Drive" . "https://drive.google.com/a/theplant.jp/#shared-")
                      ("Sina Finance" . "finance.sina.com.cn")
                      ))
