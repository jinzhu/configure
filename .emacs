(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))


(setq package-list
      '(
        ;; Flymake
        flymake-go flymake-coffee flymake-jslint flymake-ruby flymake-sass flycheck

                   ;; MODES
                   coffee-mode js2-mode markdown-mode scss-mode css-mode yaml-mode csv-mode zencoding-mode
                   git-commit-mode gitconfig-mode gitignore-mode
                   php-mode ruby-mode go-mode rspec-mode
                   multi-web-mode

                   ;; Language
                   inf-ruby ruby-electric ruby-tools ruby-end rinari yari

                   ;; Themes
                   color-theme

                   ;; AutoComplete
                   auto-complete

                   ;; Tools
                   helm projectile undo-tree multiple-cursors w3m smex
                   evil evil-leader evil-nerd-commenter switch-window
                   ack-and-a-half ace-jump-mode expand-region quickrun
                   gist powerline

                   ;; GIT
                   magit ;;git-gutter-fringe
                   vline
                   ))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq
 el-get-sources
 '(
   (:name yasnippet
          :after (yas-global-mode 1)
          )
   (:name  auto-complete-yasnippet
           :after (require 'auto-complete-yasnippet)
           )
   (:name emacs-emamux
          :type github
          :pkgname "syohex/emacs-emamux"
          )
   (:name emamux-ruby-test
          :type github
          :pkgname "syohex/emamux-ruby-test"
          )
   ))

(setq my-packages
      (append
       '(el-get rhtml-mode evil-surround sudo-save)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; Themes
(color-theme-initialize)
(color-theme-hober)

;; Global Setting
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq make-backup-files nil)
(setq kept-old-versions 2)
(setq kept-new-versions 200)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp")))

(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(blink-cursor-mode -1)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(setq show-trailing-whitespace t)

(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(electric-pair-mode)
(electric-indent-mode +1)

(global-set-key (kbd "RET") 'newline-and-indent)

;; Key Bindings
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Use regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; Dired
(setq dired-recursive-deletes 'top)

;; Multiple Cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Flymake
(flymake-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Smex <C-h f> -> describe-function, <M-.> -> definition, <C-h w> -> key bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ace Jump Mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Expand Region
(global-set-key (kbd "C-@") 'er/expand-region)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.8)


;; Emamux
(require 'emamux)
(require 'emamux-ruby-test)

;; Evil, VIM Mode
(evil-mode 1)
(require 'evil-leader)
(evil-leader/set-leader ";")
(evil-leader/set-key
  "b" 'ido-switch-buffer
  "k" 'kill-buffer
  "w" 'save-buffer
  "r" 'projectile-recentf
  "a" 'projectile-ack
  "tp" 'emamux:run-command
  "ts" 'emamux:send-command
  "ti" 'emamux:inspect-runner
  "tq" 'emamux:close-runner-pane
  "tx" 'emamux:close-panes
  "tr" 'emamux-ruby-test:run-all
  "tR" 'emamux-ruby-test:run-focused-test
  )

;; Recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Projectile
(projectile-global-mode)
;; (setq projectile-require-project-root nil)
(setq projectile-enable-caching nil)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

;; Evil Surround
(global-surround-mode 1)

;; Evil Nerd Commenter
(define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd "gc") 'comment-region)
(define-key evil-insert-state-map (kbd "M-j") 'next-line)
(define-key evil-insert-state-map (kbd "M-k") 'previous-line)
(define-key evil-insert-state-map (kbd "M-l") 'right-char)
(define-key evil-insert-state-map (kbd "M-h") 'left-char)


;; Switch Window
(require 'switch-window)

;; Ruby Mode
(defalias 'ri 'yari)
(add-hook 'ruby-mode-hook (electric-pair-mode 0))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("sake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("god$" . ruby-mode))
(add-to-list 'auto-mode-alist '("thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.spec$" . ruby-mode))

(require 'ruby-electric)
(require 'ruby-end)

;; RHTML Mode
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Multi Web Mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'rhtml-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "erb" "rhtml"))
(multi-web-global-mode)

;; IDO
(ido-mode)
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


;; Email
(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

(setq
 user-mail-address "wosmvp@gmail.com"
 user-full-name  "Jinzhu"
 )

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
