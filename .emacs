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
        flymake-go flymake-coffee flymake-jslint flymake-ruby flymake-sass

                   ;; MODES
                   coffee-mode js2-mode markdown-mode scss-mode css-mode yaml-mode csv-mode zencoding-mode
                   php-mode ruby-mode go-mode rspec-mode

                   ;; Themes
                   color-theme

                   ;; AutoComplete
                   auto-complete

                   ;; Tools
                   magit helm projectile undo-tree multiple-cursors w3m find-file-in-project smex
                   evil evil-leader evil-nerd-commenter switch-window
                   ack-and-a-half ace-jump-mode expand-region
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
   ))

(setq my-packages
      (append
       '(el-get rhtml-mode evil-surround)
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
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)

(global-set-key (kbd "RET") 'newline-and-indent)
(electric-indent-mode +1)

;; Key Bindings
(global-set-key (kbd "<f12>") 'menu-bar-mod)

;; Flymake
(flymake-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Tools

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
(setq ac-auto-show-menu 0.8)

;; Evil, VIM Mode
(evil-mode 1)
(require 'evil-leader)
(evil-leader/set-leader ";")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "w" 'save-buffer
  "r" 'projectile-recentf
  "a" 'projectile-ack
  )

;; Recentf
(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; Projectile
(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-enable-caching nil)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

;; Evil Surround
(global-surround-mode 1)

;; Evil Nerd Commenter
(define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd "gc") 'comment-region)

;; Switch Window
(require 'switch-window)

(ido-mode)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el"
                                    ".ini" ".cfg" ".conf" ".rb" ".rake" ".coffee" ".scss"))

;; (setq gnus-select-method '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))
