(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))


(setq package-list
      '(flymake-go flymake-coffee flymake-jslint flymake-ruby flymake-shell flymake-json flymake-sass
                   ack-and-a-half w3m))

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
   (:name color-theme
          :after (progn
                   (color-theme-initialize)
                   (color-theme-hober)
                   ))

   (:name auto-complete
          :after (progn
                   (require 'auto-complete-config)
                   (ac-config-default)
                   (ac-fuzzy-complete)
                   ;; Show 0.8 second later
                   (setq ac-auto-show-menu 0.8)
                   ))

   (:name magit
          :after (progn
                   (global-set-key (kbd "C-x g") 'magit-status)
                   ))

   (:name ace-jump-mode
          :after (progn
                   (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
                   ))

   (:name expand-region
          :after (progn
                   (global-set-key (kbd "C-@") 'er/expand-region)
                   ))

   (:name find-file-in-project
          :after (progn
                   (global-set-key (kbd "C-p") 'find-file-in-project)
                   ))

   ;; Smex, <C-h f> -> describe-function, <M-.> -> definition, <C-h w> -> key bindings
   (:name smex
          :after (progn
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                   ))

   (:name yasnippet
          :after (yas-global-mode 1)
          )

   (:name evil
          :after (evil-mode 1)
          )

   (:name evil-leader
          :after (progn
                   (evil-leader/set-leader ";")
                   (evil-leader/set-key
                     "b" 'switch-to-buffer
                     "k" 'kill-buffer
                     "w" 'save-buffer
                     )
                   ))
   ))

(setq my-packages
      (append
       '(
         el-get switch-window undo-tree
                quickrun multiple-cursors
                coffee-mode js2-mode markdown-mode sass-mode scss-mode css-mode yaml-mode ruby-mode go-mode rspec-mode
                yari rinari ruby-test-mode rvm
                )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)


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

;; Key Bindings
(global-set-key (kbd "<f12>") 'menu-bar-mod)

;; Flymake
(flymake-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(Global-set-key (kbd "C-p") 'find-file-in-project)
