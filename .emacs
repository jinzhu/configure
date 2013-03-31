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
                   helm projectile helm-projectile undo-tree multiple-cursors w3m smex
                   evil evil-leader evil-nerd-commenter switch-window
                   ack-and-a-half ace-jump-mode expand-region quickrun
		   xclip gist pos-tip
		   session

                   ;; GIT ;;git-gutter-fringe
                   magit vline
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
   (:name youdao
          :type github
          :pkgname "zhenze12345/youdao.el"
          :after (progn
                   (require 'youdao)
                   ;; http://fanyi.youdao.com/openapi?path=data-mode
                   (setf youdao-key-from "jinzhu")
                   (setf youdao-key "1159909992")
                   (global-set-key (kbd "C-c C-v") 'youdao-translate-word)
                   ))
   (:name weibo
          :type github
          :pkgname "austin-----/weibo.emacs"
          :after (require 'weibo)
          )
   ))

(setq my-packages
      (append
       '(el-get rhtml-mode evil-surround sudo-save mmm-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; Themes
(color-theme-initialize)
(color-theme-hober)

;; Global Setting
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(display-time-mode 1)

(setq
 make-backup-files nil
 kept-old-versions 2
 kept-new-versions 200
 delete-old-versions t
 version-control t
 auto-save-default nil
 tab-width 2
 indent-tabs-mode nil
 tab-always-indent nil
 inhibit-startup-message t
 display-time-24hr-format t
 display-time-day-and-date 0
 )
(global-auto-revert-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10)

(blink-cursor-mode -1)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(setq show-trailing-whitespace t)

(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(electric-pair-mode t)
(electric-indent-mode +1)

(global-set-key (kbd "RET") 'newline-and-indent)

;; Key Bindings
(global-set-key (kbd "<f12>") 'menu-bar-mode)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; xclip
(xclip-mode 1)
(turn-on-xclip)

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
(global-set-key (kbd "M-m") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ace Jump Mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Expand Region
(global-set-key (kbd "C-@") 'er/expand-region)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Hippie Expand
(global-set-key (kbd "M-\\") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-complete-file-name-partially try-complete-file-name
			       try-expand-dabbrev try-expand-dabbrev-visible
			       try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
			       try-complete-lisp-symbol-partially try-complete-lisp-symbol
			       try-expand-tag))

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.8)

;; CoffeeMode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Emamux
(require 'emamux)
(require 'emamux-ruby-test)

;; ERC
(add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
(setq erc-fill-column 70)

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
  "tl" 'emamux:run-last-command
  "ts" 'emamux:send-command
  "ti" 'emamux:inspect-runner
  "tq" 'emamux:close-runner-pane
  "tx" 'emamux:close-panes
  "tr" 'emamux-ruby-test:run-all
  "tR" 'emamux-ruby-test:run-focused-test
  "gd" 'vc-diff
  "gs" 'magit-status
  )

;; Recentf
(setq recentf-max-saved-items 200
      recentf-max-menu-items 25)
(recentf-mode +1)

;; Projectile
(projectile-global-mode)
;; (setq projectile-require-project-root nil)
(setq projectile-enable-caching nil)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(require 'helm-projectile)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
	  ;; add project files and buffers when in project
	  (helm-other-buffer '(helm-c-source-projectile-files-list
			       helm-c-source-projectile-buffers-list
			       helm-c-source-buffers-list
			       helm-c-source-recentf
			       helm-c-source-buffer-not-found)
			     "*helm prelude*")
	;; otherwise fallback to helm-mini
	(helm-mini))
    ;; fall back to helm mini if an error occurs (usually in projectile-project-root)
    (error (helm-mini))))

;; EVIL Surround
(global-surround-mode 1)

;; Evil Nerd Commenter
(define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd "gc") 'comment-or-uncomment-region)
(define-key evil-insert-state-map (kbd "M-j") 'next-line)
(define-key evil-insert-state-map (kbd "M-k") 'previous-line)
(define-key evil-insert-state-map (kbd "M-l") 'right-char)
(define-key evil-insert-state-map (kbd "M-h") 'left-char)

;; Undo
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

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

;; MMM Mode
;; (require 'mmm-mode)
;; (require 'mmm-auto)

;; Session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; IDO
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

(eval-after-load 'mu4e
  '(progn
     ;; use the standard bindings as a base
     (evil-make-overriding-map mu4e-view-mode-map 'normal t)
     (evil-make-overriding-map mu4e-main-mode-map 'normal t)
     (evil-make-overriding-map mu4e-headers-mode-map 'normal t)

     (evil-add-hjkl-bindings mu4e-view-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message
       "Q" 'mu4e-raw-view-quit-buffer)

     (evil-add-hjkl-bindings mu4e-view-raw-mode-map 'normal
       "J" 'mu4e-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "q" 'mu4e-raw-view-quit-buffer)

     (evil-add-hjkl-bindings mu4e-headers-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message
       )

     (evil-add-hjkl-bindings mu4e-main-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "RET" 'mu4e-view-message)
     ))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(cond ((get-buffer new-name)
	       (message "A buffer named '%s' already exists!" new-name))
	      (t
	       (rename-file name new-name 1)
	       (rename-buffer new-name)
	       (set-visited-file-name new-name)
	       (set-buffer-modified-p nil)))))))


(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Google: "))))))

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
		    (if (eq system-type 'darwin)
			"open"
		      (read-shell-command "Open current file with: "))
		    " "
		    buffer-file-name))))

;; MMM Mode
