(require 'package)
;; My packages
(setq prelude-packages (append '(
                                 vline quickrun pos-tip auto-complete git-gutter go-eldoc
                                 ) prelude-packages))

;; Install my packages
(prelude-install-packages)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq my-packages
       '(el-get sudo-save xclip))

(el-get 'sync my-packages)


(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)			; makes sure tabs are not used.

;; Font
(load-theme 'monokai)
(set-default-font "Monaco-14")

;; git gutter
(global-git-gutter-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; xclip
(turn-on-xclip)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

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
(setq ac-use-quick-help nil)
;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)
;; (setq ac-auto-show-menu 0.8)

;; Select candidates with C-n/C-p only when completion menu is displayed
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
