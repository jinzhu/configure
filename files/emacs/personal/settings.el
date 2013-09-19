(require 'package)

(setq prelude-clean-whitespace-on-save nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)			; makes sure tabs are not used.

;; Font
(load-theme 'monokai)
(set-default-font "Monaco-14")

;; Sudo Save
(require 'sudo-save)

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

(key-chord-define-global "yy" 'copy-current-line)
(key-chord-define-global "vv" 'select-current-line)
