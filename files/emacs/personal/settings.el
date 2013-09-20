(require 'package)

(setq prelude-clean-whitespace-on-save nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)			; makes sure tabs are not used.

;; Theme & Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes")
(load-theme 'monokai)
(set-default-font "Monaco-14")
(setq default-frame-alist '((font . "Monaco-14"))) ;; emacs --daemon

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

(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; xclip
(turn-on-xclip)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(add-hook 'before-save-hook (lambda () (prelude-indent-region-or-buffer)))

;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'go-autocomplete)

;; Select candidates with C-n/C-p only when completion menu is displayed
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)


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
(key-chord-define-global "ss" 'git-gutter:next-diff)
(key-chord-define-global "sd" 'git-gutter:previous-diff)
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
(global-set-key (kbd "<f3>") 'multi-term-dedicated-open)
(global-set-key (kbd "<C-f3>") 'multi-term)
(setq multi-term-dedicated-select-after-open-p t)
