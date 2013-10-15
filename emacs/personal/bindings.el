(global-set-key [remap kill-ring-save] 'easy-kill)

(bind-key "<escape>C-j" 'join-region)
(bind-key "<escape>E" 'eval-buffer)
(bind-key "<escape>e" 'eval-region)
(bind-key "<escape>i" 'prelude-indent-buffer)
(bind-key "<f1><left>" 'previous-buffer)
(bind-key "<f1><right>" 'next-buffer)

;; Pomodoro
(bind-key "<f5>" 'pomodoro)
(bind-key "<M-f5>" 'pomodoro-status)

;; Windmove
(windmove-default-keybindings 'super)

;; Rename buffer
(bind-key "C-x ," 'rename-buffer)

(bind-key "<M-f1>" 'goto-emacs-setting-file)
(bind-key "<C-f1>" 'goto-emacs-tips-file)
(bind-key "<M-S-f1>" 'goto-last-dir)
(bind-key "<C-f6>" 'command-history)

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

(bind-key "C-c C-f" 'find-file-at-point)

;; Smex
(bind-key "<menu>" 'smex)

(bind-key "<escape>p" 'point-undo)
(bind-key "<escape>n" 'point-redo)
(bind-key "<escape>t" 'jump-to-register)
(bind-key "<escape>r" 'point-to-register)

;; Linum mode
(bind-key "<f4>" 'global-linum-mode)

;; Dired
(bind-key "C-c j" 'dired-jump)

;; Browse Kill Ring
(bind-key "C-x y" 'helm-show-kill-ring)

;;; key choard
(setq key-chord-two-keys-delay 0.2)
(bind-key "<escape>yy" 'copy-current-line)
(key-chord-define-global "yy" 'copy-current-line)
(bind-key "<escape>vv" 'select-current-line)
(key-chord-define-global "vv" 'select-current-line)
(bind-key "<escape>m" 'magit-status)
(key-chord-define-global ";w" 'save-buffer)
(bind-key "C-c C-c" 'comment-or-uncomment-region-or-line)
(bind-key "<escape>l" 'helm-all-mark-rings)
(bind-key "<escape>s" 'smartparens-strict-mode)
(bind-key "<escape>cd" 'goto-last-dir)
(bind-key "s-b" 'helm-buffers-list)

;; Evil Mode
; (require 'evil)
; (bind-key "C-*" 'evil-search-symbol-forward)
; (bind-key "C-#" 'evil-search-symbol-backward)
