(global-set-key [remap kill-ring-save] 'easy-kill)

(bind-key "<escape>E" 'eval-buffer)
(bind-key "<escape>e" 'eval-region)

(bind-key "<f1>j" 'join-region)
(bind-key "<f1>i" 'prelude-indent-buffer)

;; tabbar <escape> up/down/right/left
(bind-key "<f1><left>"  'previous-buffer)
(bind-key "<f1><right>" 'next-buffer)
(bind-key "<f1>1" 'delete-other-windows)
(bind-key "<f1>0" 'delete-window)
(bind-key "<f1>2" 'split-window-below)
(bind-key "<f1>3" 'split-window-right)

(bind-key "<f1>k" 'kill-this-buffer)
(bind-key "<f1><f1>r" 'rename-buffer)

(bind-key "<f1>b" 'helm-buffers-list)
(bind-key "<f1>f" 'projectile-find-file)
(bind-key "<f1>r" 'prelude-recentf-ido-find-file)
(bind-key "<f1>h" 'helm-projectile)


(bind-key "<escape><up>"    'windmove-up)
(bind-key "<escape><down>"  'windmove-down)
(bind-key "<escape><right>" 'windmove-right)
(bind-key "<escape><left>"  'windmove-left)

(bind-key "<f1>l" 'helm-all-mark-rings)
(bind-key "<escape>p" 'point-undo)
(bind-key "<escape>n" 'point-redo)
(bind-key "<escape>t" 'jump-to-register)
(bind-key "<escape>r" 'point-to-register)

;; Pomodoro
(bind-key "<f5>" 'pomodoro)
(bind-key "<M-f5>" 'pomodoro-status)

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
(key-chord-define-global ";w" 'save-buffer)

(bind-key "<escape>m" 'magit-status)
(bind-key "<f1>m" 'magit-status)

(bind-key "C-c C-c" 'comment-or-uncomment-region-or-line)
(bind-key "<escape>s" 'smartparens-strict-mode)
(bind-key "<escape>cd" 'goto-last-dir)

;; Evil Mode
;; (require 'evil)
;; (bind-key "C-*" 'evil-search-symbol-forward)
;; (bind-key "C-#" 'evil-search-symbol-backward)
