(global-set-key [remap kill-ring-save] 'easy-kill)

(bind-key "<escape>E" 'eval-buffer)
(bind-key "<escape>e" 'eval-region)

(bind-key "<f1>j" 'join-region)
(bind-key "<f1>i" 'prelude-indent-buffer)

;; tabbar <escape> up/down/right/left
(bind-key "<f1><up>"  'previous-buffer)
(bind-key "<f1><down>" 'next-buffer)
(bind-key "<f1>1" 'delete-other-windows)
(bind-key "<f1>0" 'delete-window)
(bind-key "<f1>2" 'split-window-below)
(bind-key "<f1>3" 'split-window-right)

(bind-key "<f1>k" 'kill-this-buffer)
(bind-key "<f1><f1>r" 'rename-buffer)
(bind-key "<f1><f1>R" 'revert-buffer)


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

(bind-key "<M-f1>" 'goto-emacs-setting-file)
(bind-key "<C-f1>" 'goto-emacs-tips-file)
(bind-key "<M-S-f1>" 'goto-last-dir)
(bind-key "<C-f6>" 'command-history)

(bind-key "C-s" 'isearch-forward-at-point)
(bind-key "C-r" 'isearch-backward-regexp)
(define-key isearch-mode-map [up] 'isearch-repeat-backward)
(define-key isearch-mode-map [down] 'isearch-repeat-forward)

(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "<escape><escape>s" 'isearch-forward-at-point)

(bind-key "C-c C-f" 'find-file-at-point)

;; Smex
(bind-key "<menu>" 'smex)

;; Dired
(bind-key "C-c j" 'dired-jump)

;; Browse Kill Ring
(use-package browse-kill-ring
  :init (progn
          (setq browse-kill-ring-highlight-current-entry t
                browse-kill-ring-display-duplicates nil)

          (browse-kill-ring-default-keybindings)
          (define-key browse-kill-ring-mode-map (kbd "C-n")      'browse-kill-ring-forward)
          (define-key browse-kill-ring-mode-map (kbd "C-p")      'browse-kill-ring-previous)
          (define-key browse-kill-ring-mode-map (kbd "C-g")      'browse-kill-ring-quit)
          (define-key browse-kill-ring-mode-map (kbd "<escape>") 'browse-kill-ring-quit)
          )
  :bind ("C-x y" . browse-kill-ring)
  )

;;; key choard
(setq key-chord-two-keys-delay 0.2)
(bind-key "<escape>yy" 'copy-current-line)
(key-chord-define-global "yy" 'copy-current-line)
(bind-key "<escape>vv" 'select-current-line)
(key-chord-define-global "vv" 'select-current-line)
(key-chord-define-global ";w" 'save-buffer)

(bind-key "C-c C-c" 'comment-or-uncomment-region-or-line)
(bind-key "<escape>s" 'smartparens-strict-mode)
(bind-key "<escape>cd" 'goto-last-dir)

;; Line & Vline
(bind-key "<f4>v" 'vline-global-mode)
(bind-key "<f4>l" 'global-linum-mode)

;; Pomodoro
(bind-key "<f4>p" 'pomodoro)
(bind-key "<f4><f4>p" 'pomodoro-status)

;; Quickrun
(bind-key "<f4>q" 'quickrun)
(bind-key "<f4><f4>q" 'quickrun-region)

;; Switch Window
(bind-key "C-x o" 'switch-window)

;; Move Faster
(bind-key "<S-down>" '(lambda () (interactive) (forward-line 5))) ; Move the cursor around fast with shift
(bind-key "<S-up>" '(lambda () (interactive) (forward-line -5)))
(bind-key "<S-right>" '(lambda () (interactive) (forward-word 3)))
(bind-key "<S-left>" '(lambda () (interactive) (backward-word 3)))

;; Mark (useful when transient-mark-mode disabled)
(bind-key "M-`" 'jump-to-mark)
(bind-key "C-`" 'push-mark-no-activate)

(bind-key "<f4>i" 'helm-semantic-or-imenu)
(bind-key "<f4>t" 'helm-etags-select)
