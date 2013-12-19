(require-packages '(evil evil-matchit surround evil-leader))

(global-evil-leader-mode)
(evil-leader/set-leader ";")
(evil-mode 1)
(global-surround-mode 1)

(setq evil-normal-state-tag   (propertize " N " 'face '((:background "green" :foreground "black"))) ;; NORMAL
      evil-emacs-state-tag    (propertize " E " 'face '((:background "orange" :foreground "black"))) ;; EMACS
      evil-insert-state-tag   (propertize " I " 'face '((:background "red")))  ;; INSERT
      evil-motion-state-tag   (propertize " M " 'face '((:background "blue")))  ;; MOTION
      evil-visual-state-tag   (propertize " V " 'face '((:background "grey80" :foreground "black"))) ;; VISUAL
      evil-operator-state-tag (propertize " O " 'face '((:background "purple")))) ;; OPER

;; In insert mode, use Emacs mode
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(bind-key "gT" 'tabbar-backward-tab evil-normal-state-map)
(bind-key "gt" 'tabbar-forward-tab evil-normal-state-map)
(bind-key "C-e" 'evil-end-of-line evil-normal-state-map)
(bind-key "<SPC><SPC>" 'ace-jump-word-mode evil-normal-state-map)
(bind-key "C-c <SPC>" 'ace-jump-char-mode evil-normal-state-map)
(key-chord-define evil-insert-state-map ";w"  '(lambda () (interactive) (evil-normal-state) (save-buffer)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(evil-leader/set-key
  "a" 'projectile-ag
  "A" 'projectile-ack
  "b" 'ido-switch-buffer
  ";b" 'ace-jump-buffer
  "c" 'kill-this-buffer
  "B" 'projectile-switch-to-buffer
  "r" 'projectile-recentf
  "f" 'projectile-find-file
  "F" 'projectile-find-file-in-directory
  "d" 'projectile-find-dir
  "t" 'projectile-find-test-file
  "T" 'projectile-regenerate-tags
  "R" 'projectile-replace
  "p" 'projectile-switch-project
  "K" 'projectile-kill-buffers
  )

(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (shell-mode . insert)
                              (term-mode . insert)
                              (magit-mode . insert)
                              (git-commit-mode . insert))
      do (evil-set-initial-state mode state))
