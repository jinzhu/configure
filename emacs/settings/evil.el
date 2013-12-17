(require-packages '(evil evil-matchit surround))

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

(define-key evil-normal-state-map (kbd "gT") 'tabbar-backward-tab)
(define-key evil-normal-state-map (kbd "gt") 'tabbar-forward-tab)
