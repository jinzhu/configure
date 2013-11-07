(require-packages '(flycheck google-this))
(require-package 'flycheck-color-mode-line)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; Disable warnning while edit emacs lisp scripts
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))

(bind-key "M-g p" 'previous-error)
(bind-key "M-g n" 'next-error)
(bind-key "M-g l" 'flycheck-list-errors)
(bind-key "M-g M-g" 'flycheck-google-message)

(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
