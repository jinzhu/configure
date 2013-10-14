;; flycheck
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(define-key flycheck-mode-map (kbd "<f1>ej") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "<f1>ek") #'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "<f1>el") #'flycheck-list-errors)
(define-key flycheck-mode-map (kbd "<f1>eg") #'flycheck-google-message)

;; Disable warnning while edit emacs lisp scripts
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
