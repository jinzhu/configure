(use-package flycheck
  :init (progn
         (eval-after-load "flycheck"
           '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

         (define-key flycheck-mode-map (kbd "<escape><escape>j") #'flycheck-next-error)
         (define-key flycheck-mode-map (kbd "<escape><escape>k") #'flycheck-previous-error)
         (define-key flycheck-mode-map (kbd "<escape><escape>l") #'flycheck-list-errors)
         (define-key flycheck-mode-map (kbd "<escape><escape>g") #'flycheck-google-message)

         ;; Disable warnning while edit emacs lisp scripts
         (eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
         )
  )
