(require 'go-imports)    ;; the other file in this gist

;; Is goimports available?
(setq go-save-hook #'gofmt-before-save)  ;; gofmt by default
(dolist (path exec-path)
  (when (file-exists-p (concat path "/goimports"))
    (setq go-save-hook #'goimports-before-save)))

;; Format / fix imports before every save.
(add-hook 'go-mode-hook
    '(lambda()
             (add-hook 'before-save-hook go-save-hook)))
