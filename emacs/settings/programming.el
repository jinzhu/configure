(require-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;; Go Mode
(require-package 'go-mode)
(setq go-save-hook #'gofmt-before-save)  ;; gofmt by default
(dolist (path exec-path)
  (when (file-exists-p (concat path "/goimports"))
    (setq go-save-hook #'goimports-before-save)))

;; Format / fix imports before every save.
(add-hook 'go-mode-hook
          '(lambda()
             (add-hook 'before-save-hook go-save-hook)
             (flycheck-declare-checker go-fmt)
             ))

(add-hook 'go-mode-hook 'go-eldoc-setup)

;; Ruby Mode
(require-packages '(ruby-mode ruby-hash-syntax rvm yari ruby-compilation inf-ruby))
(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" ".irbrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"
               "Capfile\\'" "Guardfile\\'")

(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Web
(require-packages '(mmm-mode yaml-mode coffee-mode js2-mode js3-mode markdown-mode textile-mode))
