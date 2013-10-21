(require-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(global-subword-mode)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Go Mode
(require-packages '(go-mode go-eldoc))

;; Format / fix imports before every save.
(dolist (path exec-path)
  (if (file-exists-p (concat path "/goimports"))
      (add-hook 'before-save-hook 'goimports-before-save) (add-hook 'before-save-hook 'gofmt-before-save)
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
(require-packages '(yaml-mode coffee-mode js2-mode js3-mode markdown-mode textile-mode))

(require-package 'mmm-mode)

(defun set-up-mode-for-erb(mode)
  (require 'mmm-erb)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(mapc 'set-up-mode-for-erb '(html-mode html-erb-mode nxml-mode coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</script>" @)))
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css-inline
    :submode css-mode
    :face mmm-code-submode-face
    :front "style=\""
    :back "\"")))

(dolist (mode (list 'html-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))

;;; Use eldoc for syntax hints
(require-package 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;; crontab
(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")
