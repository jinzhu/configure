(require-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(global-subword-mode)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Lisp
(require-packages '(slime elisp-slime-nav))
(slime-setup)

;; Android
(require-packages '(android-mode))
(setq android-mode-sdk-dir "/opt/android-sdk/")
(gradle-mode 1)
(evil-leader/set-key
  ";i" '(lambda () (interactive) (gradle-execute '("installDebug" "--daemon")))
  ";a" '(lambda () (interactive) (android-start-app)))

;; Go Mode
(require-packages '(go-mode go-eldoc gore-mode))
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Ruby Mode
(require-packages '(ruby-mode ruby-hash-syntax yari ruby-compilation inf-ruby))
(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" ".irbrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"
               "Capfile\\'" "Guardfile\\'")

(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

(require-packages '(rinari rhtml-mode))
(setq ruby-insert-encoding-magic-comment nil)
(global-rinari-mode)

;; Web
(require-packages '(yaml-mode coffee-mode js2-mode js3-mode markdown-mode textile-mode web-beautify emmet-mode))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

(setq web-beautify-args  '("--indent-size" "2" "-f" "-"))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; (require-package 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (setq web-mode-engines-alist
;;       '(("erb"    . "\\.erb\\'")
;;         ))

(require-package 'mmm-mode)
(require 'mmm-defaults)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

(require-package 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((ruby-mode "<%==\\|<%=\\|<%#\\|<%" "%>")
                  (js3-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "erb"))
(multi-web-global-mode 1)

;;; Use eldoc for syntax hints
(require-package 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;; crontab
(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(require-package 'htmlize)
