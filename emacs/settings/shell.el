;; Tramp
(setq
 tramp-default-method "ssh"
 tramp-chunksize 500
 )

;; Multi Term
(require-package 'multi-term)
(require-package 'shell-command)
(shell-command-completion-mode)
(require-package 'bash-completion)
(bash-completion-setup)

(setq multi-term-program "/bin/zsh"
      multi-term-dedicated-select-after-open-p t)

(add-hook 'term-mode-hook (lambda ()
                            (bind-key "C-y"              'term-paste                term-raw-map)
                            (bind-key "C-l"              'term-send-raw             term-raw-map)
                            (bind-key "C-j"              'term-switch-to-shell-mode term-raw-map)
                            (bind-key "<escape><escape>" 'term-send-raw-meta        term-raw-map)
                            (bind-key "M-."              'term-send-raw-meta        term-raw-map)
                            (bind-key "C-c C-j"          'term-line-mode            term-raw-map)
                            (bind-key "C-c C-k"          'term-char-mode            term-raw-map)
                            (yas-minor-mode -1)
                            (ansi-color-for-comint-mode-on)
                            (set (make-local-variable 'outline-regexp) "\$ ")
                            (outline-minor-mode)
                            ))

(add-hook 'shell-mode-hook (lambda ()
                             (bind-key "<up>" 'term-send-up shell-mode-map)
                             (bind-key "<down>" 'term-send-down shell-mode-map)
                             (yas-minor-mode -1)
                             (ansi-color-for-comint-mode-on)
                             ))

(bind-key "<f1><f1>d" 'multi-term-dedicated-toggle)
(bind-key "<f1><f1>c" 'multi-term)
(bind-key "<f1><f1>n" 'multi-term-next)
(bind-key "<f1><f1>p" 'multi-term-prev)


;; eshell
(require-package 'multi-eshell)
(setq
 multi-eshell-name "*eshell*"
 multi-eshell-shell-function '(eshell)
 )

(require-package 'eshell)
(require-package 'esh-buf-stack)
(require 'em-zle)

(require 'em-smart)

(setq
 ;; eshell-directory-name (expand-file-name "eshell" cache-dir)

 eshell-last-dir-ring-size 100

 comint-scroll-to-bottom-on-input t  ; always insert at the bottom
 comint-scroll-to-bottom-on-output nil ; always add output at the bottom
 comint-scroll-show-maximum-output t
 comint-input-ignoredups t ; no duplicates in command history
 comint-completion-addsuffix t ; insert space/slash after file completion
 comint-buffer-maximum-size 200000

 eshell-save-history-on-exit   t
 eshell-history-size           1024
 eshell-hist-ignoredups        t

 eshell-cmpl-ignore-case       t
 eshell-cmpl-cycle-completions t
 eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"

 eshell-cp-interactive-query   t
 eshell-ln-interactive-query   t
 eshell-mv-interactive-query   t
 eshell-rm-interactive-query   t
 eshell-mv-overwrite-files     nil
 eshell-where-to-jump 'begin
 eshell-review-quick-commands nil
 eshell-smart-space-goes-to-end t

 eshell-scroll-to-bottom-on-output t
 eshell-scroll-show-maximum-output t

 eshell-highlight-prompt   t
 eshell-prompt-regexp "^[^#$\n]*\\([[:digit:]][\*\'\"\>]\\|[#$]\\) "

 eshell-aliases-file "~/.emacs.d/alias"
 eshell-visual-commands (quote ("vim" "tail" "less" "htop"))
 )

(defun eshell-zle-sudo-command ()
  (interactive "*")
  (let ((cmd (caar (eshell-hist-parse-arguments
                    'silent
                    (save-excursion (eshell-bol) (point))
                    (point-at-eol)))))
    (when (not cmd) (eshell-zle-up-history 1))
    (eshell-bol)
    (insert "sudo ")
    (end-of-line)
    ))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun user-send-input (&optional use-region queue-p no-newline)
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let ((proc-running-p (and (eshell-interactive-process)
                             (not queue-p)))
        (inhibit-point-motion-hooks t)
        after-change-functions)

    (if proc-running-p
        (eshell-add-input-to-history (eshell-get-old-input))
      )
    )

  (eshell-send-input use-region queue-p no-newline)
  )

(setup-eshell-buf-stack)
(add-hook 'eshell-mode-hook (lambda ()
                              (bind-key "M-q"   'eshell-push-command                eshell-mode-map)
                              (bind-key "M-."   'eshell-zle-insert-last-word        eshell-mode-map)
                              (bind-key "M-a"   'eshell-zle-accept-and-hold         eshell-mode-map)
                              (bind-key "M-_"   'eshell-zle-copy-prev-shell-word    eshell-mode-map)
                              (bind-key "M-RET" 'eshell-zle-push-line               eshell-mode-map)
                              (bind-key "M-g"   'eshell-zle-get-line                eshell-mode-map)
                              (bind-key "M-?"   'eshell-zle-which-command           eshell-mode-map)
                              (bind-key "C-r"   'eshell-isearch-backward            eshell-mode-map)
                              (bind-key "<escape><escape>" 'eshell-zle-sudo-command eshell-mode-map)
                              (bind-key "<C-backspace>" 'eshell-zle-kill-whole-line eshell-mode-map)
                              (bind-key "M-p"      'helm-eshell-history             eshell-mode-map)
                              (bind-key "<return>" 'user-send-input                 eshell-mode-map)

                              (ansi-color-for-comint-mode-on)
                              (set (make-local-variable 'outline-regexp) "\$" )
                              (add-to-list 'ac-sources `ac-source-eshell-pcomplete)
                              (auto-complete-mode t)
                              (outline-minor-mode)
                              ))

(bind-key "<f1>d" 'eshell)
(bind-key "<f1>c" `(lambda () (interactive) (eshell "-")))
(bind-key "<f1>n" 'multi-eshell-switch)
(bind-key "<f1>p" 'multi-eshell-go-back)
(bind-key "<f1><f1>l" 'eshell-show-output)
(bind-key "<f1><f1>b" 'eshell-insert-buffer-name)

(setq eshell-path-env (concat (expand-file-name "~/.gem/ruby/2.0.0/bin:") eshell-path-env))
