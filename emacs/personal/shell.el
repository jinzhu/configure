;; ;; Bash Complete
;; (require 'shell-command)
;; (shell-command-completion-mode)
;; (require 'bash-completion)
;; (bash-completion-setup)

;; Tramp
(setq
 tramp-default-method "ssh"
 tramp-chunksize 500
 )

;; Multi Term
(use-package multi-term
  :init (progn
          (setq multi-term-program "/bin/zsh")
          (setq multi-term-dedicated-select-after-open-p t)

          (defun term-switch-to-shell-mode ()
            (interactive)
            (if (equal major-mode 'term-mode)
                (progn
                  (shell-mode)
                  (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
                  (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
                  (compilation-shell-minor-mode 1)
                  (comint-send-input)
                  )
              (progn
                (compilation-shell-minor-mode -1)
                (font-lock-mode -1)
                (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
                (term-mode)
                (term-char-mode)
                (term-send-raw-string (kbd "C-l"))
                )))

          (defun goto-last-dir ()
            (interactive)
            (shell-process-cd
             (replace-regexp-in-string "\n" "" (get-string-from-file "/tmp/.last_dir"))))

          (add-hook 'term-mode-hook (lambda ()
                                      (define-key term-raw-map (kbd "C-y") 'term-paste)
                                      (define-key term-raw-map (kbd "C-l") 'term-send-raw)
                                      (define-key term-raw-map (kbd "C-j") 'term-switch-to-shell-mode)
                                      (define-key term-raw-map (kbd "<escape><escape>") 'term-send-raw-meta)
                                      (define-key term-raw-map (kbd "M-.") 'term-send-raw-meta)
                                      (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
                                      (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                                      (yas-minor-mode -1)
                                      (ansi-color-for-comint-mode-on)
                                      (set (make-local-variable 'outline-regexp) "\$ ")
                                      (outline-minor-mode)
                                      ))

          (add-hook 'shell-mode-hook (lambda ()
                                       (define-key shell-mode-map (kbd "<up>") 'term-send-up)
                                       (define-key shell-mode-map (kbd "<down>") 'term-send-down)
                                       (yas-minor-mode -1)
                                       (ansi-color-for-comint-mode-on)
                                       ))
          )
  :bind (("<f1><f1>d" . multi-term-dedicated-toggle)
         ("<f1><f1>c" . multi-term)
         ("<f1><f1>n" . multi-term-next)
         ("<f1><f1>p" . multi-term-prev)
         ))


;; Eshell
(use-package multi-eshell
  :init (setq
         multi-eshell-name "*eshell*"
         multi-eshell-shell-function '(eshell)
         )
  )

(use-package eshell
  :init (progn
          (require 'em-smart)

          (setq
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

           eshell-aliases-file "~/.emacs.d/personal/alias"
           eshell-visual-commands (quote ("vim" "tail" "less"))
           )

          (use-package em-zle)
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
                                        (local-set-key (kbd "M-q") 'eshell-push-command)
                                        (define-key eshell-mode-map (kbd "M-.") 'eshell-zle-insert-last-word)
                                        (define-key eshell-mode-map (kbd "M-a") 'eshell-zle-accept-and-hold)
                                        (define-key eshell-mode-map (kbd "M-_") 'eshell-zle-copy-prev-shell-word)
                                        (define-key eshell-mode-map (kbd "M-RET") 'eshell-zle-push-line)
                                        (define-key eshell-mode-map (kbd "M-g") 'eshell-zle-get-line)
                                        (define-key eshell-mode-map (kbd "M-?") 'eshell-zle-which-command)
                                        (define-key eshell-mode-map (kbd "C-r") 'eshell-isearch-backward)
                                        (define-key eshell-mode-map (kbd "<escape><escape>") 'eshell-zle-sudo-command)
                                        (define-key eshell-mode-map (kbd "<C-backspace>") 'eshell-zle-kill-whole-line)
                                        (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                        (define-key eshell-mode-map [return] 'user-send-input)

                                        (ansi-color-for-comint-mode-on)
                                        (set (make-local-variable 'outline-regexp) "\$" )
                                        (add-to-list 'ac-sources `ac-source-eshell-pcomplete)
                                        (auto-complete-mode t)
                                        (outline-minor-mode)
                                        )))
  :bind (("<f1>d" . eshell)
         ("<f1>c" . multi-eshell)
         ("<f1>n" . multi-eshell-switch)
         ("<f1>p" . multi-eshell-go-back)
         ("<f1><f1>l" . eshell-show-output)
         ("<f1><f1>b" . eshell-insert-buffer-name)
         )
  )
