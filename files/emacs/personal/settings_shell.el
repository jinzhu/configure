;; Multi Term
(require 'multi-term)
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

;; Bash Complete
(require 'shell-command)
(shell-command-completion-mode)
(require 'bash-completion)
(bash-completion-setup)



;; Eshell
(rvm-use-default)

(require 'eshell)
(require 'em-smart)
(require 'em-zle)

(setq
 multi-eshell-name "*eshell*"
 multi-eshell-shell-function '(eshell)
 tramp-chunksize 500
 eshell-last-dir-ring-size 100

 tramp-default-method "ssh"          ; uses ControlMaster
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
 eshell-prompt-regexp "^[^#$\n]*[#$>] "

 eshell-aliases-file "~/.emacs.d/personal/alias"
 eshell-visual-commands (quote ("vim" "tail" "ssh" "less"))
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

                              (ansi-color-for-comint-mode-on)
                              (set (make-local-variable 'outline-regexp) "\$" )
                              (add-to-list 'ac-sources `ac-source-eshell-pcomplete)
                              (auto-complete-mode t)
                              (outline-minor-mode)
                              ))

(global-unset-key (kbd "<f3>"))
(global-set-key (kbd "<f3>d") 'eshell)
(global-set-key (kbd "<f3>c") 'multi-eshell)
(global-set-key (kbd "<f3>n") 'multi-eshell-switch)
(global-set-key (kbd "<f3>p") 'multi-eshell-go-back)
(global-set-key (kbd "<f3>l") 'eshell-show-output)
(global-set-key (kbd "<f3>b") 'eshell-insert-buffer-name)


(global-set-key (kbd "<f3><f3>d") 'multi-term-dedicated-toggle)
(global-set-key (kbd "<f3><f3>c") 'multi-term)
(global-set-key (kbd "<f3><f3>n") 'multi-term-next)
(global-set-key (kbd "<f3><f3>p") 'multi-term-prev)

;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(setq
 ac-auto-show-menu 0.5
 ac-fuzzy-enable t
 ac-use-menu-map t

 ac-quick-help-prefer-pos-tip t
 ac-use-quick-help t
 ac-quick-help-delay 1.5

 ac-auto-start 1
 ac-menu-height 20
 ac-ignore-case 'smart
 ac-trigger-commands (cons 'backward-delete-char-untabify ac-trigger-commands)
 ac-comphist-file "~/.emacs.d/auto-complete"
 )

(defun ac-eshell-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-eshell-pcomplete
  '((candidates . ac-eshell-pcomplete)))

(setq ac-sources '(ac-source-semantic ac-source-yasnippet ac-source-words-in-buffer ac-source-abbrev
                                      ac-source-dictionary ac-source-files-in-current-dir ac-source-filename))

;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; GoLang
(require 'go-autocomplete)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-minor-mode-on)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
