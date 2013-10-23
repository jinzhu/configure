;; Go Autocomplete
(require-package 'go-autocomplete)
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))

;; Auto Complete
(require-package 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(setq
 ac-auto-show-menu 0.2
 ac-fuzzy-enable t
 ac-use-menu-map t

 ac-quick-help-prefer-pos-tip t
 ac-use-quick-help t
 ac-quick-help-delay 0.5

 ac-auto-start 1
 ac-menu-height 20
 ac-ignore-case 'smart
 ac-trigger-commands (cons 'backward-delete-char-untabify ac-trigger-commands)
 ;; ac-comphist-file "~/.emacs.d/auto-complete"
 ac-sources '(ac-source-imenu
	      ac-source-dictionary
	      ac-source-words-in-buffer
	      ac-source-words-in-same-mode-buffers
	      ac-source-words-in-all-buffer
	      ac-source-dictionary
	      ac-source-files-in-current-dir
	      ac-source-filename)
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

;; Default settings
(bind-key "C-n" 'ac-next ac-menu-map)
(bind-key "C-p" 'ac-previous ac-menu-map)

;; Yasnippet
(require-package 'yasnippet)
(yas-global-mode 1)
(yas-minor-mode-on)

;; Hippie Exp
(require-package 'hippie-exp)
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

(bind-key "M-/" 'hippie-expand)
